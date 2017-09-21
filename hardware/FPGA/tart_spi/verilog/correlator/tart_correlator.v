`timescale 1ns/100ps
/*
 * Module      : verilog/tart_correlator.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * 
 * This file is part of TART.
 * 
 * TART is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * TART is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser Public License along with
 * TART.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * 
 * Description:
 * Top-level of the TART, time-multiplexed block of correlator-blocks.
 * 
 * NOTE:
 *  + hardwired for a 24 antenna setup;
 *  + the correlators are set to use 12:1 time-multiplexing, so `clk_x` must
 *    be 12x the frequency of the sampling clock;
 *  + the upper 3-bits of the address determine either:
 *     a) one of the six correlator blocks;
 *     b) the ones-counter unit; or
 *     c) the bank of system registers.
 *  + the number of bits in a block's counter corresponds to the maximum size
 *    of the accumulator, because the visibilities are monotone increasing;
 * 
 * TODO:
 *  + when `bst_i` deasserts, deassert `stb[i]` the next cycle? Currently, the
 *    "tails" of a transaction are one cycle too long;
 *  + compute the exponent of the count-size;
 *  + status registers, and correctly handle overflows;
 *  + upgrade to Wishbone SPEC B4;
 * 
 */

`include "tartcfg.v"

module tart_correlator
  #(//  Data and visibilities parameters:
    parameter ACCUM = 24,      // Accumulator bit-widths
    parameter MSB   = ACCUM-1,
    parameter BYTES = ACCUM>>3,
    parameter IBITS = 24,      // Number of data sources
    parameter ISB   = IBITS-1,

    //  Various additional data bit-widths:
    parameter WIDTH = ACCUM*2, // Combined Re & Im components
    parameter WSB   = WIDTH-1,
    parameter QBITS = WIDTH*4, // Total data-bus bit-width of SRAM(s)
    parameter QSB   = QBITS-1,

    //  Time-multiplexing parameters:
    parameter TRATE = 12,      // Time-multiplexing rate
    parameter TBITS = 4,       // Required bit-width for TRATE counter
    parameter TSB   = TBITS-1, // MSB of time-multiplexed registers

    //  Visibilities banks parameters:
    parameter XBITS = 4,       // Bank address address bit-width
    parameter XSB   = XBITS-1, // MSB of bank-address

    //  Correlator parameters:
    parameter ABITS = 14,       // External I/O address bits
    parameter ASB   = ABITS-1,  // MSB of address
    parameter NSRAM = ACCUM>>2, // #<block SRAM> for read-back
    parameter SEQRD = 1,        // Sequential correlator accesses (0/1)?

    //  Correlator optimisation settings:
    parameter DSLOW = 0,        // Slow DSP clocks (0/1)?
    parameter SSLOW = 0,        // Slow SDP clocks (0/1)?
    parameter DDUPS = 0,        // Duplicate high-fanout DSP registers (0/1)?
    parameter SDUPS = 0,        // Duplicate high-fanout SDP registers (0/1)?

    //  SRAM address bits:
    parameter KBITS = TBITS+XBITS, // External I/O address bits
    parameter KSB   = KBITS-1,
    parameter JBITS = KBITS+3,
    parameter JSB   = JBITS-1,

    // Wishbone modes/parameters:
    parameter ASYNC = 1,
    parameter PIPED = 1,
    parameter CHECK = 1,

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_x, // correlator clock
    input          clk_i, // SRAM/bus clock
    input          rst_i,

    //  Wishbone (SPEC B4) interface for reading visibilities:
    input          cyc_i,
    input          stb_i,
    input          we_i, // writes are ignored
    output         ack_o,
    output         wat_o,
    output         rty_o,
    output         err_o,
    input [ASB:0]  adr_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    //  Bus clock-domain source & framing signals:
    input          vld_i,       // enables the correlators when valid
    input          new_i,       // new sample next clock-cycle
    input [ISB:0]  sig_i,       // oversampled source signal

    //  Bus clock-domain status & control signals:
    output         switching_o,

    //  Correlator clock-domain status & control signals:
    input [MSB:0]  sums_x_i, // number of products to accumulate
    output         swap_x_o, // bank-switch strobe
    output [XSB:0] bank_x_o
    );


   //-------------------------------------------------------------------------
   //  SRAM control and data signals.
   wire            sram_ce;
   wire [JSB:0]    sram_ad;
   wire [MSB:0]    sram_d0, sram_d1, sram_d2, sram_d3, sram_d4, sram_d5;
   wire [MSB:0]    sram_do;
   reg [MSB:0]     sram_out;

   //-------------------------------------------------------------------------
   //  Wishbone control and data signals.
   reg [2:0]       sel0, sel1;
   wire [2:0]      sel_w, sel;
   wire [JSB:0]    adr_w;
   wire            stb_w;

   //-------------------------------------------------------------------------
   //  Antenna data, Hilbert-transformed, bank selection, and switching
   //  signals.
   reg          pre_x, strobe_x, go_x;
   reg [ISB:0]  re_x, im_x;
   wire         sw_x, go_b, strobe_b;
   wire [ISB:0] re_b, im_b;


   //-------------------------------------------------------------------------
   //  Internal bus-signal assignments.
   //-------------------------------------------------------------------------
   //  Ignore the CYC signal if using a point-to-point interconnect, else
   //  be a bit more careful.
   assign stb_w = CHECK ? cyc_i && stb_i : stb_i;

   //  Address:     BANK ADDR      TMUX IDX    CORR#       COS/SIN
   assign adr_w = {adr_i[ASB:10], adr_i[4:1], adr_i[6:5], adr_i[0]};

   //  Block:      BLOCK#
   assign sel_w = adr_i[9:7];

   //  If the bus only accesses a single correlator per transaction, then use
   //  the simpler circuitry.
   assign sel   = SEQRD ? sel_w : sel1;

   assign swap_x_o = sw_x;


   //-------------------------------------------------------------------------
   //  Select and capture data from the correlators, during read-back.
   //-------------------------------------------------------------------------
   //  Select the correlator to capture data from.
   //  NOTE: The prefetcher holds the `sel` bits constant for an entire
   //    correlator read, so not needed for the standard TART configuration;
   //    therefore, use `SEQRD == 1`, unless doing something weird.
   always @(posedge clk_i)
     if (stb_w)
       {sel1, sel0} <= #DELAY {sel0, sel_w};

   //  Capture data from the SRAM MUX to an output register.
   always @(posedge clk_i)
     sram_out <= #DELAY sram_do;


   //-------------------------------------------------------------------------
   //  Lift source-signals into the 12x domain.
   //-------------------------------------------------------------------------
   always @(posedge clk_x)
     begin
        pre_x    <= #DELAY strobe_b && !pre_x && !strobe_x;
        strobe_x <= #DELAY pre_x;
        go_x     <= #DELAY go_b;
        re_x     <= #DELAY re_b;
        im_x     <= #DELAY im_b;
     end


   //-------------------------------------------------------------------------
   //  Local synchronous reset for the correlator-blocks.
   //  NOTE: Needs to be synchronised from the bus-clock domain.
   //-------------------------------------------------------------------------
   (* NOMERGE *) reg rst_x = 1'b1, rst_s = 1'b1;

   always @(posedge clk_x)
     {rst_x, rst_s} <= #DELAY {rst_s, rst_i};


   //-------------------------------------------------------------------------
   //  Hilbert transform to recover imaginaries.
   //-------------------------------------------------------------------------
   (* AREA_GROUP = "hilb" *)
   fake_hilbert
     #(  .WIDTH(IBITS)
         ) HILB0
       ( .clock_i (clk_i),
         .reset_i (rst_i),

         .enable_i(vld_i),      // input source & framing signals
         .strobe_i(new_i),
         .signal_i(sig_i),

         .locked_o(go_b),
         .strobe_o(strobe_b),    // new `antenna` sample next cycle
         .framed_o(),            // last cycle of sample
         .sig_re_o(re_b),
         .sig_im_o(im_b)
         );


   //-------------------------------------------------------------------------
   //  TART bank-switching unit.
   //-------------------------------------------------------------------------
   //  Local synchronous reset for the visibilities read-back core.
   (* NOMERGE *) reg rst_b = 1'b1;

   always @(posedge clk_x)
     rst_b <= #DELAY rst_x;

   //-------------------------------------------------------------------------
   bank_switch
     #(  .COUNT(ACCUM),
         .TICKS(4)
         ) SW0
       ( .clk_x   (clk_x),
         .rst_x   (rst_b),
         .clk_i   (clk_i),
         .rst_i   (rst_i),
         .ce_i    (go_x),
         .frame_i (strobe_x),
         .bcount_i(sums_x_i),
         .swap_x  (sw_x),
         .swap_o  (switching_o)
         );


   //-------------------------------------------------------------------------
   //  Six correlator-block instances.
   //-------------------------------------------------------------------------
   //  The configuration file includes the parameters that determine which
   //  antannae connect to each of the correlators.
`include "../include/tart_pairs.v"

   (* AREA_GROUP = "cblk0" *)
   correlator_block
     #( .PAIRS0(PAIRS00_00),
        .PAIRS1(PAIRS00_01),
        .PAIRS2(PAIRS00_02),
        .PAIRS3(PAIRS00_03),

        .ACCUM(ACCUM),         // Accumulator bit-widths
        .IBITS(IBITS),         // Number of data sources
        .WIDTH(WIDTH),         // Combined Re & Im components
        .QBITS(QBITS),         // Total data-bus bit-width of SRAM
        .TRATE(TRATE),         // Time-multiplexing rate
        .TBITS(TBITS),         // Required bit-width for TRATE counter
        .XBITS(XBITS),         // Bank address address bit-width
        .NSRAM(NSRAM),         // #<block SRAM> for read-back
        .CARRY(0),             // DSP48A2-based correlator
        .TICKS(4),             // RMW cycle latency (3/4)
        .SLOW (DSLOW),         // Slow clocks (0/1)?
        .DUPS (DDUPS),         // Duplicate high-fanout regs (0/1)?
        .ABITS(KBITS),         // External I/O address bits
        .DELAY(DELAY)
        ) CXB0
       (
        .clk_x(clk_x),
        .rst_x(rst_x),
        .clk_i(clk_i), // SRAM/bus clock

        //  Real and imaginary components from the antennas:
        .sw_i(sw_x), // switch banks
        .en_i(go_x), // data is valid
        .re_i(re_x), // real component of imput
        .im_i(im_x), // imaginary component of input

        .bank_o(bank_x_o),

        //  SRAM interface for reading back the visibilities:
        .sram_ce_i(sram_ce),
        .sram_ad_i(sram_ad),
        .sram_do_o(sram_d0)
        );

   (* AREA_GROUP = "cblk1" *)
   correlator_block
     #( .PAIRS0(PAIRS01_00),
        .PAIRS1(PAIRS01_01),
        .PAIRS2(PAIRS01_02),
        .PAIRS3(PAIRS01_03),

        .ACCUM(ACCUM),         // Accumulator bit-widths
        .IBITS(IBITS),         // Number of data sources
        .WIDTH(WIDTH),         // Combined Re & Im components
        .QBITS(QBITS),         // Total data-bus bit-width of SRAM
        .TRATE(TRATE),         // Time-multiplexing rate
        .TBITS(TBITS),         // Required bit-width for TRATE counter
        .XBITS(XBITS),         // Bank address address bit-width
        .NSRAM(NSRAM),         // #<block SRAM> for read-back
        .CARRY(0),             // DSP48A2-based correlator
        .TICKS(4),             // RMW cycle latency (3/4)
        .SLOW (DSLOW),         // Slow clocks (0/1)?
        .DUPS (DDUPS),         // Duplicate high-fanout regs (0/1)?
        .ABITS(KBITS),         // External I/O address bits
        .DELAY(DELAY)
        ) CXB1
       (
        .clk_x(clk_x),
        .rst_x(rst_x),
        .clk_i(clk_i), // SRAM/bus clock

        //  Real and imaginary components from the antennas:
        .sw_i(sw_x), // switch banks
        .en_i(go_x), // data is valid
        .re_i(re_x), // real component of imput
        .im_i(im_x), // imaginary component of input

        .bank_o(),

        //  SRAM interface for reading back the visibilities:
        .sram_ce_i(sram_ce),
        .sram_ad_i(sram_ad),
        .sram_do_o(sram_d1)
        );

   (* AREA_GROUP = "cblk2" *)
   correlator_block
     #( .PAIRS0(PAIRS02_00),
        .PAIRS1(PAIRS02_01),
        .PAIRS2(PAIRS02_02),
        .PAIRS3(PAIRS02_03),

        .ACCUM(ACCUM),         // Accumulator bit-widths
        .IBITS(IBITS),         // Number of data sources
        .WIDTH(WIDTH),         // Combined Re & Im components
        .QBITS(QBITS),         // Total data-bus bit-width of SRAM
        .TRATE(TRATE),         // Time-multiplexing rate
        .TBITS(TBITS),         // Required bit-width for TRATE counter
        .XBITS(XBITS),         // Bank address address bit-width
        .NSRAM(NSRAM),         // #<block SRAM> for read-back
        .CARRY(0),             // DSP48A2-based correlator
        .TICKS(4),             // RMW cycle latency (3/4)
        .SLOW (DSLOW),         // Slow clocks (0/1)?
        .DUPS (DDUPS),         // Duplicate high-fanout regs (0/1)?
        .ABITS(KBITS),         // External I/O address bits
        .DELAY(DELAY)
        ) CXB2
       (
        .clk_x(clk_x),
        .rst_x(rst_x),
        .clk_i(clk_i), // SRAM/bus clock

        //  Real and imaginary components from the antennas:
        .sw_i(sw_x), // switch banks
        .en_i(go_x), // data is valid
        .re_i(re_x), // real component of imput
        .im_i(im_x), // imaginary component of input

        .bank_o(),

        //  SRAM interface for reading back the visibilities:
        .sram_ce_i(sram_ce),
        .sram_ad_i(sram_ad),
        .sram_do_o(sram_d2)
        );

   (* AREA_GROUP = "cblk3" *)
   correlator_block
     #( .PAIRS0(PAIRS03_00),
        .PAIRS1(PAIRS03_01),
        .PAIRS2(PAIRS03_02),
        .PAIRS3(PAIRS03_03),

        .ACCUM(ACCUM),         // Accumulator bit-widths
        .IBITS(IBITS),         // Number of data sources
        .WIDTH(WIDTH),         // Combined Re & Im components
        .QBITS(QBITS),         // Total data-bus bit-width of SRAM
        .TRATE(TRATE),         // Time-multiplexing rate
        .TBITS(TBITS),         // Required bit-width for TRATE counter
        .XBITS(XBITS),         // Bank address address bit-width
        .NSRAM(NSRAM),         // #<block SRAM> for read-back
        .CARRY(0),             // DSP48A2-based correlator
        .TICKS(4),             // RMW cycle latency (3/4)
        .SLOW (DSLOW),         // Slow clocks (0/1)?
        .DUPS (DDUPS),         // Duplicate high-fanout regs (0/1)?
        .ABITS(KBITS),         // External I/O address bits
        .DELAY(DELAY)
        ) CXB3
       (
        .clk_x(clk_x),
        .rst_x(rst_x),
        .clk_i(clk_i), // SRAM/bus clock

        //  Real and imaginary components from the antennas:
        .sw_i(sw_x), // switch banks
        .en_i(go_x), // data is valid
        .re_i(re_x), // real component of imput
        .im_i(im_x), // imaginary component of input

        .bank_o(),

        //  SRAM interface for reading back the visibilities:
        .sram_ce_i(sram_ce),
        .sram_ad_i(sram_ad),
        .sram_do_o(sram_d3)
        );


   //-------------------------------------------------------------------------
   //  Instantiate two carry-chain-based correlator-blocks.
   //-------------------------------------------------------------------------
   (* AREA_GROUP = "cblk4" *)
   correlator_block
     #( .PAIRS0(PAIRS04_00),
        .PAIRS1(PAIRS04_01),
        .PAIRS2(PAIRS04_02),
        .PAIRS3(PAIRS04_03),

        .ACCUM(ACCUM),         // Accumulator bit-widths
        .IBITS(IBITS),         // Number of data sources
        .WIDTH(WIDTH),         // Combined Re & Im components
        .QBITS(QBITS),         // Total data-bus bit-width of SRAM
        .TRATE(TRATE),         // Time-multiplexing rate
        .TBITS(TBITS),         // Required bit-width for TRATE counter
        .XBITS(XBITS),         // Bank address address bit-width
        .NSRAM(NSRAM),         // #<block SRAM> for read-back
        .CARRY(1),             // carry-chain-based correlator
        .TICKS(3),             // RMW cycle latency (3/4)
        .SLOW (SSLOW),         // Slow clocks (0/1)?
        .DUPS (SDUPS),         // Duplicate high-fanout regs (0/1)?
        .ABITS(KBITS),         // External I/O address bits
        .DELAY(DELAY)
        ) CXB4
       (
        .clk_x(clk_x),
        .rst_x(rst_x),
        .clk_i(clk_i), // SRAM/bus clock

        //  Real and imaginary components from the antennas:
        .sw_i(sw_x), // switch banks
        .en_i(go_x), // data is valid
        .re_i(re_x), // real component of imput
        .im_i(im_x), // imaginary component of input

        .bank_o(),

        //  SRAM interface for reading back the visibilities:
        .sram_ce_i(sram_ce),
        .sram_ad_i(sram_ad),
        .sram_do_o(sram_d4)
        );

   (* AREA_GROUP = "cblk5" *)
   correlator_block
     #( .PAIRS0(PAIRS05_00),
        .PAIRS1(PAIRS05_01),
        .PAIRS2(PAIRS05_02),
        .PAIRS3(PAIRS05_03),

        .ACCUM(ACCUM),         // Accumulator bit-widths
        .IBITS(IBITS),         // Number of data sources
        .WIDTH(WIDTH),         // Combined Re & Im components
        .QBITS(QBITS),         // Total data-bus bit-width of SRAM
        .TRATE(TRATE),         // Time-multiplexing rate
        .TBITS(TBITS),         // Required bit-width for TRATE counter
        .XBITS(XBITS),         // Bank address address bit-width
        .NSRAM(NSRAM),         // #<block SRAM> for read-back
        .CARRY(1),             // carry-chain-based correlator
        .TICKS(3),             // RMW cycle latency (3/4)
        .SLOW (SSLOW),         // Slow clocks (0/1)?
        .DUPS (SDUPS),         // Duplicate high-fanout regs (0/1)?
        .ABITS(KBITS),         // External I/O address bits
        .DELAY(DELAY)
        ) CXB5
       (
        .clk_x(clk_x),
        .rst_x(rst_x),
        .clk_i(clk_i), // SRAM/bus clock

        //  Real and imaginary components from the antennas:
        .sw_i(sw_x), // switch banks
        .en_i(go_x), // data is valid
        .re_i(re_x), // real component of imput
        .im_i(im_x), // imaginary component of input

        .bank_o(),

        //  SRAM interface for reading back the visibilities:
        .sram_ce_i(sram_ce),
        .sram_ad_i(sram_ad),
        .sram_do_o(sram_d5)
        );


   //-------------------------------------------------------------------------
   //  Explicitly instantiate an 8:1 MUX for the output-data, so that it can
   //  be floor-planned.
   //-------------------------------------------------------------------------
   MUX8 #( .WIDTH(ACCUM) ) DMUX
     ( .a(sram_d0),
       .b(sram_d1),
       .c(sram_d2),
       .d(sram_d3),
       .e(sram_d4),
       .f(sram_d5),
       .g({ACCUM{1'bx}}),       // only six correlators
       .h({ACCUM{1'bx}}),
       .s(sel),
       .x(sram_do)
       );


   //-------------------------------------------------------------------------
   //  Drive a SRAM, and using a Wishbone (SPEC B4) interface.
   //-------------------------------------------------------------------------
   wb_sram_port
     #( .WIDTH(ACCUM),          // SRAM features & bit-widths
        .ABITS(JBITS),
        .TICKS(3),              // SRAM read, then two levels of MUX's
        .READ (1),              // read visibilities back
        .WRITE(0),              // read-only
        .USEBE(0),
        .PIPED(PIPED),          // Wishbone mode settings
        .ASYNC(0),              // synchronous ACK
        .CHECK(CHECK),          // if part of a bus then use checking
        .DELAY(DELAY)
        ) SCTRL
       (
        .clk_i(clk_i),
        .rst_i(rst_i),

        .cyc_i(cyc_i),
        .stb_i(stb_i),
        .we_i (1'b0 ),
        .ack_o(ack_o),
        .wat_o(wat_o),
        .rty_o(rty_o),
        .err_o(err_o),
        .adr_i(adr_w),          // uses the re-ordered address
        .sel_i({BYTES{1'b1}}),
        .dat_i({ACCUM{1'bx}}),  // dat_i),
        .dat_o(dat_o),

        .sram_ce_o(sram_ce),
        .sram_we_o(),
        .sram_ad_o(sram_ad),
        .sram_be_o(),
        .sram_do_i(sram_out),
        .sram_di_o()
        );


endmodule // tart_correlator
