`timescale 1ns/100ps
/*
 * Module      : verilog/correlator/top_DSP.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
 * 
 * Time-multiplexed block of correlator-blocks, and this version uses the
 * Xilinx DSP48A1 primitives (of Spartan 6's).
 * 
 * NOTE:
 *  + typically several of these would be attached to a common set of antenna
 *    and a system bus;
 *  + the correlators based upon the DSP48A1 primitive require TICKS == 4,
 *    whereas the SDP version requires TICKS == 3;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as multi-
 *    port SRAM's are used;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM resources;
 * 
 * Changelog:
 *  + 28/10/2016  --  initial file (rebuilt from `correlator_block_DSP.v`);
 * 
 * TODO:
 * 
 */

module top_DSP
  #(//  Data and visibilities parameters:
    parameter ACCUM = 24,      // Accumulator bit-widths
    parameter MSB   = ACCUM-1,
    parameter IBITS = 24,      // Number of data sources
    parameter ISB   = IBITS-1,

    //  Pairs of antennas to correlate, for each block:
    parameter PAIRS0 = 120'h0,
    parameter PAIRS1 = 120'h0,
    parameter PAIRS2 = 120'h0,
    parameter PAIRS3 = 120'h0,

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
    parameter NSRAM = ACCUM>>2, // #<block SRAM> for read-back
    parameter TICKS = 4,        // RMW cycle latency (3/4)
    parameter SLOW  = 0,        // Slow clocks (0/1)?
    parameter DUPS  = 0,        // Duplicate high-fanout registers (0/1)?

    //  SRAM address bits:
    parameter ABITS = TBITS+XBITS, // External I/O address bits
    parameter ASB   = ABITS-1,

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_x, // correlator clock
    input          clk_i, // SRAM/bus clock
    input          rst_i,

    // Real and imaginary components from the antennas:
    input          sw_i, // switch banks
    input          en_i, // data is valid
    input [ISB:0]  re_i, // real component of imput
    input [ISB:0]  im_i, // imaginary component of input

    //  SRAM interface for reading back the visibilities:
    input          sram_ce_i,
    input [ASB:0]  sram_adr_i,
    output [QSB:0] sram_dat_o
//     output [MSB:0] sram_dat_o   // NOTE: 2 cycles of read latency
    );


   wire [TSB:0]    x_rd_adr, x_wr_adr, y_rd_adr, y_wr_adr;
   wire            sw, en;
   wire [XSB:0]    bank;
   wire [ISB:0]    re, im;


   //-------------------------------------------------------------------------
   //  Block of four DSP48A2-based correlators.
   //-------------------------------------------------------------------------
   control
     #( .IBITS(IBITS),
        .TICKS(TICKS),
        .SLOW (SLOW),
        .DUPS (DUPS),
        .TRATE(TRATE),
        .TBITS(TBITS),
        .DELAY(DELAY)
        ) CTRL
       (.clk_x     (clk_x), // correlator clock
        .rst       (rst_i),
        .sw_i      (sw_i), // switch banks
        .en_i      (en_i), // data is valid
        .re_i      (re_i), // real component of input
        .im_i      (im_i), // imaginary component
        .x_rd_adr_o(x_rd_adr),
        .x_wr_adr_o(x_wr_adr),
        .y_rd_adr_o(y_rd_adr),
        .y_wr_adr_o(y_wr_adr),
        .swap_o    (sw), // bank-swap in progress
        .en_o      (en),
        .bank_o    (bank), // active visibilities bank
        .real_o    (re),
        .imag_o    (im)
        );


   //-------------------------------------------------------------------------
   //  Block of four DSP48A2-based correlators.
   //-------------------------------------------------------------------------
   block_DSP
     #( .ACCUM(ACCUM),          // Accumulator bit-widths
        .IBITS(IBITS),          // Number of data sources
        .PAIRS0(PAIRS0),
        .PAIRS1(PAIRS1),
        .PAIRS2(PAIRS2),
        .PAIRS3(PAIRS3),
        .WIDTH(WIDTH),         // Combined Re & Im components
        .QBITS(QBITS),         // Total data-bus bit-width of SRAM
        .TRATE(TRATE),         // Time-multiplexing rate
        .TBITS(TBITS),         // Required bit-width for TRATE counter
        .XBITS(XBITS),         // Bank address address bit-width
        .NSRAM(NSRAM),         // #<block SRAM> for read-back
        .ABITS(ABITS),         // External I/O address bits
        .DELAY(DELAY)
        ) BDSP
       (
        .clk_x(clk_x),

        .x_rd_adr_i(x_rd_adr),
        .x_wr_adr_i(x_wr_adr),
        .y_rd_adr_i(y_rd_adr),
        .y_wr_adr_i(y_wr_adr),

        .sw_i(sw), // switch banks
        .en_i(en), // data is valid
        .re_i(re), // real component of imput
        .im_i(im), // imaginary component of input

        .sram_clk_i(clk_i),
        .sram_ce_i (sram_ce_i),
        .sram_adr_i(sram_adr_i),
        .sram_dat_o(sram_dat_o)
        );


endmodule // top_DSP
