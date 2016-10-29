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
 * Time-multiplexed block of blocks of correlator-blocks, and using a mix of
 * Xilinx DSP48A1 primitives (of Spartan 6's) and standard carry-chains
 * adders, for the accumulators.
 * 
 * NOTE:
 *  + the correlators based upon the DSP48A1 primitive require TICKS == 4,
 *    whereas the SDP version requires TICKS == 3;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as the
 *    large number of accumulations (per bank, and correlator) can allow
 *    plenty of time for the visibilities to be read back, using the multi-
 *    port SRAM's;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM, carry-chain,
 *    and DSP48A2 resources;
 * 
 * Changelog:
 *  + 28/10/2016  --  initial file (rebuilt from `tart_correlator.v`);
 * 
 * TODO:
 * 
 */

module top
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
    parameter DSLOW = 0,        // Slow DSP clocks (0/1)?
    parameter SSLOW = 0,        // Slow SDP clocks (0/1)?
    parameter DDUPS = 0,        // Duplicate high-fanout DSP registers (0/1)?
    parameter SDUPS = 0,        // Duplicate high-fanout SDP registers (0/1)?

    //  SRAM address bits:
    parameter ABITS = TBITS+XBITS, // External I/O address bits
    parameter ASB   = ABITS-1,

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

    //  Wishbone-like bus interface for reading visibilities.
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

    //  Data from the antennae:
    input          en_i, // data is valid
    input [ISB:0]  ax_i  // antenna data input
    );


   //  SRAM control and data signals.
   wire            sram_ce;
   wire [ASB:0]    sram_ad;
   wire [QSB:0]    sram_d0, sram_d1, sram_d2, sram_d3, sram_d4, sram_d5;


   //-------------------------------------------------------------------------
   //  Map Wishbone signals to the outputs.
   //-------------------------------------------------------------------------
   assign wat_o  = 1'b0;
   assign rty_o  = 1'b0;
   assign err_o  = 1'b0;


   //-------------------------------------------------------------------------
   //  Hilbert transform to recover imaginaries.
   //-------------------------------------------------------------------------
   fake_hilbert #( .WIDTH(AXNUM) ) HILB0
     (  .clk(clk_x),
        .rst(rst_i),
        .en(en_i),
        .d(ax_i),
        .valid(go),
        .strobe(strobe), // `antenna` data is valid
        .frame(frame),   // last cycle for `antenna` data to be valid
        .re(re),
        .im(im)
        );


   //-------------------------------------------------------------------------
   //  TART bank-switching unit.
   //-------------------------------------------------------------------------
   tart_bank_switch #( .COUNT(BLOCK), .TICKS(4) ) SW0
     ( .clk_x(clk_x),
       .clk_i(clk_i),
       .rst_i(rst_i),
       .ce_i (go),
       .frame_i(frame),
       .bcount_i(blocksize),
       .swap_x(swap_x),
       .swap_o(switch)
       );


   //-------------------------------------------------------------------------
   //  Six correlator-block instances.
   //-------------------------------------------------------------------------
   top_DSP
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
        .TICKS(TICKS),         // RMW cycle latency (3/4)
        .SLOW (DSLOW),         // Slow clocks (0/1)?
        .DUPS (DDUPS),         // Duplicate high-fanout regs (0/1)?
        .ABITS(ABITS),         // External I/O address bits
        .DELAY(DELAY)
        ) DSP0
       (
        .clk_x(clk_x),
        .clk_i(clk_i), // SRAM/bus clock
        .rst_i(rst_i),

        // Real and imaginary components from the antennas:
        .sw_i(sw), // switch banks
        .en_i(go), // data is valid
        .re_i(re), // real component of imput
        .im_i(im), // imaginary component of input

        //  SRAM interface for reading back the visibilities:
        .sram_ce_i (sram_ce),
        .sram_adr_i(sram_ad),
        .sram_dat_o(sram_d0)
        );


   //-------------------------------------------------------------------------
   //  Drive a SRAM, and using a Wishbone (SPEC B4) interface.
   //-------------------------------------------------------------------------
   wb_sram_interface
     #( .WIDTH(QBITS),          // SRAM features & bit-widths
        .ABITS(ABITS),
        .TICKS(3),              // SRAM read, then two levels of MUX's
        .USEBE(0),
        .PIPED(PIPED),          // Wishbone mode settings
        .ASYNC(ASYNC),
        .CHECK(CHECK)
        ) SCTRL
       (
        .clk_i(clk_i),
        .cyc_i(cyc_i),
        .stb_i(stb_i),
        .we_i (1'b0 ),
        .ack_o(ack_o),
        .wat_o(),
        .rty_o(),
        .err_o(),
        .adr_i(adr_i),
        .sel_i(sel_i),
        .dat_i(dat_i),
        .dat_o(dat_o),

        .sram_ce_o (sram_ce_o),
        .sram_we_o (),
        .sram_adr_o(sram_adr_o),
        .sram_bes_o(),
        .sram_dat_i(sram_dat_i),
        .sram_dat_o()
        );


endmodule // top
