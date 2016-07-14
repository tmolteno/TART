`timescale 1ns/100ps
/*
 * Module      : verilog/tart_aquire.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Time-multiplexed correlator block.
 * 
 * This version operates the Xilinx SRAM's in Simple Dual-Port mode, to make
 * better use of the available SRAM resources.
 * 
 * NOTE:
 *  + typically several of these would be attached to a common set of antenna
 *    and a system bus;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as multi-
 *    port SRAM's are used;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM resources;
 *  + ignores write-attempts, but still generates acknowledges;
 * 
 * Changelog:
 *  + 04/07/2016  --  initial file (refactored from `correlator`);
 * 
 */

`include "tartcfg.v"

module correlator_DSP
  #(parameter ACCUM = `ACCUM_BITS,            // Re/Im accumulator bit-widths
    parameter MSB   = ACCUM - 1,
    parameter WIDTH = ACCUM + ACCUM, // Combined Re & Im components
    parameter WSB   = WIDTH - 1,
    // Pairs of antennas to correlate:
    parameter PAIRS = 120'hb1a191817161b0a090807060,
    parameter DELAY = 3)
   (
    input          clk_x, // correlator clock
    input          rst,

    // Real and imaginary components from the antennas.
    input          sw, // switch banks
    input          en, // data is valid
    input [23:0]   re,
    input [23:0]   im,
    input [4:0]    rd,
    input [4:0]    wr,

    output reg     vld = 0,
    output [WSB:0] vis,

    output reg     overflow_cos = 0,
    output reg     overflow_sin = 0
    );


   //-------------------------------------------------------------------------
   //  Xilinx Distributed & Block RAM's for the accumulators & visibilities.
   //-------------------------------------------------------------------------
   // For Xilinx FPGA's, this should be two `RAM32M's, and operating in SDP
   // mode.
   wire [MSB:0]        dcos, dsin, qcos, qsin;

   // Xilinx Block RAM for the buffered, visibilities data.
   wire                oc, os;
   reg                 go = 0;


   //-------------------------------------------------------------------------
   //  Control signals.
   //-------------------------------------------------------------------------
   //  Add a cycle of latency to wait for the RAM read.
   //  Add another cycle of latency, to wait for the DSP addition.
   always @(posedge clk_x)
     {vld, go} <= #DELAY {go, en};


   //-------------------------------------------------------------------------
   //  Select pairs of antenna to correlate.
   //-------------------------------------------------------------------------
   //  TODO: Can more of this be parameterised?
   parameter PAIRS00 = (PAIRS >>   0) & 10'h3ff;
   parameter PAIRS01 = (PAIRS >>  10) & 10'h3ff;
   parameter PAIRS02 = (PAIRS >>  20) & 10'h3ff;
   parameter PAIRS03 = (PAIRS >>  30) & 10'h3ff;
   parameter PAIRS04 = (PAIRS >>  40) & 10'h3ff;
   parameter PAIRS05 = (PAIRS >>  50) & 10'h3ff;
   parameter PAIRS06 = (PAIRS >>  60) & 10'h3ff;
   parameter PAIRS07 = (PAIRS >>  70) & 10'h3ff;
   parameter PAIRS08 = (PAIRS >>  80) & 10'h3ff;
   parameter PAIRS09 = (PAIRS >>  90) & 10'h3ff;
   parameter PAIRS0A = (PAIRS >> 100) & 10'h3ff;
   parameter PAIRS0B = (PAIRS >> 110) & 10'h3ff;

`ifdef __icarus
   // NOTE: Icarus Verilog doesn't seem to support curly-braces for setting
   //   the wire values;
   wire [9:0]   pairs[0:11];

   assign pairs[00] = PAIRS00;
   assign pairs[01] = PAIRS01;
   assign pairs[02] = PAIRS02;
   assign pairs[03] = PAIRS03;
   assign pairs[04] = PAIRS04;
   assign pairs[05] = PAIRS05;
   assign pairs[06] = PAIRS06;
   assign pairs[07] = PAIRS07;
   assign pairs[08] = PAIRS08;
   assign pairs[09] = PAIRS09;
   assign pairs[10] = PAIRS0A;
   assign pairs[11] = PAIRS0B;
`else
   wire [9:0]   pairs[0:11] = {PAIRS00, PAIRS01, PAIRS02, PAIRS03,
                               PAIRS04, PAIRS05, PAIRS06, PAIRS07,
                               PAIRS08, PAIRS09, PAIRS0A, PAIRS0B};
`endif

   wire [9:0]   pairs_index = pairs[x_rd_adr];
   wire [4:0]   a_index = pairs_index[4:0];
   wire [4:0]   b_index = pairs_index[9:5];

   wire         ar = re[a_index];
   wire         br = re[b_index];
   wire         bi = im[b_index];


   //-------------------------------------------------------------------------
   //  Time-multiplexed correlator.
   //-------------------------------------------------------------------------
   correlate_cos_sin_DSP
     #(  .ACCUM(ACCUM), .DELAY(DELAY) ) CORR_COS_SIN0
       ( .clk(clk_x),
         .rst(rst),
         .clr(clear),

         // Antenna enables and inputs:
         .en(en),
         .vld(go),
         .ar(ar),
         .br(br),
         .bi(bi),

         // Accumulator inputs and outputs:
         .dcos(dcos),
         .dsin(dsin),
         .qcos(qcos),
         .qsin(qsin),

         // Overflow flags:
         .oc(oc),
         .os(os)
         );

   
   //-------------------------------------------------------------------------
   //  RAM32M's implemented the nerdy way.
   //-------------------------------------------------------------------------
   //  TODO: Parameterise the accumulator width.
   RAM32X6_SDP
     #( .INITA(64'h0),
        .INITB(64'h0),
        .INITC(64'h0),
        .INITD(64'h0),
        .DELAY(3)
        ) RAM32X6_SDP_COS0 [3:0]
       (.WCLK(clk_x),
        .WE(vld),
        .WADDR(wr),
        .DI(qcos),
        .RADDR(rd),
        .DO(dcos_w)
        );

   RAM32X6_SDP
     #( .INITA(64'h0),
        .INITB(64'h0),
        .INITC(64'h0),
        .INITD(64'h0),
        .DELAY(3)
        ) RAM32X6_SDP_SIN0 [3:0]
       (.WCLK(clk_x),
        .WE(vld),
        .WADDR(wr),
        .DI(qsin),
        .RADDR(rd),
        .DO(dsin_w)
        );


endmodule // correlator_DSP
