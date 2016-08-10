`timescale 1ns/100ps
/*
 * Module      : verilog/correlator_SDP.v
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

module correlator_SDP
  #(parameter ACCUM = `ACCUM_BITS, // Re/Im accumulator bit-widths
    parameter SUMHI = 0,       // is "ones-counting" needed?
    parameter MSB   = ACCUM-1,
    parameter WIDTH = ACCUM+ACCUM, // Combined Re & Im components
    parameter WSB   = WIDTH-1,
    // Pairs of antennas to correlate:
    parameter PAIRS = 120'hb1a191817161b0a090807060,
    parameter TRATE = 12,       // Time-multiplexing rate
    parameter TBITS = 4,
    parameter TSB   = TBITS - 1,
    parameter DELAY = 3)
   (
    input          clk_x, // correlator clock
    input          rst,

    // Real and imaginary components from the antennas.
    input          sw, // switch banks
    input          en, // data is valid
    input [23:0]   re,
    input [23:0]   im,
    input [TSB:0]  rd,
    input [TSB:0]  wr,

    output         vld,
    output [WSB:0] vis,

    output reg     overflow_cos = 0,
    output reg     overflow_sin = 0
    );


   //-------------------------------------------------------------------------
   //  Xilinx Distributed & Block RAM's for the accumulators & visibilities.
   //-------------------------------------------------------------------------
   // For Xilinx FPGA's, this should be two `RAM32M's, and operating in SDP
   // mode.
   wire [MSB:0]        dcos_w, dsin_w, qcos, qsin;
   reg [MSB:0]         dcos, dsin;
   wire                oc, os;

   assign vis = {qsin, qcos};


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

// `define __licarus
// `ifdef  __licarus
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
   wire [4:0]   a_index, b_index;
   reg          go = 0, ar, br, bi, hi;
   wire         sum = SUMHI && rd[3:1] == 3'b101;

   assign {b_index, a_index} = pairs[rd];

   //  Add a cycle of latency to wait for the RAM read.
   always @(posedge clk_x) begin
      go <= #DELAY en;
      hi <= #DELAY sum;
      ar <= #DELAY re[a_index];
      br <= #DELAY re[b_index];
      bi <= #DELAY im[b_index];
   end

   always @(posedge clk_x)
     if (en) begin : RAM_READ
        dcos <= #DELAY sw ? 0 : dcos_w;
        dsin <= #DELAY sw ? 0 : dsin_w;
     end


   //-------------------------------------------------------------------------
   //  Time-multiplexed correlator.
   //-------------------------------------------------------------------------
   correlate_cos_sin
     #(  .ACCUM(ACCUM), .DELAY(DELAY) ) CORR_COS_SIN0
       ( .clk(clk_x),
         .rst(rst),

         // Antenna enables and inputs:
         .en(go),
         .hi(hi),
         .ar(ar),
         .br(br),
         .bi(bi),

         // Accumulator inputs and outputs:
         .dcos(dcos),
         .dsin(dsin),
         .valid(vld),
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
        ) RAM32X6_SDP_COS [3:0]
       (.WCLK(clk_x),
        .WE(vld),
        .WADDR({{5-TBITS{1'b0}}, wr}),
        .DI(qcos),
        .RADDR({{5-TBITS{1'b0}}, rd}),
        .DO(dcos_w),
        .DID(2'b0),
        .DOD()
        );

   RAM32X6_SDP
     #( .INITA(64'h0),
        .INITB(64'h0),
        .INITC(64'h0),
        .INITD(64'h0),
        .DELAY(3)
        ) RAM32X6_SDP_SIN [3:0]
       (.WCLK(clk_x),
        .WE(vld),
        .WADDR({{5-TBITS{1'b0}}, wr}),
        .DI(qsin),
        .RADDR({{5-TBITS{1'b0}}, rd}),
        .DO(dsin_w),
        .DID(2'b0),
        .DOD()
        );


endmodule // correlator_SDP
