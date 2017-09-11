`timescale 1ns/100ps
/*
 * Module      : verilog/correlator_DSP.v
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
    parameter SUMHI = 0,       // is "ones-counting" needed?
    parameter MSB   = ACCUM - 1,
    parameter WIDTH = ACCUM + ACCUM, // Combined Re & Im components
    parameter WSB   = WIDTH - 1,
    // Pairs of antennas to correlate:
    parameter PAIRS = 120'hb1a191817161b0a090807060,
    parameter TRATE = 12,       // Time-multiplexing rate
    parameter TBITS = 4,
    parameter TSB   = TBITS - 1,
    parameter DELAY = 3)
   (
    input          clk_x, // correlator clock

    // Real and imaginary components from the antennas.
    input          sw, // switched banks
    input          en, // data is valid
    input [23:0]   re,
    input [23:0]   im,
    input [TSB:0]  rd,
    input [TSB:0]  wr,

    output reg     vld = 0,
    output [WSB:0] vis
    );


   //-------------------------------------------------------------------------
   //  Xilinx Distributed & Block RAM's for the accumulators & visibilities.
   //-------------------------------------------------------------------------
   // For Xilinx FPGA's, this should be two `RAM32M's, and operating in SDP
   // mode.
   wire [MSB:0]        dcos, dsin, qcos, qsin;
   reg                 go = 1'b0, wt = 1'b0;

   reg [MSB:0]         rcos = {ACCUM{1'b0}};
   reg [MSB:0]         rsin = {ACCUM{1'b0}};
   reg                 rar = 1'b0, rbr = 1'b0, rbi, rhi = 1'b0;

   assign vis = {qsin, qcos};


   //-------------------------------------------------------------------------
   //  Control signals.
   //-------------------------------------------------------------------------
   //  Add a cycle of latency to wait for the RAM read.
   //  Add another cycle of latency, to compute the DSP inputs.
   //  Add yet another cycle of latency, to wait for the DSP addition.
   always @(posedge clk_x)
     {vld, go, wt} <= #DELAY {go, wt, en};

   always @(posedge clk_x) begin
      {rsin, rcos} <= #DELAY {dsin, dcos};
      {rbi, rbr, rar, rhi} <= #DELAY {bi, br, ar, hi};
   end


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

   wire [9:0]   pairs[0:11];
   wire [4:0]   a_index, b_index;
//    wire         hi = SUMHI && rd[3:1] == 3'b101;
   wire         hi = SUMHI && {rd[3], rd[1]} == 2'b11;
   wire         ar = re[a_index];
   wire         br = re[b_index];
   wire         bi = im[b_index];

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

   assign {b_index, a_index} = pairs[rd];


   //-------------------------------------------------------------------------
   //  Time-multiplexed correlator.
   //-------------------------------------------------------------------------
   correlate_cos_sin_DSP
     #(  .ACCUM(ACCUM), .SUMHI(SUMHI), .DELAY(DELAY) ) CS0
       ( .clk(clk_x),
         .clr(sw),

`ifdef __USE_DSP_SLOW
         // Antenna enables and inputs:
         .en(en),
         .vld(go),
         .hi(hi),
         .ar(ar),
         .br(br),
         .bi(bi),

         // Accumulator inputs and outputs:
         .dcos(dcos),
         .dsin(dsin),
`else
         // Antenna enables and inputs:
         .en(wt),
         .vld(go),
         .hi(rhi),
         .ar(rar),
         .br(rbr),
         .bi(rbi),

         // Accumulator inputs and outputs:
         .dcos(rcos),
         .dsin(rsin),
`endif
         .qcos(qcos),
         .qsin(qsin)
         );

   
   //-------------------------------------------------------------------------
   //  RAM32M's implemented the nerdy way.
   //-------------------------------------------------------------------------
//    parameter INIT = 64'hf0e1d2c3b4a59687;
   parameter INIT = 64'h0;

   RAM32X6_SDP
     #( .INITA(INIT),
        .INITB(INIT),
        .INITC(INIT),
        .INITD(INIT),
        .DELAY(3)
        ) COSRAM [3:0]
       (.WCLK(clk_x),
        .WE(vld),
        .WADDR({{5-TBITS{1'b0}}, wr}),
        .DI(qcos),
        .RADDR({{5-TBITS{1'b0}}, rd}),
        .DO(dcos)
        );

   RAM32X6_SDP
     #( .INITA(INIT),
        .INITB(INIT),
        .INITC(INIT),
        .INITD(INIT),
        .DELAY(3)
        ) SINRAM [3:0]
       (.WCLK(clk_x),
        .WE(vld),
        .WADDR({{5-TBITS{1'b0}}, wr}),
        .DI(qsin),
        .RADDR({{5-TBITS{1'b0}}, rd}),
        .DO(dsin)
        );


endmodule // correlator_DSP
