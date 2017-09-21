`timescale 1ns/100ps
/*
 * Module      : verilog/tart_wishbone.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
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
 */

module correlator
  #(parameter ACCUM = 32,
    //      parameter 
    // Pairs of antennas to correlate:
    parameter PAIRS = 120'hb1a191817161b0a090807060,
    parameter MSB   = ACCUM - 1,
    parameter DELAY = 3)
   (
    input              clk_x, // correlator clock
    input              rst,

    // Wishbone-like bus interface for reading visibilities.
    input              clk_i, // bus clock
    input              cyc_i,
    input              stb_i,
    input              we_i, // writes are ignored
    input              bst_i, // burst-mode transfer?
    output reg         ack_o = 0,
    input [4:0]        adr_i,
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o,

    // Real and imaginary components from the antennas.
    input              sw, // switch banks
    input              en, // data is valid
    input [23:0]       re,
    input [23:0]       im
    );


   //-------------------------------------------------------------------------
   //  Distributed RAM's for the accumulators.
   //-------------------------------------------------------------------------
   // For Xilinx FPGA's, this should be two `RAM32M's.
   reg [MSB:0]         cosram[0:31];
   reg [MSB:0]         sinram[0:31];

   reg [MSB:0]         dcos, dsin;
   wire [MSB:0]        qcos, qsin;
   //    wire [MSB:0]    dcos = cosram[{bank, x_rd_adr}];
   //    wire [MSB:0]    dsin = sinram[{bank, x_rd_adr}];

   reg                 bank = 0, swap = 0, clear = 0;
   wire                valid;
   reg [3:0]           x_rd_adr = 0;
   reg [4:0]           x_wt_adr = 0, x_wr_adr = 0;
   wire                wrap_x_rd_adr = x_rd_adr == 11;
   wire [3:0]          next_x_rd_adr = wrap_x_rd_adr ? 0 : x_rd_adr + 1 ;

   // Pipelined correlator requires cycles for:
   //   { read, MAC, write } .
   always @(posedge clk_x)
     if (rst) begin
        x_rd_adr <= #DELAY 0;
        x_wt_adr <= #DELAY 0;
        x_wr_adr <= #DELAY 0;
     end
     else begin
        x_rd_adr <= #DELAY en ? next_x_rd_adr : x_rd_adr;
        x_wt_adr <= #DELAY go ? {bank, x_rd_adr} : x_wt_adr;
        x_wr_adr <= #DELAY valid ? x_wt_adr : x_wr_adr;
     end

   // Banks are switched at the next address-wrap event.
   always @(posedge clk_x)
     if (rst) begin : RAM_RESET_LOGIC
        swap  <= #DELAY 0;
        bank  <= #DELAY 0;
        clear <= #DELAY 1;
     end
//      else if (wrap_x_rd_adr && swap) begin // swap banks
     else if (wrap_x_rd_adr && (sw || swap)) begin // swap banks
        swap  <= #DELAY 0;
        bank  <= #DELAY ~bank;
        clear <= #DELAY 1;
     end
     else if (sw && !swap) begin // swap banks @next wrap
        swap  <= #DELAY 1;
     end
     else if (en && wrap_x_rd_adr && clear) begin // finished restarting counters
        clear <= #DELAY 0;
     end

   // Read and write RAM contents for the correlator.
   always @(posedge clk_x) begin : RAM_READ_WRITE
     if (!rst && en) begin
        dcos <= #DELAY clear ? 0 : cosram[{bank, x_rd_adr}] ;
        dsin <= #DELAY clear ? 0 : sinram[{bank, x_rd_adr}] ;
     end
     if (!rst && valid) begin
        cosram[x_wr_adr] <= #DELAY qcos;
        sinram[x_wr_adr] <= #DELAY qsin;
     end
   end


   //-------------------------------------------------------------------------
   //  Wishbone-like bus interface logic.
   //-------------------------------------------------------------------------
   wire [4:0] adr_w = {bank_n, adr_i[3:0]};
   reg        bank_n = 1;

   // Acknowledge any request, even if ignored.
   always @(posedge clk_i)
     if (rst) ack_o <= #DELAY 0;
     else     ack_o <= #DELAY cyc_i && stb_i;

   // Read only from the innactive bank.
   always @(posedge clk_i)
     if (cyc_i && stb_i)
       dat_o <= #DELAY adr_i[4] ? sinram[adr_w] : cosram[adr_w] ;

   //-------------------------------------------------------------------------
   // Synchronise the number of the innactive bank across domains.
   always @(posedge clk_i)
     if (rst) bank_n <= #DELAY 1;
     else     bank_n <= #DELAY ~bank;
   

   //-------------------------------------------------------------------------
   //  Select pairs of antenna to correlate.
   //-------------------------------------------------------------------------
   wire [119:0] pairs_wide = PAIRS;
   wire [9:0]  pairs_index = pairs[x_rd_adr];
   wire [4:0]  a_index = pairs_index[4:0];
   wire [4:0]  b_index = pairs_index[9:5];
//    reg [9:0]   pairs[0:11];
   wire [9:0]  pairs[0:11];
   reg         go = 0, ar, br, bi;

   assign pairs[00] = pairs_wide[  9:  0];
   assign pairs[01] = pairs_wide[ 19: 10];
   assign pairs[02] = pairs_wide[ 29: 20];
   assign pairs[03] = pairs_wide[ 39: 30];
   assign pairs[04] = pairs_wide[ 49: 40];
   assign pairs[05] = pairs_wide[ 59: 50];
   assign pairs[06] = pairs_wide[ 69: 60];
   assign pairs[07] = pairs_wide[ 79: 70];
   assign pairs[08] = pairs_wide[ 89: 80];
   assign pairs[09] = pairs_wide[ 99: 90];
   assign pairs[10] = pairs_wide[109:100];
   assign pairs[11] = pairs_wide[119:110];

      /*
   initial begin : PAIRS_ROM
      pairs[00] = pairs_wide[  9:  0];
      pairs[01] = pairs_wide[ 19: 10];
      pairs[02] = pairs_wide[ 29: 20];
      pairs[03] = pairs_wide[ 39: 30];
      pairs[04] = pairs_wide[ 49: 40];
      pairs[05] = pairs_wide[ 59: 50];
      pairs[06] = pairs_wide[ 69: 60];
      pairs[07] = pairs_wide[ 79: 70];
      pairs[08] = pairs_wide[ 89: 80];
      pairs[09] = pairs_wide[ 99: 90];
      pairs[10] = pairs_wide[109:100];
      pairs[11] = pairs_wide[119:110];

      pairs[00] = pairs_wide[ 7: 0];
      pairs[01] = pairs_wide[15: 8];
      pairs[02] = pairs_wide[23:16];
      pairs[03] = pairs_wide[31:24];
      pairs[04] = pairs_wide[39:32];
      pairs[05] = pairs_wide[47:40];
      pairs[06] = pairs_wide[55:48];
      pairs[07] = pairs_wide[63:56];
      pairs[08] = pairs_wide[71:64];
      pairs[09] = pairs_wide[79:72];
      pairs[10] = pairs_wide[87:80];
      pairs[11] = pairs_wide[95:88];
   end // block: PAIRS_ROM
       */

   // Add a cycle of latency to wait for the RAM read.
   always @(posedge clk_x) begin
      go <= #DELAY en;
      ar <= #DELAY re[a_index];
      br <= #DELAY re[b_index];
      bi <= #DELAY im[b_index];
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
         .ar(ar),
         .br(br),
         .bi(bi),

         // Accumulator inputs and outputs:
         .dcos(dcos),
         .dsin(dsin),
         .valid(valid),
         .qcos(qcos),
         .qsin(qsin)
         );


endmodule // correlator
