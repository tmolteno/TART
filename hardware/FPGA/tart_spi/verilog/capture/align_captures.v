`timescale 1ns/100ps
/*
 * Module      : verilog/capture/align_captures.v
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
 * Aligns a set of super-sampled signals, each with varying delay.
 * 
 * Data from multiple, independent sources can vary in phase, but if the data
 * is assumed to have total phase differences of less than half of a sample-
 * period, then it is possible to determine the common alignments of all the
 * signals.
 * 
 * NOTE:
 *  + has only been tested with 'RATIO == 12';
 *  + assumes that each `data_in[i]` remains constant until its next strobe;
 *  + the supersampling clock is assumed to be synchronous with the data
 *    clock;
 * 
 * TODO:
 *  + window sizes for invalid calculations;
 *  + clearing and counting assertions of `invalid`;
 * 
 */

module align_captures
  #( parameter WIDTH = 24,
     parameter MSB   = WIDTH-1,
     parameter RATIO = 12,
     parameter RBITS = 4,
     parameter RSB   = RBITS-1,
     parameter HALF  = RATIO>>1,
     parameter CLEAR = HALF+1,
     parameter VALID = HALF-1,
     parameter DELAY = 3)
   (
    input              clock_i,
    input              reset_i,
    input              align_i,

    input [MSB:0]      data_in,
    input [MSB:0]      strobes,
    input [MSB:0]      lockeds,
    input [MSB:0]      invalids,

    output reg [MSB:0] data_out,
    output reg         ready = 1'b0,
    output reg         locked = 1'b0,
    output reg         invalid = 1'b0,
    input              ack
    );

   reg [RSB:0]         count = {RBITS{1'b0}}, clear = {RBITS{1'b0}};
   reg                 strobe = 1'b0, window = 1'b0;
   reg [MSB:0]         strobes_reg = {RBITS{1'b0}};
   reg                 strobes_all = 1'b0;
   wire [MSB:0]        acc_w;
   wire                end_w, lkd_w, stb_w, vld_w, rdy_w;

   //  Error signals.
   wire has_overlaps, invalid_input, too_much_spread, capture_error;


   //-------------------------------------------------------------------------
   //  Internal, combinational conditionals.
   //-------------------------------------------------------------------------
   assign vld_w = strobe && clear >= CLEAR;
   assign rdy_w = strobes_all && locked && !ready;
   assign stb_w = |strobes;
   assign lkd_w = &lockeds;

   //  Accumulates the ready-strobes, until all signals are ready.
   assign acc_w = strobes_reg | strobes;
   assign end_w = &acc_w;

   //-------------------------------------------------------------------------
   //  Assert `invalid` whenever behaviour falls outside of:
   //   1) all 24 strobes arrive within 5 cycles;
   //   2) this is followed by at least 7 cycles of no strobes; and
   //   3) all signals remain locked.
   //-------------------------------------------------------------------------
   assign has_overlaps    = |(strobes_reg & strobes); // strobed twice?
   assign invalid_input   = |invalids;
   assign too_much_spread = locked && lkd_w && strobe && clear > 0 && clear < CLEAR;
   assign capture_error   = has_overlaps || invalid_input || too_much_spread;


   //-------------------------------------------------------------------------
   //  Track the arrivals of all strobes.
   //-------------------------------------------------------------------------
   // Pipeline the strobe 24-bit OR gate.
   always @(posedge clock_i)
     if (reset_i)
       strobe <= #DELAY 1'b0;
     else if (align_i)
       strobe <= #DELAY stb_w;
     else
       strobe <= #DELAY strobe;

   // A window ends once all strobes have arrived.
   always @(posedge clock_i)
     if (reset_i) begin
        strobes_reg <= #DELAY {RBITS{1'b0}};
        strobes_all <= #DELAY 1'b0;
     end
     else if (align_i) begin
        if (strobes_all && !strobe) begin
           strobes_reg <= #DELAY {RBITS{1'b0}};
           strobes_all <= #DELAY 1'b0;
        end
        else begin
           strobes_reg <= #DELAY acc_w;
           strobes_all <= #DELAY end_w;
        end
     end

   //-------------------------------------------------------------------------
   //  An acquisition window begins with any strobe, and ends once all have
   //  been received.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i)
       window <= #DELAY 1'b0;
     else if (align_i) begin
        if (stb_w)
          window <= #DELAY 1'b1;
        else if (end_w)
          window <= #DELAY 1'b0;
        else
          window <= #DELAY window;
     end

   //-------------------------------------------------------------------------
   //  Count the number of clock-ticks for a capture-period, and for the case
   //  without any data arriving.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i)
       count <= #DELAY {RBITS{1'b0}};
     else if (align_i) begin
        if (!window && strobe)
          count <= #DELAY {RBITS{1'b0}};
        else
          count <= #DELAY count + 1;
     end

   always @(posedge clock_i)
     if (reset_i)
       clear <= #DELAY {RBITS{1'b0}};
     else if (align_i)
       clear <= #DELAY strobe || window ? {RBITS{1'b0}} : clear + 1 ;
     else
       clear <= #DELAY clear;


   //-------------------------------------------------------------------------
   //  Acquisition is locked if aligned.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i)
       locked <= #DELAY 1'b0;
     else if (align_i)
       locked <= #DELAY vld_w ? lkd_w : locked ;
     else
       locked <= #DELAY locked;


   //-------------------------------------------------------------------------
   //  Present the captured & aligned data.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i)
       ready     <= #DELAY 1'b0;
     else if (align_i && rdy_w) begin
        ready    <= #DELAY 1'b1;
        data_out <= #DELAY data_in;
     end
     else
       ready <= #DELAY 1'b0;

   // TODO:
   always @(posedge clock_i)
     if (reset_i)
       invalid <= #DELAY 1'b0;
     else if (align_i)
       invalid <= #DELAY !ack && invalid || locked && capture_error;
     else
       invalid <= #DELAY invalid;


endmodule // align_captures
