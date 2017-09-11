`timescale 1ns/100ps
/*
 * Module      : bench/capture/signal_stagger.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
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
 * Simulation-only module for adding fake jitter, and offsets to the input
 * signal.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

module signal_stagger
  #(//  Signal width parameters:
    parameter WIDTH = 1,
    parameter MSB   = WIDTH-1,

    //  Digitial Frequency Synthesis (DFS) settings:
    parameter RATIO = 12,       // ratio of DFS clock to signal clock
    parameter RBITS = 4,        // bit-width of ratio counter
    parameter RSB   = RBITS-1,  // MSB of ratio counter
    parameter RZERO = 0,        // (DFS) counter zero-value
    parameter RHALF = RATIO>>1, // (DFS) counter half-value
    parameter RMAX  = RATIO-1,  // (DFS) maximum counter value

    parameter CBITS = RBITS+1,  // (signed) phase-counter bit-width
    parameter CSB   = CBITS-1,  // MSB of phase counter
    parameter CZERO = 0,        // counter zero-value
    parameter CHALF = RATIO>>1, // counter half-value
    parameter CMAX  = RATIO-1,  // maximum counter value

    //  Signal jitter/delay options:
    parameter PHASE_JITTER = 1,  // #bits of edge jitter
    parameter PHASE_OFFSET = 1,  // max amount of consistent offset from edge
    parameter CYCLE_JITTER = 1,  // #bits of cycle-length jitter

    //  Simulation-only settings:
    parameter NOISY = 0,         // display extra debug info?
    parameter DELAY = 3)         // simulated combinational delay (ns)
   (
    input              clk,
    input              rst,
    input              ce,
    input [MSB:0]      d,
    output reg         vld = 1'b0,
    output             stb,
    output reg [MSB:0] q = {WIDTH{1'b0}}
    );


   //-------------------------------------------------------------------------
   //  Signals and registers for the (signed) phase counter.
   //-------------------------------------------------------------------------
   reg signed [CSB:0]  count = CZERO;
   wire [CBITS:0]      count_next;
   wire                count_wrap;
   wire [CSB:0]        phase;

   //-------------------------------------------------------------------------
   //  These can be +/- values.
   reg signed [RSB:0]  phase_jitter; // TODO
   reg signed [RSB:0]  phase_offset;
   reg signed [RSB:0]  cycle_jitter; // TODO

   //-------------------------------------------------------------------------
   //  Signals and registers for the DFS base-cycle counter.
   reg [RSB:0]         ratio = RZERO;
   reg                 cycle = 1'b0;
   wire [RBITS:0]      rnext;
   wire                rwrap, valid;


   //-------------------------------------------------------------------------
   //  Internal assignments.
   assign count_next = count + 1;
   assign count_wrap = count == CMAX;
   assign phase      = count + phase_offset + phase_jitter;

   assign rnext      = rwrap ? RZERO : ratio + 1;
   assign rwrap      = ratio == RMAX;
   assign valid      = phase >= CHALF && cycle;

   //-------------------------------------------------------------------------
   //  Output assignments.
   assign stb        = cycle;


   //-------------------------------------------------------------------------
   //  Setup mode from the module's parameters.
   //-------------------------------------------------------------------------
   // Choose a phase-offset on start-up.
   initial begin
      phase_offset = $random;
      phase_offset = phase_offset % (PHASE_OFFSET + 1);
      if (NOISY)
        $display("Phase offset (%m):\t%d", phase_offset);
   end


   //-------------------------------------------------------------------------
   //  Compute the jitters and offsets.
   //-------------------------------------------------------------------------
   always @(posedge clk)
     if (rst || ce && count_wrap) begin
        phase_jitter = $random;
        phase_jitter = phase_jitter % (PHASE_JITTER + 1);
     end

   //-------------------------------------------------------------------------
   //  Change the phase of the incoming data.
   always @(posedge clk)
     if (rst)
       count <= #DELAY CZERO;
     else if (ce)
       count <= #DELAY count_wrap ? CZERO : count+1 ;
     else
       count <= #DELAY count;

   //-------------------------------------------------------------------------
   //  Generate the outputs.
   always @(posedge clk)
     if (rst || !ce)
       q   <= #DELAY 'bz;
     else if (phase == CHALF)
       q   <= #DELAY d;
     else
       q   <= #DELAY q;

   always @(posedge clk)
     if (rst || !ce)
       vld <= #DELAY 1'b0;
     else if (valid)
       vld <= #DELAY 1'b1;
     else
       vld <= #DELAY vld;


   //-------------------------------------------------------------------------
   //  Ratio-counter enforces the overall cycle-length/period value.
   //-------------------------------------------------------------------------
   always @(posedge clk)
     if (rst || !ce) begin
        cycle <= #DELAY 1'b0;
        ratio <= #DELAY RZERO;
     end
     else begin
        cycle <= #DELAY rnext == RMAX;
        ratio <= #DELAY rnext;
     end


endmodule // signal_stagger
