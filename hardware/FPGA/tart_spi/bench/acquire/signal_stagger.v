`timescale 1ns/100ps
/*
 * Module      : bench/acquire/signal_stagger.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
 * 
 * Simulation-only module for adding fake jitter, and offsets to the input
 * signal.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

module signal_stagger
  #(//  Signal jitter/delay parameters:
    parameter WIDTH_SIGNAL = 1,
    parameter MSB = WIDTH_SIGNAL-1,
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
    output reg [MSB:0] q = {WIDTH_SIGNAL{1'b0}}
    );

   reg signed [3:0]    phase_jitter; // TODO
   reg signed [3:0]    phase_offset;
   reg signed [3:0]    cycle_jitter; // TODO

   reg signed [4:0]    count = 5'h0;

   //-------------------------------------------------------------------------
   //  Setup mode from the module's parameters.
   //-------------------------------------------------------------------------
   // Choose a phase-offset on start-up.
   initial begin : INITIAL_VALUES
      phase_offset = $random;
      phase_offset = phase_offset % (PHASE_OFFSET + 1);
      if (NOISY)
        $display("Phase offset:\t%d", phase_offset);
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
   //-------------------------------------------------------------------------
   wire count_wrap = count == 11;
   wire [4:0] phase = count + phase_offset + phase_jitter;

   always @(posedge clk)
     if (rst)     count <= 0;
     else if (ce) count <= count_wrap ? 0 : count+1 ;
     else         count <= count;

   always @(posedge clk)
     if (ce) q <= phase == 6 ? d : q ;

endmodule // signal_stagger
