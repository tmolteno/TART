`timescale 1ns/100ps
/*
 * Module      : bench/capture/fake_signal.v
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

module fake_signal
  #(parameter WIDTH = 24,
    parameter MSB   = WIDTH-1,
    parameter RATIO = 12,
    parameter RMAX  = RATIO-1,
    parameter RZERO = 0,
    parameter SHAKE = 1,
    parameter NOISY = 1,
    parameter DELAY = 3)
   (
    input          clock,
    input          reset,
    input          enable,
    output         strobe,
    output         locked,
    output [MSB:0] signal
    );


   reg [MSB:0]     raw;          // raw, random signal data
   wire [MSB:0]    valids, pulses;


   //-------------------------------------------------------------------------
   //  Output is "locked" once all components assert their valid signals.
   //-------------------------------------------------------------------------
   assign locked = &valids;
   assign strobe = &pulses;


   //-------------------------------------------------------------------------
   //  Generate random data.
   //-------------------------------------------------------------------------
   integer         count = 0;
   wire [31:0]     cnext = cwrap ? RZERO : count + 1;
   wire            cwrap = count == RMAX;
    
   always @(posedge clock)
     if (reset)
       count <= #DELAY RATIO-4;
     else
       count <= #DELAY cnext;

   always @(posedge clock)
     if (cwrap)
       raw <= #DELAY $random;


   //-------------------------------------------------------------------------
   //  Generate fake/pseudorandom offsets and jitter, for the given signals.
   //-------------------------------------------------------------------------
   signal_stagger
     #( .PHASE_JITTER(SHAKE),
        .PHASE_OFFSET(SHAKE+1),
        .CYCLE_JITTER(0)        // UNIMPLEMENTED:
        ) STAG [MSB:0]
     (  .clk(clock),
        .rst(reset),
        .ce (enable),
        .d  (raw),
        .stb(pulses),
        .vld(valids),
        .q  (signal)
        );


   //-------------------------------------------------------------------------
   //  Additional debug output.
   //-------------------------------------------------------------------------
   always @(posedge clock)
     if (NOISY && ~&pulses && |pulses)
       $display("%12t: fake_signal: ERROR: strobes out of sync", $time);


endmodule // fake_signal
