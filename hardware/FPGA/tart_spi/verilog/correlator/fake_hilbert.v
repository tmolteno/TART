`timescale 1ns/100ps
/*
 * Module      : verilog/fake_hilbert.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Approximate Hilbert transform, for a 1-bit signal.
 * 
 * NOTE:
 *  + for 4x oversampled signals, the 90 degree phase difference between
 *    samples is approximately equal to the complex component of the previous
 *    sample, and is very cheap to compute;
 * 
 * TODO:
 * 
 */

module fake_hilbert
  #( parameter WIDTH = 24,
     parameter MSB   = WIDTH-1,
     parameter TICKS = 12,
     parameter TBITS = 4,
     parameter TSB   = TBITS-1,
     parameter DELAY = 3)
   (
    input              clk,
    input              rst,
    input              en,
    input [MSB:0]      d,
    output reg         valid = 1'b0,
    output reg         strobe = 1'b0,
    output reg         frame  = 1'b0,

    output reg [MSB:0] re = {WIDTH{1'b0}},
    output reg [MSB:0] im = {WIDTH{1'b0}}
    );

   reg                 go = 1'b0;
   reg [TSB:0]         ticks = {TBITS{1'b0}};
   wire                ticks_wrap = ticks == TICKS-1;
   wire [TSB:0]        ticks_next = ticks_wrap ? {TBITS{1'b0}} : ticks+1;

   always @(posedge clk)
     if (rst)
       ticks <= #DELAY {TBITS{1'b0}};
     else if (en || ticks > 0)
       ticks <= #DELAY ticks_next;

   always @(posedge clk)
     if (rst) begin
        go    <= #DELAY 1'b0;
        valid <= #DELAY 1'b0;
     end
     else begin
        go <= #DELAY ticks_wrap ? en : 1'b0;
        valid <= #DELAY ticks == {TBITS{1'b0}} ? go : valid;
     end

   //  Marks the first & last sample of a frame.
   always @(posedge clk)
     if (rst) begin
        strobe <= #DELAY 1'b0;
        frame  <= #DELAY 1'b0;
     end
     else begin
        strobe <= #DELAY go;
        frame  <= #DELAY ticks_wrap;
     end

   always @(posedge clk)
     if (en && ticks == {TBITS{1'b0}})
       {im, re} <= #DELAY {re, d};
     else
       {im, re} <= #DELAY {im, re};


endmodule // fake_hilbert
