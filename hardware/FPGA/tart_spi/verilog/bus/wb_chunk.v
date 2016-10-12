`timescale 1ns/100ps
/*
 * Module      : verilog/bus/wb_chunk.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Break a larger word up into chunks, and parameterised by size and bit-
 * width.
 * 
 * NOTE:
 *  + only supports "classic" Wishbone cycles;
 * 
 * TODO:
 * 
 */

module wb_chunk
  #(parameter WIDTH = 48,
    parameter WSB   = WIDTH-1,
    parameter CHUNK = 8,
    parameter MSB   = CHUNK-1,
    parameter COUNT = (WIDTH+CHUNK-1) / CHUNK - 1,
    parameter CBITS = 3,
    parameter CSB   = ABITS-1,
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input          clk_i,
    input          rst_i,
    input          cyc_i,
    input          stb_i,
    input          we_i, // ignored
    output reg     ack_o = 0,
    input [MSB:0]  dat_i, // ignored
    output [MSB:0] dat_o,

    output         fetch_o,
    input          ready_i,
    input [WSB:0]  value_i
    );

   reg [WSB:0]     word = {WIDTH{1'bx}};
   reg [CSB:0]     count = {CBITS{1'b0}};
   wire            wrap_count = count == COUNT-1;
   wire [CBITS:0]  next_count = wrap_count ? {CBITS{1'b0}} : count + 1;

   assign dat_o = word[MSB:0];

   //-------------------------------------------------------------------------
   //  Acknowledge incoming requests.
   always @(posedge clk_i)
     if (rst_i || ack_o)
       ack_o <= #DELAY 1'b0;
     else if (cyc_i && stb_i)
       ack_o <= #DELAY 1'b1;

   always @(posedge clk_i)
     if (rst_i || fetch_o && ready_i)
       count <= #DELAY {CBITS{1'b0}};
     else if (cyc_i && stb_i && !ack_o)
       count <= #DELAY next_count[CSB:0];

   //-------------------------------------------------------------------------
   //  Retrieve data from external source.
   always @(posedge clk_i)
     if (rst_i || fetch_o && ready_i)
       fetch_o <= #DELAY 1'b0;
     else if (cyc_i && stb_i && !ack_o)
       fetch_o <= #DELAY wrap_count;

   //-------------------------------------------------------------------------
   //  Store incoming data, and break into chunks, to be sent.
   always @(posedge clk_i)
     if (fetch_o && ready_i)
       word <= #DELAY value_i;
     else if (cyc_i && stb_i && ack_o)
       word <= #DELAY {{CHUNK{1'b0}}, word[WSB:CHUNK]};


endmodule // wb_chunk
