`timescale 1ns/100ps
/*
 * Module      : rtl/wb_chunk.v
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
 * Break a larger word up into chunks, and parameterised by size and bit-
 * width.
 * 
 * NOTE:
 *  + only supports "classic" Wishbone cycles;
 * 
 * TODO:
 *  + upgrade to Wishbone SPEC B4;
 * 
 */

module wb_chunk
  #(parameter WIDTH = 48,
    parameter WSB   = WIDTH-1,
    parameter CHUNK = 8,
    parameter MSB   = CHUNK-1,
    parameter COUNT = (WIDTH+CHUNK-1) / CHUNK - 1,
    parameter CBITS = 3,
    parameter CSB   = CBITS-1,
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

    output reg     fetch_o = 0,
    input          ready_i,
    input [WSB:0]  value_i
    );

   reg             empty = 1'b1;
   reg [WSB:0]     word = {WIDTH{1'bx}};
   reg [CSB:0]     count = {CBITS{1'b0}};
   wire            wrap_count = count == COUNT;
   wire [CBITS:0]  next_count = wrap_count ? {CBITS{1'b0}} : count + 1;

   assign dat_o = word[MSB:0];

   //-------------------------------------------------------------------------
   //  Acknowledge incoming requests.
   always @(posedge clk_i)
     if (rst_i || ack_o)
       ack_o <= #DELAY 1'b0;
     else if (cyc_i && stb_i && !empty)
       ack_o <= #DELAY 1'b1;

   always @(posedge clk_i)
     if (rst_i || fetch_o && ready_i)
       count <= #DELAY {CBITS{1'b0}};
     else if (cyc_i && stb_i && !ack_o && !empty)
       count <= #DELAY next_count[CSB:0];

   //-------------------------------------------------------------------------
   //  Retrieve data from external source.
   always @(posedge clk_i)
     if (rst_i || fetch_o && ready_i)
       fetch_o <= #DELAY 1'b0;
     else if (cyc_i && stb_i && empty)
       fetch_o <= #DELAY 1'b1;

   always @(posedge clk_i)
     if (rst_i)
       empty <= #DELAY 1'b1;
     else if (fetch_o && ready_i)
       empty <= #DELAY 1'b0;
     else if (wrap_count && cyc_i && stb_i && !ack_o)
       empty <= #DELAY 1'b1;

   //-------------------------------------------------------------------------
   //  Store incoming data, and break into chunks, to be sent.
   always @(posedge clk_i)
     if (fetch_o && ready_i)
       word <= #DELAY value_i;
     else if (cyc_i && stb_i && ack_o)
       word <= #DELAY {{CHUNK{1'b0}}, word[WSB:CHUNK]};


endmodule // wb_chunk
