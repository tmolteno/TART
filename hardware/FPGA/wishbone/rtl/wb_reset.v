`timescale 1ns/100ps
/*
 * Module      : rtl/wb_reset.v
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
 * Simple reset circuit, connected via a Wishbone-compatible interconnect.
 * 
 * NOTE:
 *  + supports both classic and pipelined transfers;
 * 
 * TODO:
 * 
 */

module wb_reset
  #(parameter WIDTH = 8,
    parameter MSB   = WIDTH-1,
    parameter RTIME = 4,
    parameter RSB   = RTIME-1,

    //  Simulation-only settings:
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input          clk_i,
    input          rst_i,
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output reg     ack_o = 1'b0,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    input          reset_ni, // from optional external source
    output reg     reset_o = 1'b0
    );

   reg             reset  = 1'b0;
   reg [RSB:0]     resets = {RTIME{1'b0}};
   wire            reset_w;


   assign dat_o   = {{MSB{1'b0}}, reset_o};
   assign reset_w = cyc_i && stb_i && we_i && dat_i[0];
//    assign reset_w = cyc_i && stb_i && we_i && !ack_o && dat_i[0];


   //-------------------------------------------------------------------------
   //  Logic for the Wishbone-like interconnect.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       ack_o <= #DELAY 1'b0;
     else
       ack_o <= #DELAY cyc_i && stb_i && !ack_o;


   //-------------------------------------------------------------------------
   //  Synchronise the `reset_ni` signal, as this could be from an unknown/
   //  different clock-domain.
   //-------------------------------------------------------------------------
   reg             rstn_0, rstn_1;

   always @(posedge clk_i)
     {rstn_1, rstn_0} <= #DELAY {rstn_0, reset_ni};


   //-------------------------------------------------------------------------
   //  Reset logic.
   //-------------------------------------------------------------------------
   //  NOTE: The current behaviour is to hold `reset` high until the DCM's
   //    are locked, or else allow reset-requests to come from over Wishbone.
   //  TODO: Not possible for `rstn_1` to ever be used?
   always @(posedge clk_i)
     if (rst_i) reset <= #DELAY 1'b0;
     else       reset <= #DELAY reset ? ~rstn_1 : reset_w;

   always @(posedge clk_i) begin
      reset_o <= #DELAY |resets;
      resets  <= #DELAY {resets[RTIME-2:0], reset};
   end


endmodule // wb_reset
