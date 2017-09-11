`timescale 1ns/100ps
/*
 * Module      : verilog/correlator/bank_switch.v
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
 * Counts the number of correlations for the current bank, and then signals a
 * bank-switch once the counter reaches its maximum.
 * 
 * NOTE:
 * 
 * TODO:
 *  + check the CDC for bank-switching;
 * 
 */

module bank_switch
  #( parameter COUNT = 24,
     parameter MSB   = COUNT-1,
     parameter TICKS = 4,
     parameter TSB   = TICKS-1,
     parameter DELAY = 3)
   (
    input         clk_x, // correlator clock
    input         rst_x,
    input         clk_i, // bus clock
    input         rst_i,
    input         ce_i,

    input [MSB:0] bcount_i, // block size - 1
    input         frame_i,  // new antenna data
    output        swap_x,   // NOTE: correlator domain
    output        swap_o    // NOTE: bus domain
    );

   reg                 sw = 1'b0, switch = 1'b0;
   reg                 sw_x = 0, sw_d = 0; // correlator domain
   reg                 sw_b = 1'b0;        // bus domain

   assign swap_x = sw;
   assign swap_o = switch;


   //-------------------------------------------------------------------------
   //  
   //  Hardware-correlator bank-switching control logic.
   //  
   //-------------------------------------------------------------------------
   wire [COUNT:0]      next_count = wrap_count ? {COUNT+1{1'b0}} : count+1;
   wire                wrap_count = count == bcount_i;
   reg [MSB:0]         count  = {COUNT{1'b0}};
   reg [TSB:0]         delays = {TICKS{1'b0}};

   //-------------------------------------------------------------------------
   //  Count the number of correlations for the current bank, and signal a
   //  bank-switch when the counter reaches its maximum.
   always @(posedge clk_x)
     if (rst_x) begin
        sw    <= #DELAY 1'b0;
        count <= #DELAY {COUNT{1'b0}};
     end
     else if (ce_i) begin
        sw    <= #DELAY delays[TSB] && wrap_count; // signal an upcoming bank-swap
        count <= #DELAY frame_i ? next_count[MSB:0] : count;
     end
     else begin
        sw    <= #DELAY 1'b0;
        count <= #DELAY count;
     end

   always @(posedge clk_x)
     delays <= #DELAY {delays[TSB-1:0], !rst_x && frame_i};


   //-------------------------------------------------------------------------
   //  Synchronise the bank-switching signal to the bus domain.
   //-------------------------------------------------------------------------
   //  TODO: Is this OK?
   always @(posedge clk_x)
     if (rst_x || frame_i) sw_x <= #DELAY 1'b0;
     else if (sw)          sw_x <= #DELAY 1'b1;
     else                  sw_x <= #DELAY sw_x;

   always @(posedge clk_x)
     if (rst_x) sw_d <= #DELAY 0;
     else       sw_d <= #DELAY sw_x && frame_i;

   always @(posedge clk_i or posedge sw_d)
     if (sw_d)                 sw_b <= #DELAY 1'b1;
     else if (rst_i || switch) sw_b <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (rst_i) switch <= #DELAY 1'b0;
     else       switch <= #DELAY sw_b && !switch;


endmodule // bank_switch
