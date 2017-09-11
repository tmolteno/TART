`timescale 1ns/100ps
/*
 * Module      : verilog/shift_reg.v
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
 * Shift-register that implements the behaviour of the Xilinx SRL primitives.
 * 
 * NOTE:
 *  + an input shift-amount ('a') of zero corresponds to a delay of one clock-
 *    cycle, and all ones corresponds to a delay of '2^ABITS';
 * 
 * TODO:
 *  + testing;
 *  + verify that Xilinx correctly infers the correct primitives;
 * 
 */

module shift_reg
  #(parameter DEPTH = 16,       // maximum shifter cycles
    parameter MSB   = DEPTH-1,  // MSB of the shifter
    parameter INIT  = 0,        // initialisation value
    parameter ABITS = 4,        // address bit-width
    parameter ASB   = ABITS-1,  // MSB of the address
    parameter DELAY = 3)        // simulation combinational delay
   (
    input         clk,
    input         ce,
    input [ASB:0] a,
    input         d,
    output        q
    );

   reg [MSB:0]    sr = INIT;
   wire [DEPTH:0] sr_w;

   assign q    = sr[a];
   assign sr_w = {sr, d};


   always @(posedge clk)
     if (ce)
       sr <= #DELAY sr_w[MSB:0];


endmodule // shift_reg
