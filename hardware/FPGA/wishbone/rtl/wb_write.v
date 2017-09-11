`timescale 1ns/100ps
/*
 * Module      : rtl/wb_write.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
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
 * Generates the Wishbone, master, control address signals needed to perform
 * single WRITE bus transactions.
 * 
 * NOTE:
 *  + See `wb_cycle` for a description of the Wishbone parameters and modes;
 * 
 * TODO:
 * 
 * Changelog:
 *  + 26/10/2016  --  initial file;
 * 
 */

module wb_write
  #(//  Capabilities/mode parameters:
    parameter ASYNC = 1,    // Combinational control signals (0/1/2)?
    parameter PIPED = 1,    // Pipelined (WB SPEC B4) transfers (0/1)?
    parameter CHECK = 1,    // Filter spurious "chatter," and check errors?

    parameter DELAY = 3)    // Combinational simulation delay (ns)
   (
    input  clk_i,
    input  rst_i,
    output cyc_o,
    output stb_o,
    output we_o,
    input  ack_i,
    input  wat_i,
    input  rty_i,
    input  err_i,

    input  write_i,
    output busy_o,
    output done_o,
    output fail_o
    );

   assign we_o = 1'b1;


   //-------------------------------------------------------------------------
   //  Frame the bus cycle.
   //-------------------------------------------------------------------------
   wb_cycle
     #( .ASYNC(ASYNC),
        .PIPED(PIPED),
        .CHECK(CHECK),
        .DELAY(DELAY)
        ) WRCYC
       (
        .clk_i  (clk_i),
        .rst_i  (rst_i),
        .cyc_o  (cyc_o),
        .stb_o  (stb_o),
        .ack_i  (ack_i),
        .wat_i  (wat_i),
        .rty_i  (rty_i),
        .err_i  (err_i),
        .start_i(write_i),
        .busy_o (busy_o),
        .done_o (done_o),
        .fail_o (fail_o)
        );


endmodule // wb_write
