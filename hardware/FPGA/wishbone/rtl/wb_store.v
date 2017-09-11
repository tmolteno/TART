`timescale 1ns/100ps
/*
 * Module      : rtl/wb_store.v
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
 * Generates the control and address signals needed, to store a block of data,
 * using Wishbone-like burst transfers.
 * 
 * NOTE:
 *  + Uses pipelined BURST WRITES from the Wishbone B4 spec;
 *  + Pipelined transfers assert `stb_o` once/transfer, whereas classic
 *    transfers assert `stb_o` until `ack_i`;
 *  + Commands are typically sent each cycle, unless `wat_i`, during burst
 *    transfers;
 * 
 * TODO:
 *  + add a "latency" parameter, so that the most efficient method, to track
 *    outstanding requests, can be used;
 *  + finish/test support for `wat_i`, `rty_i`, and `err_i`;
 *  + support byte-enables?
 * 
 * Changelog:
 *  + 22/10/2016  --  initial file;
 * 
 */

module wb_store
  #(parameter STORE = 24,       // number of words to store
    parameter SMAX  = STORE-1,  // maximum counter value
    parameter SBITS = 5,        // bit-width of store counter
    parameter SSB   = SBITS-1,  // MSB of store counter
    parameter PIPED = 1,        // Pipelined (WB4 spec) transfers (0/1)?
    parameter BURST = 1,        // Pipelined burst transfers (0/1)?
    parameter DELAY = 3)        // combinational simulation delay (ns)
   (
    input          clk_i,
    input          rst_i,
    output reg     cyc_o = 1'b0,
    output reg     stb_o = 1'b0,
    output         we_o,
    input          ack_i,
    input          wat_i,
    input          rty_i,
    input          err_i,
    output [SSB:0] adr_o,

    input          store_i,
    output reg     ready_o = 1'b0
    );

   reg [SSB:0]     adr = {SBITS{1'b0}};
   wire [SBITS:0]  adr_nxt = adr + 1;
   wire            cyc_w = !err_i && !rty_i && (stb_o || !ack_i);
   wire            stb_w = stb_o && (adr < SMAX || wat_i);


   assign we_o  = cyc_o;
   assign adr_o = adr;


   //-------------------------------------------------------------------------
   //  Wishbone interface state and bus-control signals.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o} <= #DELAY 2'b00;
     else if (store_i)
       {cyc_o, stb_o} <= #DELAY 2'b11;
     else if (cyc_o)
       {cyc_o, stb_o} <= #DELAY {cyc_w, stb_w};
     else
       {cyc_o, stb_o} <= #DELAY 2'b00;

   //-------------------------------------------------------------------------
   //  Address generation, with a block.
   always @(posedge clk_i)
     if (store_i)
       adr <= #DELAY {SBITS{1'b0}};
     else if (cyc_o && !wat_i)
       adr <= #DELAY adr_nxt[SSB:0];


   //-------------------------------------------------------------------------
   //  Strobe the `ready_o` signal when a store has been completed.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     ready_o <= #DELAY cyc_o && !stb_o && ack_i;


endmodule // wb_store
