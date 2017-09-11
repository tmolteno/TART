`timescale 1ns/100ps
/*
 * Module      : rtl/wb_fetch.v
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
 * Generates the control and address signals needed, to fetch a block of data,
 * using Wishbone-like burst transfers.
 * 
 * NOTE:
 *  + Uses pipelined BURST READS from the Wishbone B4 spec;
 *  + Pipelined transfers assert `stb_o` once/transfer, whereas classic
 *    transfers assert `stb_o` until `ack_i`;
 *  + Commands are typically sent each cycle, unless `wat_i`, during burst
 *    transfers;
 * 
 * TODO:
 *  + add a "latency" parameter, so that the most efficient method (to track
 *    outstanding requests) can be used;
 *  + finish/test support for `wat_i`, `rty_i`, and `err_i`;
 * 
 * Changelog:
 *  + 21/10/2016  --  initial file;
 * 
 */


//----------------------------------------------------------------------------
//  Use parameterised-size command-queue?
`define __USE_FETCH_QUEUE


module wb_fetch
  #(//  Data-fetch counter parameters:
    parameter FETCH = 24,       // number of words to fetch
    parameter FMAX  = FETCH-1,  // maximum counter value
    parameter FBITS = 5,        // bit-width of fetch counter
    parameter FSB   = FBITS-1,  // MSB of fetch counter

    //  Wishbone SPEC B4 command-queue parameters:
    parameter QUEUE = 1<<QBITS, // Size of command queue
    parameter QMAX  = QUEUE-1,  // Maximum queue index value
    parameter QBITS = 2,        // Queue index bit-width
    parameter QSB   = QBITS-1,  // MSB of queue index

    //  Additional mode parameters:
    parameter PIPED = 1,        // Pipelined (WB SPEC B4) transfers (0/1)?

    //  Simulation-only parameters:
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
    output [FSB:0] adr_o,

    input          fetch_i,
    output reg     ready_o = 1'b0
    );


   parameter FZERO = {FBITS{1'b0}};
   parameter QZERO = {QBITS{1'b0}};
   parameter QONE  = {{QSB{1'b0}}, 1'b1};


   reg [FSB:0]     adr = FZERO;


   assign we_o  = 1'b0;
   assign adr_o = adr;


`ifdef __USE_FETCH_QUEUE
   //-------------------------------------------------------------------------
   //
   //  COMMAND QUEUE CONTROL.
   //
   //  NOTE: No command-queue overflow detection!
   //  NOTE: This version supports only the parameterised number of queued
   //    commands, of a block-transfer -- and the command-queue size is
   //    ideally tuned to match the (maximum) latency of any attached, slave
   //    devices.
   //
   //-------------------------------------------------------------------------
   reg [QSB:0]     cqi = QMAX;
   wire [FBITS:0]  adr_nxt = adr + 1;
   wire            adr_max = adr == FMAX;
   wire            cyc_w, stb_w;

   //  Continue bus cycle until all commands have completed:
   assign cyc_w = !err_i && !rty_i && !(cqi == QZERO && ack_i);

   //  Issue commands until the maximum fetch-address is reached, and keep
   //  strobe high if the slave stalls:
   assign stb_w = wat_i || !adr_max;

   //-------------------------------------------------------------------------
   //  Wishbone interface state and bus-control signals.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o} <= #DELAY 2'b00;
     else if (fetch_i)
       {cyc_o, stb_o} <= #DELAY 2'b11;
     else if (cyc_o)
       {cyc_o, stb_o} <= #DELAY {cyc_w, stb_w};
     else
       {cyc_o, stb_o} <= #DELAY 2'b00;

   //-------------------------------------------------------------------------
   //  Increase the address-counter after every command has been issued
   //  (and accepted).
   always @(posedge clk_i)
     if (fetch_i)                          adr <= #DELAY FZERO;
     else if (stb_o && !wat_i && !adr_max) adr <= #DELAY adr_nxt[FSB:0];

   //-------------------------------------------------------------------------
   //  Up/down counter for the command-queue.
   always @(posedge clk_i)
     if (fetch_i)               // queue is empty on fetch-start:
       cqi <= #DELAY QMAX;
     else
       case ({stb_o, ack_i, wat_i})
         3'b000: cqi <= #DELAY cqi;     // No change.
         3'b001: cqi <= #DELAY 3'bx;    // Invalid.
         3'b010: cqi <= #DELAY cqi - 1; // Data recieved, so decrement.
         3'b011: cqi <= #DELAY 3'bx;    // Invalid.
         3'b100: cqi <= #DELAY cqi + 1; // Command enqueued, so increment.
         3'b101: cqi <= #DELAY cqi;     // Command rejected, so no change.
         3'b110: cqi <= #DELAY cqi;     // Issue + finish, so no change.
         3'b111: cqi <= #DELAY cqi - 1; // Finished, buy new command rejected.
       endcase // case ({stb_o, ack_i, wat_i})

   //-------------------------------------------------------------------------
   //  Strobe the `ready_o` signal when a fetch has been completed.
   always @(posedge clk_i)
     ready_o <= #DELAY cyc_o && !cyc_w;


`else // !`ifdef __USE_FETCH_QUEUE
   //-------------------------------------------------------------------------
   //
   //  NAIVE COMMAND QUEUE.
   //
   //  NOTE: This version supports the queuing of all commands, of a block-
   //    transfer, but is inefficient for large block-sizes.
   //
   //-------------------------------------------------------------------------
   reg [FSB:0]     wat = FMAX; // # of outstanding requests
   wire [FBITS:0]  adr_nxt = adr + 1;
   wire [FBITS:0]  wat_prv = wat - 1;
   wire            cyc_w = !err_i && !rty_i && (wat > 0 || !ack_i);
   wire            stb_w = stb_o && (adr < FMAX || wat_i);


   //-------------------------------------------------------------------------
   //  Wishbone interface state and bus-control signals.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o} <= #DELAY 2'b00;
     else if (fetch_i)
       {cyc_o, stb_o} <= #DELAY 2'b11;
     else if (cyc_o)
       {cyc_o, stb_o} <= #DELAY {cyc_w, stb_w};
     else
       {cyc_o, stb_o} <= #DELAY 2'b00;

   //-------------------------------------------------------------------------
   //  Address generation, with a block.
   always @(posedge clk_i)
     if (fetch_i)
       adr <= #DELAY {FBITS{1'b0}};
     else if (cyc_o && !wat_i)
       adr <= #DELAY adr_nxt[FSB:0];

   //-------------------------------------------------------------------------
   //  Count to make sure that all requested data is retransmitted.
   always @(posedge clk_i)
     if (rst_i || ready_o)
       wat <= #DELAY FMAX;
     else if (cyc_o && ack_i)
       wat <= #DELAY wat_prv[FSB:0];


   //-------------------------------------------------------------------------
   //  Strobe the `ready_o` signal when a fetch has been completed.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     ready_o <= #DELAY cyc_o && wat == FZERO && ack_i;
`endif // !`ifdef __USE_FETCH_QUEUE


endmodule // wb_fetch
