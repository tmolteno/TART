`timescale 1ns/100ps
/*
 * Module      : legacy/wb4_get_block.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
 * 
 * Generates the control and address signals needed, to prefetch a block of
 * data, using Wishbone-like burst transfers.
 * 
 * NOTE:
 *  + OBSOLETE (replaced by 'wb_fetch', Pat @ 21/10/2016);
 * 
 * TODO:
 * 
 */

//----------------------------------------------------------------------------
//  Uses pipelines BURST READS from the Wishbone B4 spec.
//----------------------------------------------------------------------------
module wb4_get_block
  #(parameter BSIZE = 24,
    parameter BMAX  = BSIZE-1,
    parameter BBITS = 5,
    parameter BSB   = BBITS-1,
    parameter DELAY = 3)
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
    output [BSB:0] adr_o,

    input          read_i,
    output reg     done_o = 1'b0
    );

   reg [BSB:0]     blk = {BBITS{1'b0}};
   reg [BSB:0]     wat = BMAX; // # of outstanding requests
   wire [BBITS:0]  blk_nxt = blk + 1;
   wire [BBITS:0]  wat_prv = wat - 1;
   wire            cyc_w = !err_i && !rty_i && (wat > 1 || !ack_i);
   wire            stb_w = blk < BMAX || wat_i;


   assign we_o  = 1'b0;
   assign adr_o = blk;


   //-------------------------------------------------------------------------
   //  Fetch-port state and bus-control signals.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o} <= #DELAY 2'b00;
     else if (read_i)
       {cyc_o, stb_o} <= #DELAY 2'b11;
     else if (cyc_o)
       {cyc_o, stb_o} <= #DELAY {cyc_w, stb_w};
     else
       {cyc_o, stb_o} <= #DELAY 2'b00;

   //-------------------------------------------------------------------------
   //  Address generation, with a block.
   always @(posedge clk_i)
     if (read_i)
       blk <= #DELAY {BBITS{1'b0}};
     else if (cyc_o && !wat_i)
       blk <= #DELAY blk_nxt[BSB:0];

   //-------------------------------------------------------------------------
   //  Count to make sure that all requested data is retransmitted.
   always @(posedge clk_i)
     if (rst_i || done_o)
       wat <= #DELAY BMAX;
     else if (cyc_o && ack_i)
       wat <= #DELAY wat_prv[BSB:0];


   //-------------------------------------------------------------------------
   //  Strobe the `done_o` signal when a prefetch has been completed.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     done_o <= #DELAY cyc_o && wat == 1 && ack_i;


endmodule // wb4_get_block
