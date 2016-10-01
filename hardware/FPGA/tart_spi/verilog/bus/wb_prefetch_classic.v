`timescale 1ns/100ps
/*
 * Module      : verilog/bus/wb_prefetch.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
 * 
 * Has two master Wishbone-like buses, for prefetching data from one slave
 * device/bus, and then transferring this data to another slave device.
 * 
 * The data-prefetches are broken up into single Wishbone Classic transfers.
 * These transfers are grouped into blocks, each of the parameterised size, so
 * that data can be fetched from multiple, similar devices.
 * 
 * NOTE:
 *  + currently synchronous, and both bus interfaces share the same clock;
 *  + it would be unwise to change the input `count_i` value while this module
 *    is active;
 * 
 * Changelog:
 *  + 01/10/2016  --  initial file;
 * 
 * TODO:
 *  + additional testing;
 * 
 */

module wb_prefetch_classic
  #(parameter WIDTH = 32,
    parameter MSB   = WIDTH-1,
    parameter COUNT = 576,      // <fetch count>
    parameter SBITS = 10,
    parameter SIZE  = 1 << SBITS,
    parameter ASB   = SBITS-1,
    parameter BSIZE = 24,
    parameter BBITS = 5,
    parameter BSTEP = (1 << BBITS) - BSIZE,
    parameter BSB   = BBITS-1,
    parameter UBITS = SBITS-BBITS,
    parameter USB   = UBITS-1,
    parameter DELAY = 3)
   (
    input          rst_i,
    input          clk_i,

    // Signals the start of a prefetch:
    input          begin_i,
    output reg     ready_o = 0,

    // Prefetching master Wishbone-like bus interface:
    output         a_cyc_o,
    output         a_stb_o,
    output         a_we_o,
    output         a_bst_o, // Bulk Sequential Transfer?
    input          a_ack_i,
    input          a_wat_i,
    input          a_err_i,
    output [ASB:0] a_adr_o,
    input [MSB:0]  a_dat_i,
    output [MSB:0] a_dat_o,

    // Transmitting master Wishbone-like bus interface:
    output         b_cyc_o,
    output         b_stb_o,
    output         b_we_o,
    output         b_bst_o, // Bulk Sequential Transfer?
    input          b_ack_i,
    input          b_wat_i,
    output [ASB:0] b_adr_o,
    input [MSB:0]  b_dat_i,
    output [MSB:0] b_dat_o
    );

   reg [ASB:0]         count = {SBITS{1'b0}};
   reg [SBITS:0]       count_nxt;
   reg                 count_end = 1'b0;

   reg [MSB:0]         dat;
   reg [ASB:0]         adr = {SBITS{1'b0}};

   wire                blk_inc = adr[BSB:0] == BSIZE-1;
   wire [BBITS:0]      wrd_nxt = blk_inc ? {BBITS{1'b0}} : adr[BSB:0]+1;
   wire [UBITS:0]      blk_nxt = adr[ASB:UBITS] + (blk_inc ? 1 : 0);
   wire [ASB:0]        adr_nxt = {blk_nxt[USB:0], wrd_nxt[BSB:0]};

   assign a_cyc_o = pc[0];
   assign a_stb_o = pc[0];
   assign a_we_o  = 1'b0;
   assign a_bst_o = 1'b0;
   assign a_adr_o = adr;
   assign a_dat_o = {WIDTH{1'bz}};

   assign b_cyc_o = pc[1];
   assign b_stb_o = pc[1];
   assign b_we_o  = pc[1];
   assign b_bst_o = 1'b0;
   assign b_adr_o = adr;
   assign b_dat_o = dat;

   parameter PC_IDLE  = 0;
   parameter PC_FETCH = 1;
   parameter PC_STORE = 2;

   reg [1:0]           pc = PC_IDLE;

   always @(posedge clk_i)
     if (rst_i)
       pc <= #DELAY PC_IDLE;
     else
       case (pc)
         PC_IDLE:  if (begin_i) pc <= #DELAY PC_FETCH;
         PC_FETCH: if (a_ack_i) pc <= #DELAY PC_STORE;
         PC_STORE: if (b_ack_i) pc <= #DELAY count_end ? PC_IDLE : PC_FETCH;
       endcase

   always @(posedge clk_i)
     if (rst_i || begin_i)
       adr <= #DELAY {SBITS{1'b0}};
     else
       begin
          if (pc[0] && a_ack_i) dat <= #DELAY a_dat_i;
          if (pc[1] && b_ack_i) adr <= #DELAY adr_nxt;
       end

   //-------------------------------------------------------------------------
   //  Prefetcher control-signals.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i || begin_i)
       count <= #DELAY {SBITS{1'b0}};
     else if (pc[0] && a_ack_i)
       count <= #DELAY count_nxt;

   //  Strobe the `ready_o` signal when a prefetch has been completed.
   always @(posedge clk_i)
     if (rst_i || begin_i) ready_o <= #DELAY 1'b0;
     else                  ready_o <= #DELAY count_end && pc[1] && b_ack_i;

   //  Pipeline these signals, since the block-prefetches take several clock-
   //  cycles to complete.
   always @(posedge clk_i) begin
      count_nxt <= #DELAY count + 1;
      count_end <= #DELAY !begin_i && count_nxt >= COUNT;
   end


endmodule // wb_prefetch_classic
