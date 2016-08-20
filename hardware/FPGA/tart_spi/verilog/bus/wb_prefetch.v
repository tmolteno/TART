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
 * The data prefetches are broken up into multiple block-transfers, of the
 * parameterised sizes, so that data can be fetched from multiple, similar
 * devices.
 * 
 * NOTE:
 *  + currently synchronous, and both bus interfaces share the same clock;
 *  + it would be unwise to change the input `count_i` value when this module
 *    is active;
 * 
 * Changelog:
 *  + 17/06/2016  --  initial file;
 *  + 20/08/2016  --  modified to explicitly break up transfers into smaller
 *                    blocks;
 * 
 * TODO:
 *  + more robust flow-control (by monitoring and generating bus wait-state
 *   cycles);
 * 
 */

module wb_prefetch
  #(parameter WIDTH = 32,
    parameter MSB   = WIDTH-1,
    parameter COUNT = 575,      // <fetch count> - 1
    parameter SBITS = 10,
    parameter SIZE  = 1 << SBITS,
    parameter ASB   = SBITS-1,
    parameter BSIZE = 23,
    parameter BBITS = 5,
    parameter BSTEP = (1 << BBITS) - BSIZE,
    parameter BSB   = BBITS-1,
    parameter DELAY = 3)
   (
    input              rst_i,
    input              clk_i,

    // Signals the start of a prefetch:
    input              begin_i,
    output reg         ready_o = 0,

    // Prefetching master Wishbone-like bus interface:
    output reg         a_cyc_o = 0,
    output reg         a_stb_o = 0,
    output             a_we_o,
    output reg         a_bst_o = 0, // Bulk Sequential Transfer?
    input              a_ack_i,
    input              a_wat_i,
    output reg [ASB:0] a_adr_o = 0,
    input [MSB:0]      a_dat_i,
    output reg [MSB:0] a_dat_o,

    // Transmitting master Wishbone-like bus interface:
    output reg         b_cyc_o = 0,
    output reg         b_stb_o = 0,
    output             b_we_o,
    output reg         b_bst_o = 0, // Bulk Sequential Transfer?
    input              b_ack_i,
    input              b_wat_i,
    output reg [ASB:0] b_adr_o = 0,
    input [MSB:0]      b_dat_i,
    output reg [MSB:0] b_dat_o
    );

   reg [ASB:0]         count = 0;

   wire [ASB:0]        a_adr_nxt = a_adr_o + (a_adr_blk ? BSTEP : 1);
   wire                a_adr_bst = count < COUNT - 1;
   wire                a_adr_blk = a_adr_o[BSB:0] == BSIZE;

   wire                b_adr_max = b_adr_o == COUNT;
   wire                b_adr_bst = b_adr_o < COUNT - 1;

   //-------------------------------------------------------------------------
   //  Port A master Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   reg [3:0]           wat = 0;
//    wire                a_cyc = a_stb_o || !a_ack_i;
   wire                a_cyc = a_stb_o || wat > 1 || !a_ack_i;
   wire                a_stb = a_bst_o;
   wire                a_bst = a_adr_bst && a_cyc_o;

   assign a_we_o = 0;

   //  Port A state and bus-control signals.
   always @(posedge clk_i)
     if (rst_i)
       {a_cyc_o, a_stb_o, a_bst_o} <= #DELAY 3'b000;
     else if (begin_i)
       {a_cyc_o, a_stb_o, a_bst_o} <= #DELAY 3'b111;
     else if (a_cyc_o)
       {a_cyc_o, a_stb_o, a_bst_o} <= #DELAY {a_cyc, a_stb, a_bst};
     else
       {a_cyc_o, a_stb_o, a_bst_o} <= #DELAY 3'b000;

   //  address generation.
   always @(posedge clk_i)
     if (rst_i || begin_i)        a_adr_o <= #DELAY 0;
//      else if (a_cyc_o && a_bst_o) a_adr_o <= #DELAY a_adr_nxt;
     else if (a_cyc_o && a_bst_o && !a_wat_i) a_adr_o <= #DELAY a_adr_nxt;

   //-------------------------------------------------------------------------
   //  Count the requested number of transfers.
   wire [SBITS:0]      count_next = count + 1;
   always @(posedge clk_i)
     if (rst_i || begin_i)        count <= #DELAY 0;
//      else if (a_cyc_o && a_stb_o) count <= #DELAY count_next[ASB:0];
     else if (a_cyc_o && a_bst_o) count <= #DELAY count_next[ASB:0];

   //  Count to make sure that all requested data is retransmitted.
   always @(posedge clk_i)
     if (rst_i)   wat <= #DELAY 0;
     else if (a_cyc_o)
       case ({a_stb_o, a_ack_i})
         2'b10:   wat <= #DELAY wat + 1;
         2'b01:   wat <= #DELAY wat - 1;
         default: wat <= #DELAY wat;
       endcase // case ({a_stb_o, a_ack_i})

   //  Strobe the `ready_o` signal when a prefetch has been completed.
   always @(posedge clk_i)
     if (rst_i || begin_i) ready_o <= #DELAY 0;
     else                  ready_o <= #DELAY a_cyc_o && !a_cyc;
       

   //-------------------------------------------------------------------------
   //  Port B master Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   //  Port B state and bus-control signals.
   wire                b_cyc = b_stb_o || !b_ack_i;
   wire                b_stb = b_bst_o;
   wire                b_bst = b_adr_bst && b_cyc_o;
   wire                b_begin = a_cyc_o && a_ack_i && !b_cyc_o;

   assign b_we_o = b_cyc_o;

   always @(posedge clk_i)
     if (rst_i)
       {b_cyc_o, b_stb_o, b_bst_o} <= #DELAY 3'b000;
     else if (b_begin)
       {b_cyc_o, b_stb_o, b_bst_o} <= #DELAY 3'b111;
     else if (b_cyc_o)
       {b_cyc_o, b_stb_o, b_bst_o} <= #DELAY {b_cyc, b_stb, b_bst};
     else
       {b_cyc_o, b_stb_o, b_bst_o} <= #DELAY 3'b000;

   //  Address generation.
   always @(posedge clk_i)
     if (rst_i || b_begin)        b_adr_o <= #DELAY 0;
//      else if (b_cyc_o && b_bst_o) b_adr_o <= #DELAY b_adr_o + 1;
     else if (b_cyc_o && b_bst_o && !b_wat_i) b_adr_o <= #DELAY b_adr_o + 1;

   //  Transfer incoming data to the output bus.
   always @(posedge clk_i)
     if (a_cyc_o && a_ack_i)      b_dat_o <= #DELAY a_dat_i;


`ifdef __icarus
   //-------------------------------------------------------------------------
   //  Count the number of prefetched words.
   //-------------------------------------------------------------------------
   integer             rxd = 0;

   always @(posedge clk_i)
     if (rst_i || begin_i) rxd <= #DELAY 0;
     else if (a_cyc_o && a_ack_i) rxd <= #DELAY rxd+1;

//    always @(posedge clk_i)
//      if (a_cyc_o && !a_cyc &&
`endif //  `ifdef __icarus


endmodule // wb_prefetch


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
 * Generates the control and address signals needed, to prefetch a block of
 * data, using Wishbone-like burst transfers.
 * 
 * NOTE:
 * 
 * Changelog:
 *  + 20/08/2016  --  initial file;
 * 
 * TODO:
 *  + more robust flow-control (by monitoring and generating bus wait-state
 *   cycles);
 * 
 */

module wb_get_block
  #(parameter BSIZE = 23,
    parameter BBITS = 5,
    parameter BSB   = BBITS-1,
    parameter DELAY = 3)
   (
    input          clk_i,
    input          rst_i,
    output reg     cyc_o = 0,
    output reg     stb_o = 0,
    output         we_o,
    output reg     bst_o = 0,
    input          ack_i,
    input          wat_i,
    output [BSB:0] blk_o,

    input          read_i,
    output reg     done_o = 0
    );

   wire            cyc = stb_o || wat > 1 || !ack_i;
   wire            stb = bst_o;
   wire            bst = blk < BSIZE;
   reg [BSB:0]     blk = 0;
   reg [2:0]       wat = 0; // # of outstanding requests

   wire [BBITS:0]  blk_nxt = blk + 1;

   assign we_o  = 0;
   assign blk_o = blk;


   //-------------------------------------------------------------------------
   //  Fetch-port state and bus-control signals.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o, bst_o} <= #DELAY 3'b000;
     else if (read_i)
       {cyc_o, stb_o, bst_o} <= #DELAY 3'b111;
     else if (cyc_o)
       {cyc_o, stb_o, bst_o} <= #DELAY {cyc, stb, bst};
     else
       {cyc_o, stb_o, bst_o} <= #DELAY 3'b000;

   //-------------------------------------------------------------------------
   //  Address generation, with a block.
   always @(posedge clk_i)
     if (rst_i || read_i)               blk <= #DELAY 0;
     else if (cyc_o && bst_o && !wat_i) blk <= #DELAY blk_nxt;

   //-------------------------------------------------------------------------
   //  Count to make sure that all requested data is retransmitted.
   always @(posedge clk_i)
     if (rst_i)   wat <= #DELAY 0;
     else if (cyc_o)
       case ({stb_o, ack_i})
         2'b10:   wat <= #DELAY wat + 1;
         2'b01:   wat <= #DELAY wat - 1;
         default: wat <= #DELAY wat;
       endcase // case ({stb_o, ack_i})


   //-------------------------------------------------------------------------
   //  Strobe the `done_o` signal when a prefetch has been completed.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i || read_i) done_o <= #DELAY 0;
     else                 done_o <= #DELAY cyc_o && !cyc;


endmodule // wb_get_block
