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
 * OBSOLETE:
 *  + 02/11/2016  --  use `wb_sram_prefetch.v` instead;
 * 
 * Has two master Wishbone-like buses, for prefetching data from one slave
 * device/bus, and then transferring this data to another slave device.
 * 
 * The data-prefetches are broken up into multiple block-transfers, of the
 * parameterised sizes, so that data can be fetched from multiple, similar
 * devices.
 * 
 * NOTE:
 *  + not Wishbone SPEC B4 compliant;
 *  + not robust, unless the storage device is a low-latency, deterministic
 *    store, like a SRAM;
 *  + currently synchronous, and both bus interfaces share the same clock;
 *  + it would be unwise to change the input `count_i` value while this module
 *    is active;
 * 
 * Changelog:
 *  + 17/06/2016  --  initial file;
 *  + 20/08/2016  --  modified to explicitly break up transfers into smaller
 *                    blocks;
 *  + 02/11/2016  --  obsoleted;
 * 
 * TODO:
 *  + more robust flow-control (by monitoring and generating bus wait-state
 *   cycles);
 * 
 */

module wb_prefetch
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
    input              rst_i,
    input              clk_i,

    // Signals the start of a prefetch:
    input              begin_i,
    output reg         ready_o = 0,

    // Prefetching master Wishbone-like bus interface:
    output             a_cyc_o,
    output             a_stb_o,
    output             a_we_o,
    output             a_bst_o, // Bulk Sequential Transfer?
    input              a_ack_i,
    input              a_wat_i,
    input              a_err_i,
    output [ASB:0]     a_adr_o,
    input [MSB:0]      a_dat_i,
    output [MSB:0]     a_dat_o,

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

   reg [ASB:0]         count = {ABITS{1'b0}};
   reg [SBITS:0]       count_nxt;
   reg                 count_end = 1'b0;
   reg                 read = 1'b0;
   wire                done;


   initial begin
      $display("\nModule : wb_prefetch (%m)\n\tWIDTH\t= %4d\n\tCOUNT\t= %4d\n\tSIZE\t= %4d\n\tSBITS\t= %4d\n\tBSIZE\t= %4d\n\tBBITS\t= %4d\n\tBSTEP\t= %4d\n\tUBITS\t= %4d\n", WIDTH, COUNT, SIZE, SBITS, BSIZE, BBITS, BSTEP, UBITS);
   end


   //-------------------------------------------------------------------------
   //  Prefetcher control-signals.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i || begin_i) count <= #DELAY {SBITS{1'b0}};
     else if (done)        count <= #DELAY count_nxt;

   //  Strobe the `ready_o` signal when a prefetch has been completed.
   always @(posedge clk_i)
     if (rst_i || begin_i) ready_o <= #DELAY 1'b0;
     else                  ready_o <= #DELAY count_end && done;

   //  Pipeline these signals, since the block-prefetches take several clock-
   //  cycles to complete.
   always @(posedge clk_i) begin
      count_nxt <= #DELAY count + BSIZE;
      count_end <= #DELAY !begin_i && count_nxt >= COUNT;
   end


   //-------------------------------------------------------------------------
   //  Port A master Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   reg [USB:0]         a_adr = {UBITS{1'b0}};
   wire [UBITS:0]      a_adr_nxt = a_adr + 1;
   wire [BSB:0]        a_blk;

   assign a_adr_o = {a_adr, a_blk};
   assign a_dat_o = {WIDTH{1'bx}};

   //-------------------------------------------------------------------------
   //  Keep prefetching blocks, until at least `COUNT` words have been
   //  transferred.
   always @(posedge clk_i)
     if (rst_i)
       {a_adr, read} <= #DELAY {(UBITS+1){1'b0}};
     else if (begin_i)
       {a_adr, read} <= #DELAY {{UBITS{1'b0}}, 1'b1};
     else if (done && count_nxt < COUNT)
       {a_adr, read} <= #DELAY {a_adr_nxt[USB:0], 1'b1};
     else
       {a_adr, read} <= #DELAY {a_adr, 1'b0};


   //-------------------------------------------------------------------------
   //  Port B master Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   //  Port B state and bus-control signals.
   reg [BSB:0]         b_blk = 0;
   wire [BBITS:0]      b_nxt = b_blk + 1;
   wire                b_run = a_cyc_o && a_ack_i && !b_cyc_o;
   wire                b_cyc = b_stb_o || !b_ack_i;
   wire                b_stb = b_bst_o;
   wire                b_bst = b_blk < BSIZE-2;

   assign b_we_o = b_cyc_o;

   always @(posedge clk_i)
     if (rst_i)
       {b_cyc_o, b_stb_o, b_bst_o} <= #DELAY 3'b000;
     else if (b_run)
       {b_cyc_o, b_stb_o, b_bst_o} <= #DELAY 3'b111;
     else if (b_cyc_o)
       {b_cyc_o, b_stb_o, b_bst_o} <= #DELAY {b_cyc, b_stb, b_bst};
     else
       {b_cyc_o, b_stb_o, b_bst_o} <= #DELAY 3'b000;

   //-------------------------------------------------------------------------
   //  Address generation.
   wire [SBITS:0]      b_adr_next = b_adr_o + 1;

   always @(posedge clk_i)
     if (rst_i || begin_i)
       b_adr_o <= #DELAY {SBITS{1'b0}};
     else if (b_cyc_o && (b_bst_o && !b_wat_i || !b_cyc))
       b_adr_o <= #DELAY b_adr_next[ASB:0];

   //  Transfer incoming data to the output bus.
   always @(posedge clk_i)
     if (a_ack_i) b_dat_o <= #DELAY a_dat_i;

   //-------------------------------------------------------------------------
   //  Block-counter, for computing burst transfers.
   always @(posedge clk_i)
     if (rst_i || b_run)
       b_blk <= #DELAY {BBITS{1'b0}};
     else
       b_blk <= #DELAY b_bst_o ? b_nxt[BSB:0] : b_blk;


   //-------------------------------------------------------------------------
   //  Address-generation unit, for each block.
   //-------------------------------------------------------------------------
   wb_get_block
     #(.BSIZE(BSIZE), .BBITS(BBITS), .DELAY(DELAY)
       ) FETCH0
       ( .clk_i(clk_i),
         .rst_i(rst_i),
         .cyc_o(a_cyc_o),
         .stb_o(a_stb_o),
         .we_o (a_we_o),
         .bst_o(a_bst_o),
         .ack_i(a_ack_i),
         .wat_i(a_wat_i),
         .err_i(a_err_i),
         .adr_o(a_blk),

         .read_i(read),
         .done_o(done)
         );


`ifdef __icarus
   //-------------------------------------------------------------------------
   //  Count the number of prefetched words.
   //-------------------------------------------------------------------------
   integer             rxd = 0;

   always @(posedge clk_i)
     if (rst_i || begin_i)
       rxd <= #DELAY 0;
     else if (a_cyc_o && a_ack_i)
       rxd <= #DELAY rxd+1;
`endif //  `ifdef __icarus


endmodule // wb_prefetch
