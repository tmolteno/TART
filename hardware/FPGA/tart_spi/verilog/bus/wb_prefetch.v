`timescale 1ns/100ps
/*
 * 
 * Has two master Wishbone-like buses, for prefetching data from one slave
 * device/bus, and then transferring this data to another slave device.
 * 
 * NOTE:
 *  + currently synchronous, and both bus interfaces share the same clock;
 *  + it would be unwise to change the input `count_i` value when this module
 *    is active;
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


endmodule // wb_prefetch
