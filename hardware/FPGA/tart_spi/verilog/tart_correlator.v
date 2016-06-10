`timescale 1ns/100ps
/*
 *
 * Top-level of the TART, time-multiplexed block of correlator-blocks.
 * 
 * NOTE:
 *  + hardwired for a 24 antenna setup;
 *  + the correlators are set to use 12:1 time-multiplexing, so `clk_x` must
 *    be 12x the frequency of the sampling clock;
 * 
 */

// Bus transaction states.
`define BUS_IDLE  0
`define BUS_READ  1
`define BUS_WAIT  2
`define BUS_WRITE 4

module tart_correlator
  #( parameter ACCUM = 32,
     parameter MSB   = ACCUM-1,
     parameter DELAY = 3)
   (
    input              clk_x,
    input              rst,

    // Wishbone-like bus interface for reading visibilities.
    input              clk_i, // bus clock
    input              cyc_i,
    input              stb_i,
    input              we_i, // writes only work for system registers
    output reg         ack_o = 0,
    input [10:0]       adr_i, // upper address-space for registers
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o,

    // Real and imaginary components from the antennas.
    input              en, // data is valid
    input [23:0]       re,
    input [23:0]       im,

    output reg         overflow_cos = 0,
    output reg         overflow_sin = 0
    );


   //-------------------------------------------------------------------------
   //  System registers.
   //  TODO:
   //-------------------------------------------------------------------------
   //  Register#:
   //    00  --  logarithm of the visibilities counter;
   //
   reg [MSB:0]         count_max;
   reg [7:0]           count_log;


   //-------------------------------------------------------------------------
   //  Bus interface.
   //-------------------------------------------------------------------------
   reg [2:0]           bus_state = `BUS_IDLE;

   always @(posedge clk_i)
     if (rst)
       bus_state <= `BUS_IDLE;
     else
       case (bus_state)
       endcase // case (bus_state)


   //-------------------------------------------------------------------------
   //  There are six correlator-blocks, each connected to 12 of the 24
   //  antennas.
   //-------------------------------------------------------------------------
   // Break up the 24 antennas into blocks, for convenience.
   wire [5:0]          re0 = re[ 5: 0];
   wire [5:0]          re1 = re[11: 6];
   wire [5:0]          re2 = re[17:12];
   wire [5:0]          re3 = re[23:18];

   wire [5:0]          im0 = im[ 5: 0];
   wire [5:0]          im1 = im[11: 6];
   wire [5:0]          im2 = im[17:12];
   wire [5:0]          im3 = im[23:18];

   wire [5:0]          oc, os;  // overflows

   correlator_block
     #(  .ACCUM (ACCUM),
//          .PAIRS0(PAIRS0),
//          .PAIRS1(PAIRS1),
//          .PAIRS2(PAIRS2),
//          .PAIRS3(PAIRS3),
         .DELAY (DELAY)
         ) CORRELATOR_BLOCK0
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stb_0),
         .we_i (we_i),
         .ack_o(ack_0),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dat_0),

         .sw(sw),
         .en(en),
         .re({re1, re0}),
         .im({im1, im0}),

         .overflow_cos(oc[0]),
         .overflow_sin(os[0])
         );

   correlator_block
     #(  .ACCUM (ACCUM),
//          .PAIRS0(PAIRS0),
//          .PAIRS1(PAIRS1),
//          .PAIRS2(PAIRS2),
//          .PAIRS3(PAIRS3),
         .DELAY (DELAY)
         ) CORRELATOR_BLOCK1
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stb_1),
         .we_i (we_i),
         .ack_o(ack_1),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dat_1),

         .sw(sw),
         .en(en),
         .re({re2, re0}),
         .im({im2, im0}),

         .overflow_cos(oc[1]),
         .overflow_sin(os[1])
         );


endmodule // tart_correlator    
