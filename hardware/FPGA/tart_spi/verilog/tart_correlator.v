`timescale 1ns/100ps
/*
 *
 * Top-level of the TART, time-multiplexed block of correlator-blocks.
 * 
 * NOTE:
 *  + hardwired for a 24 antenna setup;
 *  + the correlators are set to use 12:1 time-multiplexing, so `clk_x` must
 *    be 12x the frequency of the sampling clock;
 *  + the upper 3-bits of the address determine either:
 *     a) one of the six correlator blocks;
 *     b) the ones-counter unit; or
 *     c) the bank of system registers.
 *  + the number of bits in a block's counter corresponds to the maximum size
 *    of the accumulator, because the visibilities are monotone increasing;
 * 
 * TODO:
 *  + when `bst_i` deasserts, deassert `stb[i]` the next cycle? Currently, the
 *    "tails" of a transaction are one cycle too long;
 *  + compute the exponent of the count-size;
 *  + status registers, and correctly handle overflows;
 * 
 */

// Bus transaction states.
`define BUS_IDLE  0
`define BUS_WAIT  1
`define BUS_READ  2
`define BUS_WRITE 4

module tart_correlator
  #( parameter BLOCK = 32,
     parameter MSB   = BLOCK-1,
     parameter DELAY = 3)
   (
    input              clk_x,
    input              rst,

    // Wishbone-like bus interface for reading visibilities.
    input              clk_i, // bus clock
    input              cyc_i,
    input              stb_i,
    input              we_i, // writes only work for system registers
    input              bst_i, // Bulk Sequential Transfer?
    output reg         ack_o = 0,
    input [9:0]        adr_i, // upper address-space for registers
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o,

    // The real component of the signal from the antennas.
    input              enable, // data aquisition is active
    input [MSB:0]      blocksize, // block size - 1
    input              strobe, // `antenna` data is valid
    input [23:0]       antenna,// the real component from each antenna
    output reg         switch = 0
    );

   //-------------------------------------------------------------------------
   //  Hardware-correlator control logic.
   //-------------------------------------------------------------------------
   //  Fill a block with visibilities, and then switch banks.
   wire [MSB:0]        next_blk = wrap_blk ? 0 : blk + 1 ;
   wire                wrap_blk = blk == blocksize;
   reg                 active = 0;
   reg                 sw = 0, strobe_r = 0;
   reg [MSB:0]         blk = 0;
   reg [3:0]           delays = 0;

   //-------------------------------------------------------------------------
   //  The configuration file includes the parameters that determine which
   //  antannae connect to each of the correlators.
`include "../include/tart_config.v"

   // Activate the Hilbert transform and correlators once this module has been
   // enabled, and when the first valid data arrives.
   always @(posedge clk_x)
     if (rst || !enable)        active <= #DELAY 0;
     else if (enable && strobe) active <= #DELAY 1;
     else                       active <= #DELAY active;

   always @(posedge clk_x)
     if (rst) begin
        sw  <= #DELAY 0;
        blk <= #DELAY 0;
     end
     else if (go) begin
        sw  <= #DELAY delays[3] && wrap_blk; // signal an upcoming bank-swap
        blk <= #DELAY strobe ? next_blk : blk;
     end
     else begin
        sw  <= #DELAY 0;
        blk <= #DELAY blk;
     end

   always @(posedge clk_x)
     delays <= #DELAY {delays[2:0], !rst && strobe && active};

   always @(posedge clk_x)
     if (rst) switch <= #DELAY 0;
     else     switch <= #DELAY sw;

   //-------------------------------------------------------------------------
   //  Bus interface.
   //-------------------------------------------------------------------------
   reg [2:0]           bus_state = `BUS_IDLE;
//    reg [7:0]           stbs = 0;
//    reg [9:0]          adr;
   reg                 cyc_r = 0; // TODO:
   wire [MSB:0]        dats [0:7];
//    wire [7:0]          stbs, acks;
   wire [7:0]          acks;
   wire                we_w = 0, ack_w = |acks;
   wire [2:0]          bus_mode = we_i ? `BUS_WRITE : `BUS_READ ;

//    assign stbs = {6'b0, stb_i} << adr_i[10:8];

   assign acks[5:2] = 0; // FIXME:
   assign acks[7] = 0;

   always @(posedge clk_i)
     if (rst)
       bus_state <= #DELAY `BUS_IDLE;
     else
       case (bus_state)
         `BUS_IDLE:
           bus_state <= #DELAY cyc_i && stb_i ? bus_mode : bus_state;

         `BUS_WAIT:
           bus_state <= #DELAY cyc_i ? (stb_i ? bus_mode : bus_state) : `BUS_IDLE ;

         `BUS_READ:
           bus_state <= #DELAY bst_i ? bus_state : `BUS_WAIT ;

         `BUS_WRITE:
           bus_state <= #DELAY bst_i ? bus_state : `BUS_WAIT ;
       endcase // case (bus_state)

   /*
   always @(posedge clk_i)
     if (rst)
       cyc_r <= #DELAY 0;
     else if (cyc_i && bus_state == `BUS_IDLE)
       cyc_r <= #DELAY 1;
     else if (bus_state == `BUS_READ || bus_state == `BUS_WRITE)
       cyc_r <= #DELAY bst_i;
     else if (!cyc_i && cyc_r)
       cyc_r <= #DELAY 0;
     else
       cyc_r <= #DELAY cyc_r;

   // Set the strobes for the sub-units.
   always @(posedge clk_i)
     if (rst) stbs <= #DELAY 0;
     else if (cyc_i && stb_i) stbs[adr_i[10:8]] <= #DELAY 1;
     else stbs <= #DELAY 0;
    */

   //-------------------------------------------------------------------------
   //  Address decoders.
   reg [7:0]           stbs = 0;
   reg [2:0]           dev = 0;

   always @(posedge clk_i)
     if (rst)
       stbs <= #DELAY 0;
     else
       stbs <= #DELAY {8{stb_i}} & (1 << adr_i[9:7]);

   // 8:1 MUX and output-data latch.
   always @(posedge clk_i)
     if (cyc_i && !we_i && ack_w)
       dat_o <= #DELAY dats[dev];

   always @(posedge clk_i)
     dev <= #DELAY adr_i[10:8];

   always @(posedge clk_i)
     if (rst) ack_o <= #DELAY 0;
     else     ack_o <= #DELAY cyc_i && ack_w;


   //-------------------------------------------------------------------------
   //  Hilbert transform to reconstruct imaginaries.
   //-------------------------------------------------------------------------
   wire                go;
   wire [23:0]         re, im;

   fake_hilbert #( .WIDTH(24) ) HILB0
     (  .clk(clk_x),
        .rst(rst),
        .en(active),
        .d(antenna),
        .valid(go),
        .re(re),
        .im(im)
        );


   //-------------------------------------------------------------------------
   //  There are six correlator-blocks, each connected to 12 of the 24
   //  antennas.
   //-------------------------------------------------------------------------
   wire [5:0]          oc, os;  // overflows

   correlator_block
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS00_00),
         .PAIRS1(PAIRS00_01),
         .PAIRS2(PAIRS00_02),
         .PAIRS3(PAIRS00_03),
         .DELAY (DELAY)
         ) CORRELATOR_BLOCK0
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stbs[0]),
         .we_i (we_w),
         .bst_i(bst_i),
         .ack_o(acks[0]),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dats[0]),

         .sw(sw),
         .en(go),
         .re(re),
         .im(im),

         .overflow_cos(oc[0]),
         .overflow_sin(os[0])
         );

   correlator_block
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS01_00),
         .PAIRS1(PAIRS01_01),
         .PAIRS2(PAIRS01_02),
         .PAIRS3(PAIRS01_03),
         .DELAY (DELAY)
         ) CORRELATOR_BLOCK1
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stbs[1]),
         .we_i (we_w),
         .bst_i(bst_i),
         .ack_o(acks[1]),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dats[1]),

         .sw(sw),
         .en(go),
         .re(re),
         .im(im),

         .overflow_cos(oc[1]),
         .overflow_sin(os[1])
         );

   correlator_block
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS02_00),
         .PAIRS1(PAIRS02_01),
         .PAIRS2(PAIRS02_02),
         .PAIRS3(PAIRS02_03),
         .DELAY (DELAY)
         ) CORRELATOR_BLOCK2
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stbs[2]),
         .we_i (we_w),
         .bst_i(bst_i),
         .ack_o(acks[2]),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dats[2]),

         .sw(sw),
         .en(go),
         .re(re),
         .im(im),

         .overflow_cos(oc[2]),
         .overflow_sin(os[2])
         );

   correlator_block
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS03_00),
         .PAIRS1(PAIRS03_01),
         .PAIRS2(PAIRS03_02),
         .PAIRS3(PAIRS03_03),
         .DELAY (DELAY)
         ) CORRELATOR_BLOCK3
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stbs[3]),
         .we_i (we_w),
         .bst_i(bst_i),
         .ack_o(acks[3]),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dats[3]),

         .sw(sw),
         .en(go),
         .re(re),
         .im(im),

         .overflow_cos(oc[3]),
         .overflow_sin(os[3])
         );

   correlator_block
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS04_00),
         .PAIRS1(PAIRS04_01),
         .PAIRS2(PAIRS04_02),
         .PAIRS3(PAIRS04_03),
         .DELAY (DELAY)
         ) CORRELATOR_BLOCK4
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stbs[4]),
         .we_i (we_w),
         .bst_i(bst_i),
         .ack_o(acks[4]),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dats[4]),

         .sw(sw),
         .en(go),
         .re(re),
         .im(im),

         .overflow_cos(oc[4]),
         .overflow_sin(os[4])
         );

   correlator_block
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS05_00),
         .PAIRS1(PAIRS05_01),
         .PAIRS2(PAIRS05_02),
         .PAIRS3(PAIRS05_03),
         .DELAY (DELAY)
         ) CORRELATOR_BLOCK5
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stbs[5]),
         .we_i (we_w),
         .bst_i(bst_i),
         .ack_o(acks[5]),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dats[5]),

         .sw(sw),
         .en(go),
         .re(re),
         .im(im),

         .overflow_cos(oc[5]),
         .overflow_sin(os[5])
         );

   
   //-------------------------------------------------------------------------
   //  Count the number of ones, from each antenna.
   //  NOTE: Used to compute their means, as their gains are unknown.
   //-------------------------------------------------------------------------
   ones_count
     #(  .ACCUM (BLOCK),
         .DELAY (DELAY)
         ) ONES_COUNT0
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stbs[6]),
         .we_i (we_w),
         .bst_i(bst_i),
         .ack_o(acks[6]),
         .adr_i(adr_i[4:0]),
         .dat_i(32'bx),
         .dat_o(dats[6]),

         .switch(sw),
         .enable(go),
         .antenna(re)
         );


endmodule // tart_correlator    
