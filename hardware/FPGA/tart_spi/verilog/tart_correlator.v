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
    input [10:0]       adr_i, // upper address-space for registers
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o,

    // The real component of the signal from the antennas.
    input              enable, // data aquisition is active
    input              strobe, // `antenna` data is valid
    input [23:0]       antenna,// the real component from each antenna
    output reg         switch = 0
    );


   //-------------------------------------------------------------------------
   //  System registers.
   //  NOTE: Addressed by `{3'b111, reg#}`.
   //  TODO:
   //-------------------------------------------------------------------------
   //  Register#:
   //    00  --  status register;
   //    01  --  logarithm of the bit-width of the visibilities counter;
   //
   reg [MSB:0]         count_max = -1;
   reg [7:0]           count_log;
   reg                 active = 0;
   reg                 unread_data = 0;

   // TODO: Should be computed from `count_log`, and synchronised across
   //   domains.
   always @(posedge clk_i)
     if (rst)
       count_max <= #DELAY -1;
     else if (cyc_i && stb_i && we_i && adr_i == 11'h701)
       count_max <= #DELAY dat_i;


   //-------------------------------------------------------------------------
   //  Hardware-correlator control logic.
   //-------------------------------------------------------------------------
   //  Fill a block with visibilities, and then switch banks.
   wire [MSB:0]        next_blk = wrap_blk ? 0 : blk + 1 ;
   wire                wrap_blk = blk == count_max;
   reg                 sw = 0, strobe_r = 0;
   reg [MSB:0]         blk = 0;
   reg [3:0]           delays = 0;

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
   reg                 ack_7 = 0;
//    reg [7:0]           stbs = 0;
//    reg [10:0]          adr;
   wire [MSB:0]        dats [0:7];
   wire [7:0]          stbs, acks;
   wire                we_w = 0, ack_w = |acks;
   wire [2:0]          bus_mode = we_i ? `BUS_WRITE : `BUS_READ ;

   assign stbs = {6'b0, stb_i} << adr_i[10:8];

   assign acks[5:2] = 0; // FIXME:
   assign acks[7] = ack_7; // TODO: system registers.

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
   // Set the strobes for the sub-units.
   always @(posedge clk_i)
     if (rst) stbs <= #DELAY 0;
     else if (cyc_i && stb_i) stbs[adr_i[10:8]] <= #DELAY 1;
     else stbs <= #DELAY 0;
    */

   // 8:1 MUX and output-data latch.
   always @(posedge clk_i)
     if (cyc_i && stb_i && !we_i && ack_w)
       dat_o <= #DELAY dats[adr_i[10:8]];

   always @(posedge clk_i)
     if (rst) ack_o <= #DELAY 0;
     else     ack_o <= #DELAY cyc_i && stb_i ? ack_w : 0 ;

   //-------------------------------------------------------------------------
   // Acknowledge accesses to system registers.
   always @(posedge clk_i)
     if (rst)
       ack_7 <= #DELAY 0;
     else if (cyc_i && stb_i && adr_i[10:8] == 3'h7 && (bst_i || !ack_7))
       ack_7 <= #DELAY 1;
     else
       ack_7 <= #DELAY 0;


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
     #(  .ACCUM (BLOCK),
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
         .stb_i(stbs[0]),
         .we_i (we_w),
         .bst_i(bst_i),
         .ack_o(acks[0]),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dats[0]),

         .sw(sw),
         .en(go),
         .re({re1, re0}),
         .im({im1, im0}),

         .overflow_cos(oc[0]),
         .overflow_sin(os[0])
         );

   correlator_block
     #(  .ACCUM (BLOCK),
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
         .stb_i(stbs[1]),
         .we_i (we_w),
         .bst_i(bst_i),
         .ack_o(acks[1]),
         .adr_i(adr_i[6:0]),
         .dat_i(32'bx),
         .dat_o(dats[1]),

         .sw(sw),
         .en(go),
         .re({re2, re0}),
         .im({im2, im0}),

         .overflow_cos(oc[1]),
         .overflow_sin(os[1])
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
