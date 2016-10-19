`timescale 1ns/100ps
/*
 * Module      : verilog/tart_correlator.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
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

`include "tartcfg.v"

module tart_correlator
  #( parameter BLOCK = 24,
     parameter MSB   = BLOCK-1,
     parameter ABITS = 14,
     parameter ASB   = ABITS-1,
     parameter AXNUM = 24,
     parameter NSB   = AXNUM-1,
     parameter BBITS = 4,       // visibilities banks
     parameter BSB   = BBITS-1,
     parameter DELAY = 3)
   (
    input              clk_x,
    input              rst,

    //  Wishbone-like bus interface for reading visibilities.
    input              clk_i, // bus clock
    input              cyc_i,
    input              stb_i,
    input              we_i, // writes only work for system registers
    input              bst_i, // Bulk Sequential Transfer?
    output reg         ack_o = 1'b0,
    output reg         wat_o = 1'b0,
    output reg         err_o = 1'b0,
    input [ASB:0]      adr_i,
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o,

    //  The real component of the signal from the antennas.
    input              enable, // data acquisition is active
    input [MSB:0]      blocksize, // block size - 1
    output [BSB:0]     bankindex,
    output             strobe, // `antenna` data is valid
    output             swap_x, // NOTE: correlator domain
    input [NSB:0]      antenna,// the real component from each antenna

    output reg         switch = 1'b0 // NOTE: bus domain
    );

   reg                 sw = 1'b0;
   wire                go;
   wire [NSB:0]        re;
   wire [NSB:0]        im;

   assign swap_x = sw;


   //-------------------------------------------------------------------------
   //  
   //  Hardware-correlator control logic.
   //  
   //-------------------------------------------------------------------------
   //  Fill a block with visibilities, and then switch banks.
   wire [BLOCK:0]      next_blk = wrap_blk ? {(BLOCK+1){1'b0}} : blk+1;
   wire                wrap_blk = blk == blocksize;
   reg [MSB:0]         blk = {BLOCK{1'b0}};
   reg [3:0]           delays = 4'h0;

   //  Compose address:     UNIT       BLOCK       VALUE
   wire [ASB-3:0]      c_adr = {adr[ASB:10], adr[4:1], adr[6:5], adr[0]};
   wire [MSB:0]        dat_w;

   //-------------------------------------------------------------------------
   // Activate the Hilbert transform and correlators once this module has been
   // enabled, and when the first valid data arrives.
   always @(posedge clk_x)
     if (rst) begin
        sw  <= #DELAY 1'b0;
        blk <= #DELAY {BLOCK{1'b0}};
     end
     else if (go) begin
        sw  <= #DELAY delays[3] && wrap_blk; // signal an upcoming bank-swap
        blk <= #DELAY strobe ? next_blk[MSB:0] : blk;
     end
     else begin
        sw  <= #DELAY 1'b0;
        blk <= #DELAY blk;
     end

   always @(posedge clk_x)
     delays <= #DELAY {delays[2:0], !rst && strobe};


   //-------------------------------------------------------------------------
   //  Synchronise the bank-switching signal to the bus domain.
   //  NOTE: Keeps `sw_b` asserted until acknowledged.
   //-------------------------------------------------------------------------
   reg sw_x = 0, sw_d = 0;      // acquisition domain
   (* ASYNC_REG = "TRUE" *) reg sw_b = 1'b0; // bus domain

   always @(posedge clk_x)
     if (rst || strobe) sw_x <= #DELAY 1'b0;
     else if (sw)       sw_x <= #DELAY 1'b1;
     else               sw_x <= #DELAY sw_x;

   always @(posedge clk_x)
     if (rst) sw_d <= #DELAY 0;
     else     sw_d <= #DELAY sw_x && strobe;

   always @(posedge clk_i or posedge sw_d)
     if (sw_d)               sw_b <= #DELAY 1'b1;
     else if (rst || switch) sw_b <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (rst) switch <= #DELAY 1'b0;
     else     switch <= #DELAY sw_b && !switch;


   //-------------------------------------------------------------------------
   //  Bus interface.
   //-------------------------------------------------------------------------
   wire [MSB:0] dats [0:5];
   reg [5:0]    stbs = 6'h0;
   reg [2:0]    dev = 3'h0;
   reg [ASB:0]  adr = {ABITS{1'b0}};
   reg          cyc = 1'b0, we = 1'b0, bst = 1'b0; // TODO:
   wire [5:0]   acks;
   wire         we_w = 1'b0, ack_w = |acks;
   wire         upd_stb;

`ifdef __WB_CORRELATOR_CLASSIC
   wire         ack_slo;

   //  Hold the strobe constant until a transfer completes.
   assign upd_stb = stb_i && !ack_w && !ack_o;
   assign ack_slo = cyc_i && stb_i && !ack_o;

   //-------------------------------------------------------------------------
   //  Pass through (and pipeline) the bus transactions.
   always @(posedge clk_i)
     if (rst)                   cyc <= #DELAY 1'b0;
     else if (ack_slo && !cyc)  cyc <= #DELAY 1'b1;
     else if (cyc )             cyc <= #DELAY !ack_o && !ack_w;
     else                       cyc <= #DELAY cyc;

   always @(posedge clk_i)
     if (rst)        {we, bst}      <= #DELAY 2'b0;
     else if (cyc_i) {we, bst, adr} <= #DELAY {we_i, 1'b0, adr_i};

   //-------------------------------------------------------------------------
   //  Route throught the acknowledges from the correct device.
   always @(posedge clk_i)
     if (rst) ack_o <= #DELAY 1'b0;
     else     ack_o <= #DELAY ack_slo && ack_w;

`else // !`ifdef __WB_CORRELATOR_CLASSIC
   assign upd_stb = stb_i;

   //-------------------------------------------------------------------------
   //  Pass through (and pipeline) the bus transactions.
   always @(posedge clk_i)
     if (rst)                         cyc <= #DELAY 1'b0;
     else if (cyc_i && stb_i && !cyc) cyc <= #DELAY 1'b1;
     else if (cyc)                    cyc <= #DELAY |stbs;
     else                             cyc <= #DELAY cyc;

   always @(posedge clk_i)
     if (rst)        {we, bst}      <= #DELAY 0;
     else if (cyc_i) {we, bst, adr} <= #DELAY {we_i, bst_i, adr_i};

   //-------------------------------------------------------------------------
   //  Route throught the acknowledges from the correct device.
   always @(posedge clk_i)
     if (rst) ack_o <= #DELAY 1'b0;
     else     ack_o <= #DELAY ack_w;
`endif // !`ifdef __WB_CORRELATOR_CLASSIC

   //-------------------------------------------------------------------------
   //  Address decoders -- that sets the strobes for each of the sub-units.
   always @(posedge clk_i)
     if (upd_stb)
       case (adr_i[9:7])
         0: stbs <= #DELAY 6'b000001;
         1: stbs <= #DELAY 6'b000010;
         2: stbs <= #DELAY 6'b000100;
         3: stbs <= #DELAY 6'b001000;
         4: stbs <= #DELAY 6'b010000;
         5: stbs <= #DELAY 6'b100000;
         default:
           stbs <= #DELAY 6'h0;
       endcase
     else
       stbs <= #DELAY 6'h0;

   //-------------------------------------------------------------------------
   //  Assert a bus address error, for attempted accesses to unmapped address-
   //  space.
   always @(posedge clk_i)
     if (stb_i && adr_i[9:7] > 3'h5)
       err_o <= #DELAY 1'b1;
     else
       err_o <= #DELAY 1'b0;

   always @(posedge clk_i) begin
      dev   <= #DELAY adr[9:7];
      dat_o <= #DELAY cyc_i && !we_i && ack_w ? dat_w : dat_o;
   end


   //-------------------------------------------------------------------------
   //  Explicitly instantiate an 8:1 MUX for the output-data, so that it can
   //  be floor-planned.
   //-------------------------------------------------------------------------
   MUX8 #( .WIDTH(BLOCK) ) MUXDAT0
     ( .a(dats[0]),
       .b(dats[1]),
       .c(dats[2]),
       .d(dats[3]),
       .e(dats[4]),
       .f(dats[5]),
       .g({BLOCK{1'bx}}),
       .h({BLOCK{1'bx}}),
       .s(dev),
       .x(dat_w)
       );

   //-------------------------------------------------------------------------
   //  Hilbert transform to recover imaginaries.
   //-------------------------------------------------------------------------
   fake_hilbert #( .WIDTH(AXNUM) ) HILB0
     (  .clk(clk_x),
        .rst(rst),
        .en(enable),
        .d(antenna),
        .valid(go),
        .strobe(strobe), // `antenna` data is valid
        .re(re),
        .im(im)
        );


   //-------------------------------------------------------------------------
   //  
   //  There are six correlator-blocks, each connected to 12 of the 24
   //  antennas.
   //  
   //-------------------------------------------------------------------------
   //  The configuration file includes the parameters that determine which
   //  antannae connect to each of the correlators.
`include "../include/tart_pairs.v"

   (* AREA_GROUP = "cblk0" *)
   correlator_block_DSP
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS00_00),
         .PAIRS1(PAIRS00_01),
         .PAIRS2(PAIRS00_02),
         .PAIRS3(PAIRS00_03),
         .DELAY (DELAY)
         ) CXBLOCK0
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc),
         .stb_i(stbs[0]),
         .we_i (we_w),
         .bst_i(bst),
         .ack_o(acks[0]),
         .adr_i(c_adr),
         .dat_i({BLOCK{1'bx}}),
         .dat_o(dats[0]),

         .sw_i(sw),
         .en_i(go),
         .re_i(re),
         .im_i(im),

         .bank_o(bankindex)
         );

   (* AREA_GROUP = "cblk1" *)
   correlator_block_DSP
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS01_00),
         .PAIRS1(PAIRS01_01),
         .PAIRS2(PAIRS01_02),
         .PAIRS3(PAIRS01_03),
         .DELAY (DELAY)
         ) CXBLOCK1
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc),
         .stb_i(stbs[1]),
         .we_i (we_w),
         .bst_i(bst),
         .ack_o(acks[1]),
         .adr_i(c_adr),
         .dat_i({BLOCK{1'bx}}),
         .dat_o(dats[1]),

         .sw_i(sw),
         .en_i(go),
         .re_i(re),
         .im_i(im)
         );

   (* AREA_GROUP = "cblk2" *)
   correlator_block_DSP
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS02_00),
         .PAIRS1(PAIRS02_01),
         .PAIRS2(PAIRS02_02),
         .PAIRS3(PAIRS02_03),
         .DELAY (DELAY)
         ) CXBLOCK2
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc),
         .stb_i(stbs[2]),
         .we_i (we_w),
         .bst_i(bst),
         .ack_o(acks[2]),
         .adr_i(c_adr),
         .dat_i({BLOCK{1'bx}}),
         .dat_o(dats[2]),

         .sw_i(sw),
         .en_i(go),
         .re_i(re),
         .im_i(im)
         );

   (* AREA_GROUP = "cblk3" *)
   correlator_block_DSP
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS03_00),
         .PAIRS1(PAIRS03_01),
         .PAIRS2(PAIRS03_02),
         .PAIRS3(PAIRS03_03),
         .DELAY (DELAY)
         ) CXBLOCK3
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc),
         .stb_i(stbs[3]),
         .we_i (we_w),
         .bst_i(bst),
         .ack_o(acks[3]),
         .adr_i(c_adr),
         .dat_i({BLOCK{1'bx}}),
         .dat_o(dats[3]),

         .sw_i(sw),
         .en_i(go),
         .re_i(re),
         .im_i(im)
         );

   (* AREA_GROUP = "cblk4" *)
   correlator_block_SDP
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS04_00),
         .PAIRS1(PAIRS04_01),
         .PAIRS2(PAIRS04_02),
         .PAIRS3(PAIRS04_03),
         .DELAY (DELAY)
         ) CXBLOCK4
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc),
         .stb_i(stbs[4]),
         .we_i (we_w),
         .bst_i(bst),
         .ack_o(acks[4]),
         .adr_i(c_adr),
         .dat_i({BLOCK{1'bx}}),
         .dat_o(dats[4]),

         .sw_i(sw),
         .en_i(go),
         .re_i(re),
         .im_i(im)
         );

   (* AREA_GROUP = "cblk5" *)
   correlator_block_SDP
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS05_00),
         .PAIRS1(PAIRS05_01),
         .PAIRS2(PAIRS05_02),
         .PAIRS3(PAIRS05_03),
         .DELAY (DELAY)
         ) CXBLOCK5
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc),
         .stb_i(stbs[5]),
         .we_i (we_w),
         .bst_i(bst),
         .ack_o(acks[5]),
         .adr_i(c_adr),
         .dat_i({BLOCK{1'bx}}),
         .dat_o(dats[5]),

         .sw_i(sw),
         .en_i(go),
         .re_i(re),
         .im_i(im)
         );


endmodule // tart_correlator    
