`timescale 1ns/100ps
/*
 * Module      : verilog/tart_dsp.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Fake top-level of the TART DSP units, to find the "all-zero" bug.
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
 *  + 
 *  + when `bst_i` deasserts, deassert `stb[i]` the next cycle? Currently, the
 *    "tails" of a transaction are one cycle too long;
 *  + compute the exponent of the count-size;
 *  + status registers, and correctly handle overflows;
 * 
 */

`include "tartcfg.v"

module tart_fake_dsp
   #(parameter NREAD = `READ_COUNT, // Number of visibilities to read back
     parameter DELAY = 3)
   (
    input          clk_x,
    input          rst_i,

    // Wishbone-like bus interface for reading visibilities.
    input          aq_clk_i, // bus clock
    input          aq_cyc_i,
    input          aq_stb_i,
    input          aq_we_i, // writes only work for system registers
    input          aq_bst_i, // Bulk Sequential Transfer?
    output         aq_ack_o,
    input [XSB:0]  aq_blk_i, // upper address-space for registers
    input [7:0]    aq_dat_i,
    output [7:0]   aq_dat_o,

    // Debugging signals.
    output         stuck_o,
    output         limp_o,

    // The real component of the signal from the antennas.
    input          aq_enable, // data acquisition is active
    output         switching, // NOTE: bus domain
    input [MSB:0]  blocksize, // block size - 1
    input [23:0]   antenna, // the raw antenna signal

    output         newblock,
    output [MSB:0] checksum, // TODO:
    output         streamed
    );

   //-------------------------------------------------------------------------
   //
   //  TART's system-wide, Wishbone-like interconnect and peripherals.
   //
   //-------------------------------------------------------------------------
   //  Visibilities/correlator settings.
   parameter AXNUM = `NUM_ANTENNA;// Number of antennae
   parameter ACCUM = `ACCUM_BITS; // Bit-width of the accumulators
   parameter BLOCK = ACCUM;       // Maximum #bits of the block-size
   parameter MSB   = BLOCK-1;     // Data transfer MSB
   parameter TRATE = `TMUX_RATE;  // Time-multiplexing rate
   parameter COUNT = `VISB_LOG2;  // (1 << 3) - 1;
//    parameter NREAD = `READ_COUNT; // Number of visibilities to read back
   parameter RBITS = `READ_BITS;  // = ceiling{log2(NREAD)};
   parameter RSB   = RBITS-1;     // MSB of read-back address

   //  Wishbone settings.
   parameter BBITS = `WBBUS_BITS; // Bit-width of the SoC Wishbone bus
   parameter BSB   = BBITS-1;     // Bus data MSB
   parameter WSB   = BBITS-2;     // SPI -> WB bus address-width
   parameter ABITS = `WBADR_BITS; // Correlator bus address bit-width
   parameter ASB   = ABITS-1;     // Address MSB
   parameter XBITS = `BLOCK_BITS; // Bit-width of the block-counter
   parameter XSB   = XBITS-1;     // MSB of the block-counter


   assign stuck_o = stuck;
   assign limp_o  = limp;


   //-------------------------------------------------------------------------
   //     SYNCHRONISE SIGNALS FROM THE WISHBONE CLOCK DOMAIN.
   //-------------------------------------------------------------------------
   reg [MSB:0]  block_x  = 0;
   reg          enable_x = 0;

   (* ASYNC_REG = "TRUE" *) reg [MSB:0]  block_s  = 0;
   (* ASYNC_REG = "TRUE" *) reg          enable_s = 0;

   always @(posedge clk_x) begin
      block_s  <= #DELAY blocksize;
      enable_s <= #DELAY aq_enable;
   end

   always @(posedge clk_x) begin
      block_x  <= #DELAY block_s;
      enable_x <= #DELAY enable_s;
   end


   //-------------------------------------------------------------------------
   //  Add a single hardware correlator, for testing.
   //-------------------------------------------------------------------------
   correlator_SDP
     #(  .ACCUM(ACCUM),
         .SUMHI(0),
         .TBITS(TBITS),
         .PAIRS(PAIRS0),
         .DELAY(DELAY)
         ) CORRELATOR0
       ( .clk_x(clk_x),
         .rst(rst),

         .sw(clear),
         .en(en),
         .re(re),
         .im(im),
         .rd(x_rd_adr),
         .wr(x_wr_adr),

         .vld(vld),
         .vis(vis[WSB:0])
         );

   //-------------------------------------------------------------------------
   //  Explicit instantiation, because XST sometimes gets it wrong.
   //-------------------------------------------------------------------------
   //  TODO: Parameterise the number of block SRAM's.
   RAMB8X32_SDP #(.DELAY(DELAY)) VISRAM [NSRAM-1:0]
     ( .WCLK(clk_x),
       .WE(vld),
       .WADDR({block, x_wr_adr}),
       .DI(vis),
       .RCLK(clk_i),
       .CE(1'b1),
       .RADDR(adr_i[ASB:3]),
       .DO(dat)
       );
   
   //-------------------------------------------------------------------------
   //  RAM32M's implemented the nerdy way.
   //-------------------------------------------------------------------------
   //  TODO: Parameterise the accumulator width.
   parameter INIT = 64'hf0e1d2c3b4a59687;

   RAM32X6_SDP
     #( .INITA(INIT),
        .INITB(INIT),
        .INITC(INIT),
        .INITD(INIT),
        .DELAY(3)
        ) RAM32X6_SDP_COS [3:0]
       (.WCLK(clk_x),
        .WE(vld),
        .WADDR({{5-TBITS{1'b0}}, wr}),
        .DI(qcos),
        .RADDR({{5-TBITS{1'b0}}, rd}),
        .DO(dcos_w)
        );

   RAM32X6_SDP
     #( .INITA(INIT),
        .INITB(INIT),
        .INITC(INIT),
        .INITD(INIT),
        .DELAY(3)
        ) RAM32X6_SDP_SIN [3:0]
       (.WCLK(clk_x),
        .WE(vld),
        .WADDR({{5-TBITS{1'b0}}, wr}),
        .DI(qsin),
        .RADDR({{5-TBITS{1'b0}}, rd}),
        .DO(dsin_w)
        );


endmodule // tart_fake_dsp
