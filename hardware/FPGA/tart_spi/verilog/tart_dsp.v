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
 * Top-level of the TART DSP units, which contain the time-multiplexed block
 * of correlator-blocks.
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

module tart_dsp
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
   //     
   //     CORRELATOR AND VISIBILITIES BLOCKS.
   //     
   //-------------------------------------------------------------------------
   //  WB signals to access the prefetched visibilities (that have been
   //  stored within a block SRAM).
   wire [BSB:0] v_drx, v_dtx;
   wire [ASB:0] v_adr;
   wire         v_cyc, v_stb, v_bst, v_we, v_ack, v_wat;

   //  WB signals between the visibilities-prefetch logic-core and the block
   //  of correlators.
   wire [RSB+XBITS:0] c_adr;
   wire [MSB:0]       c_drx, c_dtx;
   wire               c_cyc, c_stb, c_bst, c_we, c_ack;
   wire               c_wat = 1'b0;

   //  Keep (it's name the same), so that it can be exempted from timing.
   (* KEEP = "TRUE" *) wire c_err;

   //-------------------------------------------------------------------------
   //  Streaming, visibilities-read-back logic-core.
   //-------------------------------------------------------------------------
   wb_stream
     #(  .WIDTH (BBITS),
         .WORDS (NREAD << 2),
         .WBITS (ABITS),
         .DELAY (DELAY)
         ) STREAM
       ( .clk_i(aq_clk_i),
         .rst_i(rst_i),

         .m_cyc_o(v_cyc),       // this bus prefetches visibilities, and
         .m_stb_o(v_stb),       // sequentially
         .m_we_o (v_we),
         .m_bst_o(v_bst),
         .m_ack_i(v_ack),
         .m_wat_i(v_wat),
         .m_adr_o(v_adr),
         .m_dat_i(v_drx),
         .m_dat_o(v_dtx),

         .s_cyc_i(aq_cyc_i),       // visibilities are streamed from here to the
         .s_stb_i(aq_stb_i),       // SPI module
         .s_we_i (aq_we_i),
         .s_bst_i(1'b0),
         .s_ack_o(aq_ack_o),
         .s_wat_o(),
         .s_dat_i(8'bx),
         .s_dat_o(aq_dat_o),

         .wrapped(streamed)     // strobes when block has been streamed
         );


   //-------------------------------------------------------------------------
   //     VISIBILITIES READ-BACK UNIT.
   //-------------------------------------------------------------------------
   tart_visibilities
     #(  .BLOCK (BLOCK),
         .COUNT (NREAD),
         .TRATE (TRATE),
         .DELAY (DELAY)
         ) VISIBILITIES
       ( .clk_i(aq_clk_i),
         .rst_i(rst_i),

         .cyc_i(v_cyc),         // this bus accesses the prefetched bank of
         .stb_i(v_stb),         // visibilities -- which are prefetched after
         .we_i (v_we),          // every bank-switch
         .bst_i(v_bst),
         .ack_o(v_ack),
         .wat_o(v_wat),
         .adr_i({aq_blk_i, v_adr}),
         .byt_i(v_dtx),
         .byt_o(v_drx),

         .cyc_o(c_cyc),         // master interface that connects to the
         .stb_o(c_stb),         // correlators, to read back their computed
         .we_o (c_we),          // visibilities
         .bst_o(c_bst),
         .ack_i(c_ack),
         .err_i(c_err),
         .wat_i(c_wat),
         .adr_o(c_adr),
         .dat_i(c_drx),
         .dat_o(c_dtx),

         .switching(switching), // strobes at every bank-switch (bus domain)
         .available(newblock),  // strobes when new block is available
         .checksum (checksum)   // computed checksum of block
         );


   //-------------------------------------------------------------------------
   //     TOP-LEVEL CORRELATORS-BLOCK FUNCTIONAL UNIT.
   //-------------------------------------------------------------------------
   tart_correlator
     #(  .BLOCK (BLOCK),
         .AXNUM (AXNUM),
         .ABITS (RBITS + XBITS),
         .DELAY (DELAY)
         ) CORRELATOR
       ( .clk_x(clk_x),         // 12x data-rate sampling clock
         .rst  (rst_i),
         .clk_i(aq_clk_i),

         .cyc_i(c_cyc),         // the correlator connects to the read-back
         .stb_i(c_stb),         // unit for the visibilities, via this bus
         .we_i (c_we),
         .bst_i(c_bst),
         .ack_o(c_ack),
         .err_o(c_err),
         .adr_i(c_adr),
         .dat_i(c_dtx),
         .dat_o(c_drx),

         .enable(enable_x),     // begins correlating once asserted
         .blocksize(block_x),   // number of samples per visibility sum
//          .strobe(strobe),       // indicates arrival of a new sample
         .antenna(antenna),     // antenna data
         .switch(switching)     // asserts on bank-switch (sample domain)
         );


endmodule // tart_dsp
