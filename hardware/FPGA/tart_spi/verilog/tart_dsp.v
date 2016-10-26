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
 * The visibilities are computed by 24 correlators, each with 12x time-
 * multiplexing, so that 576 correlations are performed for each antenna
 * sample (with default settings).
 * 
 * NOTE:
 *  + currently specialied for a 24 antenna setup;
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
 *  + disable the correlators to prevent overwriting data, if that mode has
 *    been selected;
 *  + upgrade to WB SPEC B4;
 * 
 */

`include "tartcfg.v"

module tart_dsp
   #(//  Visibilities/correlator settings:
     parameter AXNUM = 24,      // Number of antennae
     parameter ACCUM = 24,      // Bit-width of the accumulators
     parameter BLOCK = ACCUM,   // Maximum #bits of the block-size
     parameter MSB   = BLOCK-1, // Data transfer MSB
     parameter TRATE = 12,      // Time-multiplexing rate
     parameter TBITS = 4,       // Number of bits for TMUX counter

     //  Visibilities-bus settings:
     parameter NREAD = 576,     // Number of visibilities to read back
     parameter RBITS = 10,      // = ceiling{log2(NREAD)}
     parameter RSB   = RBITS-1, // MSB of read-back address
     parameter XBITS = 4,       // Bit-width of the block-counter
     parameter XSB   = XBITS-1, // MSB of the block-counter

     //  Correlator-bus settings:
     parameter CBITS = XBITS+RBITS,
     parameter CSB   = CBITS-1,

     //  Wishbone settings:
     parameter BBITS = 8,       // Bit-width of the SoC Wishbone bus
     parameter BSB   = BBITS-1, // Bus data MSB
     parameter WSB   = BBITS-2, // SPI -> WB bus address-width
     parameter ABITS = 12,      // WB bus address bit-width
     parameter ASB   = ABITS-1, // Address MSB

     parameter DELAY = 3)
   (
    input          clk_x,
    input          rst_i,

    // Wishbone-like bus interface for reading visibilities.
    input          aq_clk_i, // bus clock
    input          aq_cyc_i,
    input          aq_stb_i,
    input          aq_we_i, // writes only work for system registers
`ifndef __WB_SPEC_B4
    input          aq_bst_i, // Bulk Sequential Transfer?
`endif
    output         aq_ack_o,
    output         aq_wat_o,
    output         aq_rty_o,
    output         aq_err_o,
    input [XSB:0]  aq_adr_i,
    input [7:0]    aq_dat_i,
    output [7:0]   aq_dat_o,

    // Debugging signals.
    output         stuck_o,
    output         limp_o,

    // The real component of the signal from the antennas.
    input          vx_enable, // correlation is active
    input          vx_stream, // is the data being streamed out?
    output [XSB:0] vx_block,
    input          overwrite, // overwrite when buffers full?
    output         switching, // NOTE: bus domain
    input [MSB:0]  blocksize, // block size - 1
    input [23:0]   antenna, // the raw antenna signal

    (* ASYNC_REG = "TRUE" *)
    output reg     overflow = 1'b0,
    output         newblock,
    output [MSB:0] checksum, // TODO:
    output         streamed
    );

   //-------------------------------------------------------------------------
   //
   //  TART's system-wide, Wishbone-like interconnect and peripherals.
   //
   //-------------------------------------------------------------------------
   assign stuck_o  = stuck;
   assign limp_o   = limp;


   //-------------------------------------------------------------------------
   //     
   //     SYNCHRONISE SIGNALS FROM THE WISHBONE CLOCK DOMAIN.
   //     
   //-------------------------------------------------------------------------
   reg [MSB:0]  block_x  = 0;
   reg          enable_x = 1'b0;

   (* ASYNC_REG = "TRUE" *) reg [MSB:0]  block_s  = 0;
   (* ASYNC_REG = "TRUE" *) reg          enable_s = 0;

   always @(posedge clk_x) begin
      block_s  <= #DELAY blocksize;
      enable_s <= #DELAY vx_enable;
   end

   always @(posedge clk_x) begin
      block_x  <= #DELAY block_s;
      enable_x <= #DELAY enable_s;
   end


   //-------------------------------------------------------------------------
   //     
   //     BANK OVERWRITE DETECTION LOGIC.
   //     
   //-------------------------------------------------------------------------
`ifdef  __USE_OVERFLOW_DETECTION
   wire               switch_x;
   (* KEEP      = "TRUE" *) wire               overflow_x;
   (* KEEP      = "TRUE" *) wire [XSB:0]       bank;

   //  Assert overflow whenever a bank-switch causes new data to be written
   //  to the bank currently being read back.
   always @(posedge aq_clk_i)
     overflow <= #DELAY overflow_x;
`endif


   //-------------------------------------------------------------------------
   //     
   //     CORRELATOR AND VISIBILITIES BLOCKS.
   //     
   //-------------------------------------------------------------------------
   //  WB signals to access the prefetched visibilities (that have been
   //  stored within a block SRAM).
   wire [BSB:0] v_drx, v_dtx;
   wire [ASB:0] v_adr;
   wire         v_cyc, v_stb, v_bst, v_we;  // Master WB control signals
   wire         v_ack, v_wat, v_rty, v_err; // Slave WB response signals

   //  WB signals between the visibilities-prefetch logic-core and the block
   //  of correlators.
   wire [CSB:0] c_adr;
   wire [MSB:0] c_drx, c_dtx;
   wire         c_cyc, c_stb, c_bst, c_we;
   wire         c_ack, c_wat, c_rty, c_err;

   //-------------------------------------------------------------------------
   //  When the SPI (or any other off-board I/O) interface is inactive, hold
   //  the streaming read-back module in the reset state.
   //  NOTE: This is required for the SPI module as it prefetches ahead, and
   //    then discards any unused data, causing some visibilities to be lost.
`ifdef __USE_ASYNC_FETCH
   wire               stream_reset = ~vx_stream;
`else   
   reg                stream_reset = 1'b1;

   always @(posedge aq_clk_i)
     stream_reset <= #DELAY ~vx_stream;
`endif


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
         .rst_i(stream_reset),

         .m_cyc_o(v_cyc),     // this bus prefetches visibilities, and
         .m_stb_o(v_stb),     // sequentially
         .m_we_o (v_we),
`ifndef __WB_SPEC_B4
         .m_bst_o(v_bst),
`endif
         .m_ack_i(v_ack),
         .m_wat_i(v_wat),
         .m_rty_i(v_rty),
         .m_err_i(v_err),
         .m_adr_o(v_adr),
         .m_dat_i(v_drx),
         .m_dat_o(v_dtx),

         .s_cyc_i(aq_cyc_i),  // visibilities are streamed from here to the
         .s_stb_i(aq_stb_i),  // SPI module
         .s_we_i (aq_we_i),
`ifndef __WB_SPEC_B4
         .s_bst_i(1'b0),
`endif
         .s_ack_o(aq_ack_o),
         .s_wat_o(aq_wat_o),
         .s_rty_o(aq_rty_o),
         .s_err_o(aq_err_o),
         .s_dat_i(8'bx),
         .s_dat_o(aq_dat_o),

         .wrapped(streamed)   // strobes when block has been streamed
         );


   //-------------------------------------------------------------------------
   //     VISIBILITIES READ-BACK UNIT.
   //-------------------------------------------------------------------------
   tart_visibilities
     #(  .BLOCK (BLOCK),
         .COUNT (NREAD),
         .TRATE (TRATE),
         .TBITS (TBITS),
         .DELAY (DELAY)
         ) VIZ
       ( .clk_i(aq_clk_i),
         .rst_i(rst_i),

         //  Wishbone interconnect between correlators and prefetch SRAM.
         .cyc_i(v_cyc),         // this bus accesses the prefetched bank of
         .stb_i(v_stb),         // visibilities -- which are prefetched after
         .we_i (v_we),          // every bank-switch
`ifndef __WB_SPEC_B4
         .bst_i(v_bst),
`endif
         .ack_o(v_ack),
         .wat_o(v_wat),
         .rty_o(v_rty),
         .err_o(v_err),
         .adr_i({aq_adr_i, v_adr}),
         .byt_i(v_dtx),
         .byt_o(v_drx),

         //  Wishbone interconnect to external I/O interface (e.g., SPI).
         .cyc_o(c_cyc),         // master interface that connects to the
         .stb_o(c_stb),         // correlators, to read back their computed
         .we_o (c_we),          // visibilities
`ifndef __WB_SPEC_B4
         .bst_o(c_bst),
`endif
         .ack_i(c_ack),
         .wat_i(c_wat),
         .rty_i(c_rty),
         .err_i(c_err),
         .adr_o(c_adr),
         .dat_i(c_drx),
         .dat_o(c_dtx),

         //  Control and status signals (Wishbone domain).
         .streamed (streamed),  // signals that a bank has been sent
         .overwrite(overwrite), // overwrite when buffers full?
         .switching(switching), // strobes at every bank-switch (bus domain)
         .available(newblock),  // strobes when new block is available
         .checksum (checksum),  // computed checksum of block

         //  Correlator-domain signals:
         .clk_x     (clk_x),
         .switch_x  (switch_x),
         .overflow_x(overflow_x)
         );


   //-------------------------------------------------------------------------
   //     TOP-LEVEL CORRELATORS-BLOCK FUNCTIONAL UNIT.
   //-------------------------------------------------------------------------
   tart_correlator
     #(  .BLOCK (BLOCK),
         .AXNUM (AXNUM),
         .ABITS (CBITS),
         .DELAY (DELAY)
         ) COR
       ( .clk_x(clk_x),         // 12x data-rate sampling clock
         .rst  (rst_i),
         .clk_i(aq_clk_i),

         .cyc_i(c_cyc),         // the correlator connects to the read-back
         .stb_i(c_stb),         // unit for the visibilities, via this bus
         .we_i (c_we),
`ifndef __WB_SPEC_B4
         .bst_i(c_bst),
`endif
         .ack_o(c_ack),
         .wat_o(c_wat),
         .rty_o(c_rty),
         .err_o(c_err),
         .adr_i(c_adr),
         .dat_i(c_dtx),
         .dat_o(c_drx),

         .enable(enable_x),    // begins correlating once asserted
         .blocksize(block_x),  // number of samples per visibility sum
         .bankindex(vx_block), // the current bank-address being written to
//          .strobe(strobe),       // indicates arrival of a new sample
         .antenna(antenna),     // antenna data
         .swap_x(switch_x),
         .switch(switching)     // asserts on bank-switch (sample domain)
         );


   //-------------------------------------------------------------------------
   //     
   //     DEBUGGING STUFF.
   //     
   //-------------------------------------------------------------------------
`ifdef __RELEASE_BUILD
   wire               stuck = 1'b0;
   wire               limp  = 1'b0;

`else
   reg                stuck = 1'b0;
   reg                limp  = 1'b1;
   reg [5:0]          count = 6'b0;
   wire [5:0]         cnext = count[4:0] + 1'b1;

   //  Assert 'stuck` if transfers are taking too long.
   always @(posedge aq_clk_i)
     if (rst_i)
       {count, stuck} <= #DELAY 7'b0;
     else if (c_cyc && c_ack) begin
        if (!count[5])
          {count, stuck} <= #DELAY {cnext, stuck};
        else
          {count, stuck} <= #DELAY {count, 1'b1};
     end
     else if (!c_cyc)
       {count, stuck} <= #DELAY {6'b0, stuck};

   //  Clear `limp` if the device is prefetching visibilities.
   always @(posedge aq_clk_i)
     if (rst_i)
       limp <= #DELAY 1'b1;
     else if (c_cyc && c_ack && vx_enable)
       limp <= #DELAY 1'b0;

   always @(posedge aq_clk_i)
     if (stuck) begin
        #10 $display("%12t: WB stuck.", $time);
        #80 $finish;
     end
`endif // !`ifdef __RELEASE_BUILD


endmodule // tart_dsp
