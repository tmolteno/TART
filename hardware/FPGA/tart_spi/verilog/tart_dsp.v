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
 *  + better tagging of the signals so that they show their clock-domains;
 *  + compute the exponent of the count-size;
 *  + status registers, and correctly handle overflows;
 *  + disable the correlators to prevent overwriting data, if that mode has
 *    been selected (TEST);
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

     //  Parameters for the bank addresses:
     parameter XBITS = 4,       // Bit-width of the block-counter
     parameter XSB   = XBITS-1, // MSB of the block-counter

     //  Correlator-bus settings:
     parameter CBITS = XBITS+RBITS,
     parameter CSB   = CBITS-1,

     //  Wishbone bit-width parameters:
     parameter BBITS = 8,       // Bit-width of the SoC Wishbone bus
     parameter BSB   = BBITS-1, // Bus data MSB
     parameter WSB   = BBITS-2, // SPI -> WB bus address-width
     parameter ABITS = 12,      // WB bus address bit-width
     parameter ASB   = ABITS-1, // Address MSB

     //  Wishbone mode parameters:
     parameter PIPED = 1,       // Wishbone SPEC B4 mode (0/1)?
     parameter CHECK = 1,       // Sanity checking when on a bus (0/1)?

     //  Simulation-only parameters:
     parameter DELAY = 3)
   ( input          clk_x,      // correlator clock
     input          clk_i,      // bus clock
     input          rst_i,      // global reset

     // Wishbone-like bus interface for reading visibilities.
     input          cyc_i,
     input          stb_i,
     input          we_i, // writes only work for system registers
     output         ack_o,
     output         wat_o,
     output         rty_o,
     output         err_o,
     input [XSB:0]  adr_i,
     input [7:0]    dat_i,
     output [7:0]   dat_o,

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
   //  WB signals to access the prefetched visibilities (that have been
   //  stored within a block SRAM).
   wire [MSB:0] v_drx, v_dtx;
   wire [RSB:0] v_adr;          // read-back address
   wire [CSB:0] c_adr;          // bank + read-back address
   wire         v_cyc, v_stb, v_we; // Master WB control signals
   wire         v_ack, v_wat, v_rty, v_err; // Slave WB response signals

   //-------------------------------------------------------------------------
   //  Signals for the dual-port SRAM's.
   wire         sram_a_ce, sram_a_we; // Port used to store visibilities
   wire [RSB:0] sram_a_ad;
   wire [3:0]   sram_a_be;
   wire [31:0]  sram_a_di, sram_a_do;

   wire         sram_b_ce, sram_b_we; // Port to read back visibilities
   wire [ASB:0] sram_b_ad;
   wire         sram_b_be;
   wire [7:0]   sram_b_di, sram_b_do;


   //-------------------------------------------------------------------------
   //
   //  TART's system-wide, Wishbone-like interconnect and peripherals.
   //
   //-------------------------------------------------------------------------
   assign stuck_o  = stuck;
   assign limp_o   = limp;


   //-------------------------------------------------------------------------
   //     
   //     SYNCHRONISE THE SIGNALS FROM THE WISHBONE CLOCK DOMAIN.
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
   (* NOMERGE = "TRUE" *) wire         overflow_x;
   (* NOMERGE = "TRUE" *) wire [XSB:0] bank;

   //  Assert overflow whenever a bank-switch causes new data to be written
   //  to the bank currently being read back.
   always @(posedge clk_i)
     overflow <= #DELAY overflow_x;
`endif


   //-------------------------------------------------------------------------
   //     
   //     CORRELATOR AND VISIBILITIES BLOCKS.
   //     
   //-------------------------------------------------------------------------
   //  When the SPI (or any other off-board I/O) interface is inactive, hold
   //  the streaming read-back module in the reset state.
   //  NOTE: This is required for the SPI module as it prefetches ahead, and
   //    then discards any unused data, causing some visibilities to be lost.
`ifdef __USE_ASYNC_FETCH
   wire               stream_reset = ~vx_stream;
`else   
   reg                stream_reset = 1'b1;

   always @(posedge clk_i)
     stream_reset <= #DELAY ~vx_stream;
`endif


   //-------------------------------------------------------------------------
   //  Streaming, visibilities-read-back logic-core.
   //-------------------------------------------------------------------------
    wb_sram_stream
     #(  .WIDTH(BBITS),         // Wishbone to SPI data bit-width
         .WORDS(NREAD << 2),    // Number of bytes to stream
         .WBITS(ABITS),         // Word-counter bit-width
         .RESET(1),             // Need to reset after each SPI transfer
         .START(0),             // Start address
         .TICKS(1),             // Read latency of the attached SRAM's
         .READ (1),             // Stream from the SRAM's
         .WRITE(0),             // Only need read access
         .USEBE(0),             // Single byte data
         .PIPED(PIPED),         // Pipelined (SPEC B4) transfers
         .ASYNC(1),             // Registered outputs
         .CHECK(CHECK),         // Bus is shared by several slaves
         .DELAY(DELAY)
         ) STREAM
       ( .clk_i(clk_i),
         .rst_i(rst_i),

         //  Wishbone (SPEC B4) interface to external I/O (SPI) device:
         .cyc_i(cyc_i),
         .stb_i(stb_i),
         .we_i (we_i),
         .ack_o(ack_o),
         .wat_o(wat_o),
         .rty_o(rty_o),
         .err_o(err_o),
         .sel_i(1'b0),
         .dat_i(dat_i),
         .dat_o(dat_o),

         //  SRAM (8-bit) read interface, for the prefetched visibilities:
         .sram_ce_o(sram_b_ce),
         .sram_we_o(sram_b_we),
         .sram_be_o(sram_b_be),
         .sram_ad_o(sram_b_ad),
         .sram_do_i(sram_b_do),
         .sram_di_o(sram_b_di),
 
         .wrapped_o(streamed)  // strobes when block has been streamed
         );


   //-------------------------------------------------------------------------
   //     VISIBILITIES READ-BACK UNIT.
   //-------------------------------------------------------------------------
   parameter BSIZE = TRATE*2;   // sine & cosine components in the SRAM's
   parameter DBITS = TBITS+1;   // distributed SRAM address bits
   parameter COUNT = 24;        // correlator number/count
   parameter NBITS = 5;

   wire [31:0] v_dat, csum_w;

   assign v_dtx    = v_dat[MSB:0];
   assign checksum = csum_w[MSB:0];

   tart_visibilities
     #(  .WIDTH(32),            // SRAM data bit-width
         .XBITS(XBITS),         // number of banks of visibilities
         .COUNT(COUNT),         // prefetch from `COUNT` correlators
         .CBITS(NBITS),         // correlator-count bit-width
         .BSIZE(BSIZE),         // number of words/correlator
         .BBITS(DBITS),         // bit-width of word address
         .DELAY(DELAY)
         ) VIZ
       ( .clk_i(clk_i),
         .rst_i(rst_i),

         //  Wishbone interconnect between correlators and prefetch SRAM:
         .cyc_o(v_cyc),
         .stb_o(v_stb),
         .we_o (v_we), 
         .ack_i(v_ack),
         .wat_i(v_wat),
         .rty_i(v_rty),
         .err_i(v_err),
         .adr_o(v_adr),
         .dat_i({{32-ACCUM{1'b0}}, v_drx}),
         .dat_o(v_dat),

         //  SRAM (32-bit) write interface, for storing the visibilities:
         .sram_ce_o(sram_a_ce),
         .sram_we_o(sram_a_we),
         .sram_be_o(sram_a_be),
         .sram_ad_o(sram_a_ad),
         .sram_do_i(sram_a_do),
         .sram_di_o(sram_a_di),

         //  Control and status signals (Wishbone domain).
         .streamed_i (streamed),  // signals that a bank has been sent
         .overwrite_i(overwrite), // overwrite when buffers full?
         .switching_i(switching), // strobes at every bank-switch (bus domain)
         .available_o(newblock),  // strobes when new block is available
         .checksum_o (csum_w),    // computed checksum of block

         //  Correlator-domain signals:
         .clk_x     (clk_x),
         .switch_x  (switch_x),
         .overflow_x(overflow_x)
         );


   //-------------------------------------------------------------------------
   //     TOP-LEVEL CORRELATORS-BLOCK FUNCTIONAL UNIT.
   //-------------------------------------------------------------------------
   //  Combined bank-index and visibility-address.
   assign c_adr = {adr_i, v_adr};

// `define __USE_OLD_CORRELATOR
`ifdef __USE_OLD_CORRELATOR
   tart_correlator
     #(  .BLOCK (BLOCK),
         .AXNUM (AXNUM),
         .ABITS (CBITS),
         .DELAY (DELAY)
         ) COR
       ( .clk_x(clk_x),         // 12x data-rate sampling clock
         .rst  (rst_i),
         .clk_i(clk_i),

         .cyc_i(v_cyc),         // the correlator connects to the read-back
         .stb_i(v_stb),         // unit for the visibilities, via this bus
         .we_i (v_we),
         .ack_o(v_ack),
         .wat_o(v_wat),
         .rty_o(v_rty),
         .err_o(v_err),
         .adr_i(c_adr),
         .dat_i(v_dtx),
         .dat_o(v_drx),

         .enable(enable_x),    // begins correlating once asserted
         .blocksize(block_x),  // number of samples per visibility sum
         .bankindex(vx_block), // the current bank-address being written to
//          .strobe(strobe),       // indicates arrival of a new sample
         .antenna(antenna),     // antenna data
         .swap_x(switch_x),     // asserts on bank-switch (sample domain)
         .switch(switching)     // asserts on bank-switch (bus domain)
         );


`else // !`ifdef __USE_OLD_CORRELATOR
   top
     #(  .ACCUM(BLOCK),         // accumulator bits
         .IBITS(AXNUM),         // data-input bits
         .ABITS(CBITS),         // correlator address bits
         .TRATE(TRATE),         // time-multiplexing rate
         .TBITS(TBITS),         // TMUX bits
         .XBITS(XBITS),         // bank-address bits
         .SEQRD(1),             // prefetch is sequential
         .DSLOW(0),             // use fast-clock settings for DSP
         .SSLOW(0),             //   and SDP correlators
         .DDUPS(0),             // no DSP duplicate address regs
         .SDUPS(1),             // use SDP duplicate address regs
         .PIPED(PIPED),         // use parent Wishbone mode
         .CHECK(1),             // on bus, so do checking
         .DELAY(DELAY)
         ) COR
       ( .clk_x(clk_x),         // 12x data-rate sampling clock
         .clk_i(clk_i),         // bus and SRAM-read clock
         .rst_i(rst_i),

         .cyc_i(v_cyc),         // the correlator connects to the read-back
         .stb_i(v_stb),         // unit for the visibilities, via this bus
         .we_i (v_we),
         .ack_o(v_ack),
         .wat_o(v_wat),
         .rty_o(v_rty),
         .err_o(v_err),
         .adr_i(c_adr),
         .dat_i(v_dtx),
         .dat_o(v_drx),

         .switching_o(switching), // asserts on bank-switch (bus domain)

         .ce_x_i  (enable_x),  // begins correlating once asserted
         .sums_x_i(block_x),   // number of samples per visibility sum
         .data_x_i(antenna),   // antenna data
         .swap_x_o(switch_x),  // bank-switch strobe
         .bank_x_o(vx_block)   // bank address
         );
`endif // !`ifdef __USE_OLD_CORRELATOR


   //-------------------------------------------------------------------------
   //  SRAM's for the prefetched data.
   //-------------------------------------------------------------------------
   //  Reorder the data so that it can be read out byte-by-byte, and without
   //  any reordering (as long as little endian is desired).
   wire [15:0] sram_a_hi, sram_a_lo;

   //  Swizzle the nibbles.
   assign sram_a_hi = {sram_a_di[31:28], sram_a_di[23:20],
                       sram_a_di[15:12], sram_a_di[7:4]};
   assign sram_a_lo = {sram_a_di[27:24], sram_a_di[19:16],
                       sram_a_di[11:8] , sram_a_di[3:0]};

   //  Stores the lower nibbles of the visibilities.
   RAMB16X16X4_TDP
     #( .DELAY(DELAY)
        ) SRAM0
       (.CLKA (clk_i),          // 16-bit write-only port
        .ENA  (sram_a_ce),
        .WEA  (sram_a_be[1:0]),
        .ADDRA(sram_a_ad),
        .DIA  (sram_a_lo),
        .DOA  (sram_a_do[15:0]),

        .CLKB (clk_i),          // 4-bit read-only port
        .ENB  (sram_b_ce),
        .WEB  (sram_b_we),
        .ADDRB(sram_b_ad),
        .DIB  (sram_b_di[3:0]),
        .DOB  (sram_b_do[3:0])
        );

   //  Stores the upper nibbles of the visibilities.
   RAMB16X16X4_TDP
     #( .DELAY(DELAY)
        ) SRAM1
       (.CLKA (clk_i),          // 16-bit write-only port
        .ENA  (sram_a_ce),
        .WEA  (sram_a_be[3:2]),
        .ADDRA(sram_a_ad),
        .DIA  (sram_a_hi),
        .DOA  (sram_a_do[31:16]),

        .CLKB (clk_i),          // 4-bit read-only port
        .ENB  (sram_b_ce),
        .WEB  (sram_b_we),
        .ADDRB(sram_b_ad),
        .DIB  (sram_b_di[7:4]),
        .DOB  (sram_b_do[7:4])
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
   reg [5:0]          count = 6'h0;
   wire [5:0]         cnext = count[4:0] + 1'b1;

   //  Assert 'stuck` if transfers are taking too long.
   always @(posedge clk_i)
     if (rst_i)
       {count, stuck} <= #DELAY 7'b0;
     else if (v_cyc && v_ack) begin
        if (!count[5])
          {count, stuck} <= #DELAY {cnext, stuck};
        else
          {count, stuck} <= #DELAY {count, 1'b1};
     end
     else if (!v_cyc)
       {count, stuck} <= #DELAY {6'h0, stuck};

   //  Clear `limp` if the device is prefetching visibilities.
   always @(posedge clk_i)
     if (rst_i)
       limp <= #DELAY 1'b1;
     else if (v_cyc && v_ack && vx_enable)
       limp <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (stuck) begin
        #10 $display("%12t: WB stuck.", $time);
        #80 $finish;
     end
`endif // !`ifdef __RELEASE_BUILD


endmodule // tart_dsp
