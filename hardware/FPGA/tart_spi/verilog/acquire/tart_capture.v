`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/tart_capture.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Time-multiplexed correlator block.
 * 
 * This version operates the Xilinx SRAM's in Simple Dual-Port mode, to make
 * better use of the available SRAM resources.
 * 
 * NOTE:
 *  + typically several of these would be attached to a common set of antenna
 *    and a system bus;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as multi-
 *    port SRAM's are used;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM resources;
 *  + ignores write-attempts, but still generates acknowledges;
 * 
 * Changelog:
 *  + 04/07/2016  --  initial file (refactored from `correlator`);
 * 
 * TODO:
 *  + setup a bunch of FROM/TO constraints for CDC;
 * 
 */

`include "tartcfg.v"

module tart_capture
  #(//  Bit-width parameters:
    parameter AXNUM = 24,       // number of antennae
    parameter MSB   = AXNUM-1,  // MSB of antenna signal
    parameter ABITS = 20,       // DRAM controller address bit-width
    parameter ASB   = ABITS-2,  // MSB of DRAM address

    //  Fake antenna-data settings:
    parameter DEBUG = 1,        // allow debug builds & options?
    parameter MULTI = 1,        // runtime fake-data options?
    parameter RNG   = 1,        // random data?
    parameter CONST = 0,        // constant data?
    parameter CDATA = 24'h0,    // constant data value

    //  Data-alignment options:
    parameter ALIGN = 0,        // (re-)align captured data?
    parameter FSIZE = 2,        // 2^FSIZE asynchronous FIFO entries
    parameter RATIO = 12,       // oversampling ratio?
    parameter RMAX  = RATIO-1,  // maximum clock-counter value
    parameter RBITS = 4,        // bit-width of clock-counter
    parameter RSB   = RBITS-1,  // MSB of clock-counter

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_i, // bus clock
    input          clk_x, // capture clock
    input          clk_e, // external clock
    output         clk_d, // delayed clock
    input          rst_i,

    //  Raw signal data input:
    input [MSB:0]  ax_dat_e_i,  // signal/external (xtal) clock domain

    //  Memory Controller Block (MCB) signals:
    output         mcb_ce_o,
    output         mcb_wr_o,
    input          mcb_rdy_i,
    output [ASB:0] mcb_adr_o,
    output [31:0]  mcb_dat_o,

    //  Antenna inputs, control-signals, and captured-data outputs:
    input          vx_ce_i,
    input          aq_ce_i,

    //  Data capture and alignment control-signals:
    input [2:0]    aq_delay_i,
    input          aq_align_i,
    output         aq_valid_o,
    output         aq_error_o,  // signal list
    input          aq_clear_i,  // clear flag, and reacquire
    input          aq_retry_i,

    //  Control-signals for fake acquistion data:
    input          aq_debug_i,
    input          aq_shift_i,
    input          aq_count_i,

    //  Fake/real acquistion data:
    output         ax_vld_x_o,  // asserted when data is valid
    output         ax_new_x_o,  // strobes when new data is available
    output [MSB:0] ax_dat_x_o,  // present the captured data

    //  Request for acquired data (stored in the DRAM):
    input          rd_req_i,
    output [2:0]   tart_state   // debug info
    );


   //-------------------------------------------------------------------------
   //  Antenna-data routing signals:
   (* IOB = "TRUE" *)
   reg [MSB:0]     ax_real_e;
   reg [MSB:0]     ax_data_e;
   wire [MSB:0]    source_w;
   reg             ax_en_e = 1'b0;
   wire            ax_ce_w, ax_en_w;
   reg             rst_t, rst_x;
   wire            ax_empty_x, ax_full_e;
   reg             aq_empty_n;

   //-------------------------------------------------------------------------
   //  Acquistion-block signals:
   //  TODO: Which domains do these belong to?
   wire [MSB:0]    aq_read_data;
   wire [8:0]      aq_bb_rd_address;
   wire [8:0]      aq_bb_wr_address;

   //-------------------------------------------------------------------------
   //  Fake/debug antenna-data signals:
   wire            debug_w;     // bus domain
   reg             rst_s, rst_e; // bus -> signal domain synchronisers
   reg             en_fake_s = 1'b0, go_fake_s = 1'b0;
   reg             aq_debug_e, aq_shift_e, aq_count_e;
   wire [MSB:0]    ax_fake_e;
   reg             en_fake_e = 1'b0;
   wire            go_fake_e, fifo_ce_e, debug_e;

   //-------------------------------------------------------------------------
   //  Sample-/correlator- domain signals.
   //   (Generated or used by the data-capture and alignment unit)
   //-------------------------------------------------------------------------
   wire [MSB:0]    aq_clean_x;

   //  Synchroniser bus-domain signals:
   reg             aq_align_s = 1'b0, aq_clear_s = 1'b0, aq_retry_s = 1'b0;
   reg             aq_valid_s = 1'b0, aq_error_s = 1'b0;

   //  Correlator-domain signals:
   reg             aq_align_x = 1'b0, aq_clear_x = 1'b0, aq_retry_x = 1'b0;
   reg             aq_valid_r = 1'b0, aq_error_r = 1'b0;
   wire            aq_ready_x, aq_valid_x, aq_error_x;

   //-------------------------------------------------------------------------
   //  CDC signals for the unaligned data:
   reg [MSB:0]     dat_x;
   reg [RSB:0]     cnt_x = {RBITS{1'b0}};
   reg             new_x = 1'b0, vld_x = 1'b0;
   wire            max_x;
   wire [MSB:0]    dat_w;
   wire [RBITS:0]  nxt_x;
   reg             aq_valid_t;


   //-------------------------------------------------------------------------
   //  Data-capture assignments.
   //-------------------------------------------------------------------------
   assign aq_valid_o = ALIGN ? aq_valid_s : aq_valid_t;
   assign aq_error_o = ALIGN ? aq_error_s : 1'b0;

   //-------------------------------------------------------------------------
   // Antenna sources MUX, for choosing real data or fake data.
   assign source_w   = debug_e ? ax_fake_e : ax_real_e;
   assign ax_en_w    = debug_w ? go_fake_s : ax_ce_w;
   assign fifo_ce_e  = debug_w ? go_fake_e : ax_en_e;

   assign ax_ce_w    = aq_ce_i || vx_ce_i;
   assign debug_w    = DEBUG && aq_debug_i;
   assign debug_e    = DEBUG && aq_debug_e;


   //-------------------------------------------------------------------------
   //  Correlator-domain signal assignments.
   //-------------------------------------------------------------------------
   assign ax_vld_x_o = ALIGN ? aq_valid_x : vld_x;
   assign ax_new_x_o = ALIGN ? aq_ready_x : new_x;
   assign ax_dat_x_o = ALIGN ? aq_clean_x : dat_x;

   //-------------------------------------------------------------------------
   //  CDC signal-assignments for unaligned data.
   assign nxt_x  = cnt_x + 1;
   assign max_x  = cnt_x == RMAX;


   //-------------------------------------------------------------------------
   //
   //  DATA CAPTURE.
   //
   //-------------------------------------------------------------------------
   //  Synchronise the bus-domain reset into the signal-domain.
   always @(posedge clk_e)
     {rst_e, rst_s} <= #DELAY {rst_s, rst_i};

   //  Synchronise the bus-domain reset into the correlator-domain.
   always @(posedge clk_x)
     {rst_x, rst_t} <= #DELAY {rst_t, rst_i};

   //  Compute the bus-domain, valid-data output.
   always @(posedge clk_i)
     aq_valid_t <= #DELAY vld_x;


   //-------------------------------------------------------------------------
   //  Basic, data-capture register implementation.
   //-------------------------------------------------------------------------
   //  TODO: Use a more robust data-capture circuit.
   always @(posedge clk_e)
     if (rst_e)
       ax_en_e <= #DELAY 1'b0;
     else begin
        ax_en_e    <= #DELAY ax_en_w;
     end

   //-------------------------------------------------------------------------
   //  Enable the fake-data generator when in debug-mode.
   always @(posedge clk_i)
     if (rst_i)
       en_fake_s <= #DELAY 1'b0;
     else
       en_fake_s <= #DELAY ax_ce_w && debug_w;

   //-------------------------------------------------------------------------
   //  CDC for crossing from the bus domain to the signal domain, or vice
   //  versa.
   always @(posedge clk_e) begin
      en_fake_e  <= #DELAY en_fake_s;
      aq_debug_e <= #DELAY aq_debug_i;
      aq_shift_e <= #DELAY aq_shift_i;
      aq_count_e <= #DELAY aq_count_i;
      ax_real_e  <= #DELAY ax_dat_e_i; // typically registered in IOB's
      ax_data_e  <= #DELAY source_w;   // fake or real
   end

   //  Synchronise the fake-data valid signal.
   always @(posedge clk_i)
     go_fake_s <= #DELAY go_fake_e;


   //-------------------------------------------------------------------------
   //  Dequeue samples from the CDC FIFO.
   //-------------------------------------------------------------------------
   always @(posedge clk_x)
     if (rst_x || max_x)
       cnt_x <= #DELAY {RBITS{1'b0}};
     else if (!ax_empty_x)
       cnt_x <= #DELAY nxt_x[RSB:0];

   always @(posedge clk_x)
     new_x <= #DELAY max_x;

   //  Output data is valid so long as the FIFO doesn't underflow.
   always @(posedge clk_x)
     if (rst_x || ax_empty_x && new_x)
       vld_x <= #DELAY 1'b0;
     else if (!ax_empty_x && new_x)
       vld_x <= #DELAY 1'b1;
     else
       vld_x <= #DELAY vld_x;

   always @(posedge clk_x)
     if (new_x)
       dat_x <= #DELAY dat_w;


   //-------------------------------------------------------------------------
   //  Clock Domain Crossing (CDC) for the alignment signals.
   //-------------------------------------------------------------------------
   //  Use synchronisers to cross from the bus to the correlator domain.
   //  NOTE: The paths between these paired flip-flops needs to be as short as
   //    possible; e.g., using FROM/TO constraints.
   always @(posedge clk_i) begin
      // bus- to correlator- domain synchronisers:
      aq_align_s <= #DELAY ALIGN ? aq_align_i : 1'b0;
      aq_clear_s <= #DELAY ALIGN ? aq_clear_i : 1'b0;
      aq_retry_s <= #DELAY ALIGN ? aq_retry_i : 1'b0;

      // correlator- to bus- domain synchronisers:
      aq_valid_s <= #DELAY aq_valid_r;
      aq_error_s <= #DELAY aq_error_r;
   end

   //-------------------------------------------------------------------------
   //  High-speed end of the CDC.
   always @(posedge clk_x) begin
      // bus- to correlator- domain synchronisers:
      aq_align_x <= #DELAY aq_align_s;
      aq_clear_x <= #DELAY aq_clear_s;
      aq_retry_x <= #DELAY aq_retry_s;

      // correlator- to bus- domain synchronisers:
      aq_valid_r <= #DELAY aq_valid_x;
      aq_error_r <= #DELAY aq_error_x;
   end


`ifndef __RELEASE_BUILD   
   //-------------------------------------------------------------------------
   //  Fake data generation circuit, for debugging.
   fake_telescope
     #(  .WIDTH(AXNUM),
         .MULTI(MULTI),
         .RNG  (RNG),
         .CONST(CONST),
         .CDATA(CDATA),
         .DELAY(DELAY)
         ) FAKE_TART0
       ( .clock_i (clk_e),
         .reset_i (rst_e),
         .enable_i(en_fake_e),
         .shift_i (aq_shift_e),
         .count_i (aq_count_e),
         .valid_o (go_fake_e),
         .data_o  (ax_fake_e)
         );
`endif


   //-------------------------------------------------------------------------
   //  Clock Domain Crossing (CDC) for the unaligned antenna signals.
   //-------------------------------------------------------------------------
   //  NOTE: For multi-bit signals to cross from the signal domain (typically
   //    16.368 MHz) to the correlator domain (12x 16.368 MHz, and without
   //    a known/fixed phase relationship), then either this FIFO should be
   //    used, or the capture & alignment unit, below.
   afifo_gray #( .WIDTH(AXNUM), .ABITS(FSIZE) ) XFIFO
     ( .rd_clk_i (clk_x),
       .rd_en_i  (new_x),
       .rd_data_o(dat_w),
       
       .wr_clk_i (clk_e),
       .wr_en_i  (fifo_ce_e),
       .wr_data_i(ax_data_e),

       .rst_i    (rst_e),
       .rempty_o (ax_empty_x),
       .wfull_o  (ax_full_e)
       );


   //-------------------------------------------------------------------------
   //  Data-capture and alignment unit.
   //-------------------------------------------------------------------------
   capture
     #( .WIDTH(AXNUM),
        .RATIO(RATIO),
        .RBITS(RBITS),
        .DELAY(DELAY)
        ) ALIGN0
       (
        .clock_i(ALIGN ? clk_x : 1'b0),
        .reset_i(ALIGN ? rst_x : 1'b0),
        .align_i(aq_align_x),
        .ready_o(aq_ready_x),
        .valid_o(aq_valid_x),
        .error_o(aq_error_x),
        .clear_i(aq_clear_x),
        .acks_i ({AXNUM{aq_retry_x}}),
        .data_i (ax_data_e),    // raw, signal-domain data
        .data_o (aq_clean_x)    // aligned, sample-domain data
        );


   //-------------------------------------------------------------------------
   //
   //  ACQUISITION BLOCK
   //
   //-------------------------------------------------------------------------
`ifdef __USE_ACQUISITION

   //-------------------------------------------------------------------------
   //  Programmable delay.
   //-------------------------------------------------------------------------
   //  TODO: OBSOLETE?
	 delay_data_sampling_clk DELAY_RX_CLK
     ( .fast_clk(clk_i),
		   .data_sample_delay(aq_delay_i),
		   .slow_clk(clk_d)
	     );


   //-------------------------------------------------------------------------
   //  FIFO for temporary buffering.
   //-------------------------------------------------------------------------
   block_buffer AQ_BB
     ( .clk          (clk_i),
       .read_data    (aq_read_data),
       .write_data   (ax_data_e),
       .write_address(aq_bb_wr_address),
       .read_address (aq_bb_rd_address)
       );


   //-------------------------------------------------------------------------
   //      Storage block controller.
   //-------------------------------------------------------------------------
   fifo_sdram_fifo_scheduler
     #(.SDRAM_ADDRESS_WIDTH(ABITS))
   SCHEDULER0
     ( .clk  (clk_d),
       .clk6x(clk_i),
       .rst  (rst_i),

       .aq_bb_wr_address(aq_bb_wr_address),
       .aq_bb_rd_address(aq_bb_rd_address),
       .aq_read_data    (aq_read_data),

       .spi_start_aq(aq_ce_i),
       .spi_buffer_read_complete(rd_req_i),

       .cmd_enable (mcb_ce_o),
       .cmd_wr     (mcb_wr_o),
       .cmd_ready  (mcb_rdy_i),
       .cmd_address(mcb_adr_o),
       .cmd_data_in(mcb_dat_o),

       .tart_state (tart_state)
       );


`else // !`ifdef __USE_ACQUISITION
   //  Only other use is to drive a test-pin.
   assign clk_d     = 1'b0;

   //  Drive zeroes onto the unused pins:
   assign mcb_ce_o  = 1'b0;
   assign mcb_wr_o  = 1'b0;
   assign mcb_adr_o = {(ABITS-1){1'b0}};
   assign mcb_dat_o = {32{1'b0}};

`endif // !`ifdef __USE_ACQUISITION


endmodule // tart_capture
