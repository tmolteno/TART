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
 */

`include "tartcfg.v"

module tart_capture
  #(//  Bit-width parameters:
    parameter AXNUM = 24,
    parameter MSB   = AXNUM-1,
    parameter ABITS = 20,
    parameter ASB   = ABITS-2,

    //  Fake antenna-data settings:
    parameter DEBUG = 1,        // allow debug builds & options?
    parameter MULTI = 1,        // runtime fake-data options?
    parameter RNG   = 1,
    parameter CONST = 0,
    parameter CDATA = 24'h0,

    //  Data-alignment options:
    parameter ALIGN = 0,        // (re-)align captured data?
    parameter RATIO = 12,       // oversampling ratio?
    parameter RBITS = 4,        // bit-width of clock-counter

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_i, // bus clock
    input          clk_x, // capture clock
    input          clk_e, // external clock
    output         clk_d, // delayed clock
    input          rst_i,

    //  Memory Controller Block (MCB) signals:
    output         mcb_ce_o,
    output         mcb_wr_o,
    input          mcb_rdy_i,
    output [ASB:0] mcb_adr_o,
    output [31:0]  mcb_dat_o,

    //  Antenna inputs, control-signals, and captured-data outputs:
    input          vx_ce_i,
    input          aq_ce_i,
    input [2:0]    aq_delay_i,

    //  Control-signals for fake acquistion data:
    input          aq_debug_i,
    input          aq_shift_i,
    input          aq_count_i,

    //  Fake/real acquistion data:
    output         aq_valid_o,
    input [MSB:0]  ax_data_i,
    output [MSB:0] ax_data_o,
    input          rd_req_i,

    //  Debug info:
    output [2:0]   tart_state
    );


   //-------------------------------------------------------------------------
   //  Antenna-data routing signals:
   (* IOB = "TRUE" *)
   reg [MSB:0]     ax_real;
   reg [MSB:0]     ax_data;
   wire [MSB:0]    ax_data_w;
   reg             ax_en = 1'b0;
   wire            ax_ce_w, ax_en_w;

   //-------------------------------------------------------------------------
   //  Acquistion-block signals:
   wire [MSB:0]    aq_read_data;
   wire [8:0]      aq_bb_rd_address;
   wire [8:0]      aq_bb_wr_address;

   //-------------------------------------------------------------------------
   //  Fake/debug antenna-data signals:
   reg             en_fake = 1'b0;
   wire [MSB:0]    ax_fake;
   wire            go_fake, debug_w;

   //-------------------------------------------------------------------------
   //  Sample-/correlator- domain signals:
   //   (Generated or used by the data-capture and alignment unit)
   //  TODO:
   wire [MSB:0] data_x, aq_retry_x;
   wire         align_x, ready_x, valid_x, error_x;


   //-------------------------------------------------------------------------
   //  Data-capture assignments.
   //-------------------------------------------------------------------------
   assign aq_valid_o = ax_en;
   assign ax_data_o  = ax_data;

   //-------------------------------------------------------------------------
   // Antenna sources MUX, for choosing real data or fake data
   assign ax_data_w  = debug_w ? ax_fake : ax_real;
   assign ax_en_w    = debug_w ? go_fake : ax_ce_w;

   assign debug_w    = DEBUG && aq_debug_i;
   assign ax_ce_w    = aq_ce_i || vx_ce_i;


   //-------------------------------------------------------------------------
   //
   //  DATA CAPTURE.
   //
   //-------------------------------------------------------------------------

   //-------------------------------------------------------------------------
   //  Basic, data-capture register implementation.
   //-------------------------------------------------------------------------
   //  TODO: Use a more robust data-capture circuit.
   always @(posedge clk_e)
     if (rst_i)
       ax_en   <= #DELAY 1'b0;
     else begin 
        ax_en   <= #DELAY ax_en_w;
        ax_real <= #DELAY ax_data_i;
        ax_data <= #DELAY ax_data_w;
     end


   //-------------------------------------------------------------------------
   //  Enable the fake-data generator when in debug-mode.
   always @(posedge clk_i)
     if (rst_i)
       en_fake <= #DELAY 1'b0;
     else
       en_fake <= #DELAY ax_ce_w && debug_w;
   

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
         .reset_i (rst_i),
         .enable_i(en_fake),
         .shift_i (aq_shift_i),
         .count_i (aq_count_i),
         .valid_o (go_fake),
         .data_o  (ax_fake)
         );
`endif


   //-------------------------------------------------------------------------
   //  Data-capture and alignment unit.
   //-------------------------------------------------------------------------

   assign retry = {AXNUM{1'b0}}; // TODO
   assign align = ALIGN ? aq_ce_i : 1'b0;

   capture
     #( .WIDTH(AXNUM),
        .RATIO(RATIO),
        .RBITS(RBITS),
        .DELAY(DELAY)
        ) ALIGN0
       (
        .clock_i(ALIGN ? clk_x : 1'b0),
        .reset_i(ALIGN ? rst_i : 1'b0),
        .align_i(aq_align_x),
        .ready_o(aq_ready_x),
        .valid_o(aq_valid_x),
        .error_o(aq_error_x),
        .clear_i(aq_clear_x),
        .acks_i (retry),
        .data_i (data_e),       // signal-domain data
        .data_o (data_x)        // sample-domain data
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
       .write_data   (ax_data),
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
