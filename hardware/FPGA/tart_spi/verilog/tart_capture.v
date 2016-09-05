`timescale 1ns/100ps
/*
 * Module      : verilog/tart_capture.v
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
  #(parameter AXNUM = 24,
    parameter MSB   = AXNUM-1,
    parameter ABITS = 20,
    parameter ASB   = ABITS-2,
    parameter RNG   = 1,
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
    input          aq_ce_i,
    input [2:0]    aq_delay_i,
    input          aq_debug_i,
//     output         aq_valid_o,
    input [MSB:0]  ax_data_i,
    output [MSB:0] ax_data_o,
    input          rd_req_i,

    //  Debug info:
    output [2:0]   tart_state
    );


// `ifdef __USE_OLD_CAPTURE   
   wire [2:0]      aq_delay;

   (* IOB = "TRUE" *)
   reg [MSB:0]     ax_real;
   reg [MSB:0]     ax_data;
   wire [MSB:0]    ax_fake;
   wire [MSB:0]    ax_data_w;


   //-------------------------------------------------------------------------
   //     DATA CAPTURE
   //-------------------------------------------------------------------------
   // Antenna source MUX, for choosing real data or fake data
   assign ax_data_w = aq_debug_i ? ax_fake : ax_real;
   assign ax_data_o = ax_data;

   //  TODO: Use a more robust data-capture circuit.
   always @(posedge clk_e) begin
//    always @(posedge clk_i) begin
      ax_real <= #DELAY ax_data_i;
      ax_data <= #DELAY ax_data_w;
   end


   //-------------------------------------------------------------------------
   //     ACQUISITION BLOCK
   //-------------------------------------------------------------------------
   wire [MSB:0] aq_write_data;
   wire [MSB:0] aq_read_data;
   wire [8:0]   aq_bb_rd_address;
   wire [8:0]   aq_bb_wr_address;
   
   //-------------------------------------------------------------------------
   //  Fake data generation circuit, for debugging.
   fake_telescope #( .WIDTH(AXNUM), .RNG(RNG) ) FAKE_TART0
     ( .write_clk(clk_e), .write_data(ax_fake) );


   //-------------------------------------------------------------------------
   //  Programmable delay.
	 delay_data_sampling_clk DELAY_RX_CLK
     ( .fast_clk(clk_i),
		   .data_sample_delay(aq_delay_i),
		   .slow_clk(clk_d)
	     );


`ifdef __USE_ACQUISITION
   //-------------------------------------------------------------------------
   //  FIFO for temporary buffering.
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

   //  Drive zeroes onto the unused pins:
   assign mcb_ce_o  = 1'b0;
   assign mcb_wr_o  = 1'b0;
   assign mcb_adr_o = {(ABITS-1){1'b0}};
   assign mcb_dat_o = {32{1'b0}};

`endif // !`ifdef __USE_ACQUISITION


endmodule // tart_capture
