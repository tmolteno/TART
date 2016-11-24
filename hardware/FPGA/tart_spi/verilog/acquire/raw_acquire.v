`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/raw_acquire.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
 * 
 * Capture raw antenna signals, buffering them in a Spartan 6 SRAM, while
 * streaming to an off-chip SDRAM.
 * 
 * NOTE:
 *  + the `x` suffix is used to tag signals of the (12x by default) sampling/
 *    correlator clock-domain;
 *  + even though the sample-clock's frequency is an integer multiple of the
 *    external clock, the phase relationship is unknown (due to the quirky
 *    Spartan 6 DCM's), thus asynchronous domain-crossing techniques must be
 *    used;
 * 
 * TODO:
 *  + the handling of asynchronous signals is still a mess;
 * 
 */

`include "tartcfg.v"

module raw_acquire
  #(//  Bit-width parameters:
    parameter AXNUM = 24,       // number of antennae
    parameter MSB   = AXNUM-1,  // MSB of antenna signal
    parameter ABITS = 20,       // DRAM controller address bit-width
    parameter ASB   = ABITS-2,  // MSB of DRAM address

    //  Additional settings:
    parameter RESET = 0,        // enable reset-to-zero (0/1)?

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clock_i, // bus clock
    input          reset_i, // bus-domain reset
    input          clock_x, // data-sampling clock
    input          reset_x, // sample-domain reset

    //  Module control-signals:
    input          capture_i, // enable raw-data capture (0/1)
    input          request_i, // retrieve sample from SDRAM
   
    //  Raw signal data input:
    //  NOTE: Sample/correlator clock-domain.
    input          strobe_x_i, // strobes for each new sample
    input [MSB:0]  signal_x_i, // supersampled & aligned antenna signals

    //  Memory Controller Block (MCB) signals:
    output         mcb_ce_o,
    output         mcb_wr_o,
    input          mcb_rdy_i,
    output [ASB:0] mcb_adr_o,
    output [31:0]  mcb_dat_o,

    output [2:0]   state_o      // just for debug info
    );


   //-------------------------------------------------------------------------
   //  Acquistion-block signals:
   //  TODO: Which domains do these belong to?
   wire [MSB:0]    rdata;
   wire [8:0]      raddr;
   reg [8:0]       waddr_x = 9'h0;
   wire [9:0]      waddr_x_next;


`ifdef __USE_ACQUISITION
   //-------------------------------------------------------------------------
   //  Increment address after each write.
   //-------------------------------------------------------------------------
   assign waddr_x_next = waddr_x + 1;

   always @(posedge clock_x)
     if (reset_x && RESET)      // not really needed, as continually wraps by
       waddr_x <= #DELAY 9'h0;  // default ...
     else if (strobe_x_i)
       waddr_x <= #DELAY waddr_x_next[8:0];
     else
       waddr_x <= #DELAY waddr_x;

       
   //-------------------------------------------------------------------------
   //  Xilinx block SRAM for temporary buffering.
   //-------------------------------------------------------------------------
   //  NOTE: Functions as a FIFO, but with the control-logic spread between
   //    this and the 'fifo_sdram_fifo_scheduler' modules.
   block_buffer AQ_BB
     ( // read port:
       .read_clock_i   (clock_i),
       .read_address_i (raddr),
       .read_data_o    (rdata),
       // write port:
       .write_clock_i  (clock_x),
       .write_enable_i (strobe_x_i),
       .write_address_i(waddr_x),
       .write_data_i   (signal_x_i)
       );


   //-------------------------------------------------------------------------
   //      Storage block controller.
   //-------------------------------------------------------------------------
   //  NOTE: Bus-clock domain.
   fifo_sdram_fifo_scheduler
     #(.SDRAM_ADDRESS_WIDTH(ABITS))
   SCHEDULER0
     ( .clock  (clock_i),           // bus-clock
       .reset  (reset_i),           // global reset (bus-domain)

       .aq_bb_rd_address(raddr),
       .aq_read_data    (rdata),

       .spi_start_aq(capture_i),
       .spi_buffer_read_complete(request_i),

       .cmd_enable (mcb_ce_o),
       .cmd_write  (mcb_wr_o),
       .cmd_ready  (mcb_rdy_i),
       .cmd_address(mcb_adr_o),
       .cmd_data_in(mcb_dat_o),

       .tart_state (state_o)
       );


`else // !`ifdef __USE_ACQUISITION
   //-------------------------------------------------------------------------
   //  Just sleep the Memory Controller Block (MCB) if not using the raw-data
   //  acquisition block.
   //-------------------------------------------------------------------------
   assign mcb_ce_o  = 1'b0;
   assign mcb_wr_o  = 1'b0;
   assign mcb_adr_o = {(ABITS-1){1'b0}};
   assign mcb_dat_o = {32{1'b0}};

`endif // !`ifdef __USE_ACQUISITION


endmodule // raw_acquire
