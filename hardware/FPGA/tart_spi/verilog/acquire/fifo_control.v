`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/fifo_control.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Forwards data from the raw-data FIFO to the DRAM, and then controls the
 * DRAM read-back, once it has filled up.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 * Changelog:
 *  + asynchronous resets changed to synchronous resets;
 * 
 */


//----------------------------------------------------------------------------
//  States for raw-data acquisition, and read-back.
//----------------------------------------------------------------------------
`define AQ_WAITING   3'd0
`define AQ_BUFFERING 3'd1
`define AQ_READBACK  3'd2
`define AQ_IDLE      3'd3
`define AQ_FINISHED  3'd4


module fifo_sdram_fifo_scheduler
  #(parameter SDRAM_ADDRESS_WIDTH = 25,
    parameter ASB   = SDRAM_ADDRESS_WIDTH-2,
    parameter CSB   = SDRAM_ADDRESS_WIDTH-1,
    parameter DELAY = 3)
   (
    input              clock_i,
    input              reset_i,
    input              enable_i,
    input              strobe_i,

    output reg [8:0]   aq_bb_rd_adr = 9'b0,
    output reg [8:0]   aq_bb_wr_adr = 9'b0,
    input [23:0]       aq_read_data,

    input              spi_buffer_read_complete,

    input              cmd_waiting,
    output reg         cmd_request = 1'b0,
    output reg         cmd_write   = 1'b0,
    output reg [ASB:0] cmd_address = {CSB{1'b0}},
    output reg [31:0]  cmd_data_in = 32'b0,

    output reg [2:0]   tart_state  = `AQ_WAITING
    );


   //-------------------------------------------------------------------------
   //  Address pointers.
   //-------------------------------------------------------------------------
   //  NOTE: 1-bit bigger than needed.
   reg [CSB:0]         sdram_wr_ptr = {SDRAM_ADDRESS_WIDTH{1'b0}};
   reg [CSB:0]         sdram_rd_ptr = {SDRAM_ADDRESS_WIDTH{1'b0}};
   reg [1:0]           Sync_start = 2'b0;

   // new signal synchronized to (=ready to be used in) clock_iB domain
   wire                enable = Sync_start[1];


   //-------------------------------------------------------------------------
   //  Write addresses.
   //-------------------------------------------------------------------------
   //  TODO: Counts 6x too fast?
   //  TODO: Write addresses aren't used.
   always @(posedge clock_i)
     if (reset_i)
       aq_bb_wr_adr <= #DELAY 9'b0;
     else if (enable_i && strobe_i)
       aq_bb_wr_adr <= #DELAY aq_bb_wr_adr + 1'b1;


   //-------------------------------------------------------------------------
   //  Raw-data acquisition, and read-back, state-machine.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i) begin
        sdram_wr_ptr <= #DELAY 0; // 1 bit bigger than needed.
        sdram_rd_ptr <= #DELAY 0; // 1 bit bigger than needed.
        tart_state   <= #DELAY `AQ_WAITING;
        cmd_write    <= #DELAY 1'b0;
        aq_bb_rd_adr <= #DELAY 9'b0;
     end
     else
       case (tart_state)
         `AQ_WAITING: begin
            if (sdram_wr_ptr[SDRAM_ADDRESS_WIDTH-1])
              tart_state <= #DELAY `AQ_READBACK;
            else if (aq_bb_rd_adr != aq_bb_wr_adr)
              tart_state <= #DELAY `AQ_BUFFERING;
         end // case: `AQ_WAITING

         `AQ_BUFFERING: begin
            if (cmd_waiting && !cmd_request) begin
               cmd_write    <= #DELAY 1'b1;
               cmd_address  <= #DELAY sdram_wr_ptr[SDRAM_ADDRESS_WIDTH-2:0];
               sdram_wr_ptr <= #DELAY sdram_wr_ptr + 1'b1;
               cmd_data_in  <= #DELAY aq_read_data[23:0];
               aq_bb_rd_adr <= #DELAY aq_bb_rd_adr + 1'b1;
               tart_state   <= #DELAY `AQ_WAITING;
            end
         end // case: `AQ_BUFFERING

         `AQ_READBACK: begin
            if (cmd_waiting && !cmd_request) begin
               cmd_write    <= #DELAY 1'b0;
               cmd_address  <= #DELAY sdram_rd_ptr[SDRAM_ADDRESS_WIDTH-2:0];
               sdram_rd_ptr <= #DELAY sdram_rd_ptr + 1'b1;
               tart_state   <= #DELAY `AQ_IDLE;
            end
         end // case: `AQ_READBACK

         `AQ_IDLE: begin
            if (sdram_rd_ptr[SDRAM_ADDRESS_WIDTH-1])
              tart_state <= #DELAY `AQ_FINISHED;
            else if (spi_buffer_read_complete)
              tart_state <= #DELAY `AQ_READBACK;
         end // case: `AQ_IDLE

         `AQ_FINISHED: begin
            $display("Finished.");
         end // case: `AQ_FINISHED

       endcase // case (tart_state)


   //-------------------------------------------------------------------------
   //  Issue READ/WRITE requests to the memory controller.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i || cmd_request)
       cmd_request <= #DELAY 1'b0;
     else
       case (tart_state)
         `AQ_BUFFERING, `AQ_READBACK:
           cmd_request  <= #DELAY cmd_waiting;

         default:
           cmd_request  <= #DELAY 1'b0;

       endcase // case (tart_state)


endmodule // fifo_sdram_fifo_scheduler
