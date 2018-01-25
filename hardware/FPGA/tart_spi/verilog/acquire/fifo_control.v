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
 * 
 * This file is part of TART.
 * 
 * TART is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * TART is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser Public License along with
 * TART.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * 
 * Description:
 * Forwards data from the raw-data FIFO to the DRAM, and then controls the
 * DRAM read-back, once it has filled up.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 * Changelog:
 *  + 04/12/2016  --  asynchronous resets changed to synchronous resets;
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


module fifo_control
  #(parameter SDRAM_ADDRESS_WIDTH = 25,
    parameter ASB   = SDRAM_ADDRESS_WIDTH-2,
    parameter CSB   = SDRAM_ADDRESS_WIDTH-1,
    parameter DELAY = 3)
   (
    input              clock_i,
    input              reset_i,
    input              enable_i,
    input              strobe_i,

    output [8:0]       bb_rd_adr_o,
    output [8:0]       bb_wr_adr_o,
    input [23:0]       bb_rd_dat_i,

    input              aq_rd_req_i,

    input              cmd_waiting,
    output reg         cmd_request = 1'b0,
    output reg         cmd_write = 1'b0,
    output reg [ASB:0] cmd_address = {CSB{1'b0}},
    output [31:0]      cmd_data_in,

    output reg         overflow_o = 1'b0,
    output reg         readback_o = 1'b0,
    output [2:0]       aq_state_o
    );


   //-------------------------------------------------------------------------
   //  Address pointers.
   //-------------------------------------------------------------------------
   //  SDRAM address pointers.
   //  NOTE: 1-bit bigger than needed.
   reg [CSB:0]         sdram_wr_ptr = {SDRAM_ADDRESS_WIDTH{1'b0}};
   reg [CSB:0]         sdram_rd_ptr = {SDRAM_ADDRESS_WIDTH{1'b0}};

   //-------------------------------------------------------------------------
   //  FIFO address pointers.
   //  NOTE: The extra MSB is for overflow detection.
   reg [9:0]           bb_rd_adr = 10'h0, bb_wr_adr = 10'h0;
   wire [10:0]         bb_wr_nxt, bb_rd_nxt;
   wire                bb_empty_n, bb_full;
   reg [2:0]           aq_state = `AQ_WAITING;


   //-------------------------------------------------------------------------
   //  FIFO signal assignments.
   //-------------------------------------------------------------------------
   assign bb_rd_nxt   = bb_rd_adr + 1;
   assign bb_wr_nxt   = bb_wr_adr + 1;
   assign bb_empty_n  = bb_rd_adr != bb_wr_adr;
   assign bb_full     = bb_rd_adr[9] != bb_wr_adr[9] &&
                        bb_rd_adr[8:0] == bb_wr_adr[8:0];

   //-------------------------------------------------------------------------
   //  A MCB command-signal assignments.
   assign cmd_accept  = cmd_waiting && cmd_request;

   //-------------------------------------------------------------------------
   //  Output assignments.
   assign bb_rd_adr_o = bb_rd_adr[8:0];
   assign bb_wr_adr_o = bb_wr_adr[8:0];
   assign cmd_data_in = bb_rd_dat_i;
   assign aq_state_o  = aq_state;


   //-------------------------------------------------------------------------
   //  FIFO write-address logic.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i)
       bb_wr_adr <= #DELAY 10'h0;
     else if (enable_i && strobe_i)
       bb_wr_adr <= #DELAY bb_wr_nxt[9:0];


   //-------------------------------------------------------------------------
   //  FIFO read-address logic.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i)
       bb_rd_adr <= #DELAY 10'h0;
     else if (cmd_accept && cmd_write)
       bb_rd_adr <= #DELAY bb_rd_nxt[9:0];


   //-------------------------------------------------------------------------
   //  FIFO state & control signals.
   //-------------------------------------------------------------------------
   //  Assert availability for read-backs, once the buffering has completed.
   always @(posedge clock_i)
     if (reset_i)
       readback_o <= #DELAY 1'b0;
     else if (enable_i && aq_state > 1)
       readback_o <= #DELAY 1'b1;

   //-------------------------------------------------------------------------
   //  Assert overflow if write is attempted while full.
   always @(posedge clock_i)
     if (reset_i || !enable_i)
       overflow_o <= #DELAY 1'b0;
     else if (bb_full && strobe_i && state == `AQ_BUFFERING)
       overflow_o <= #DELAY 1'b1;


   //-------------------------------------------------------------------------
   //  Raw-data acquisition, and read-back, state-machine.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i) begin
        sdram_wr_ptr <= #DELAY 0; // 1 bit bigger than needed.
        sdram_rd_ptr <= #DELAY 0; // 1 bit bigger than needed.
        cmd_request  <= #DELAY 1'b0;
        cmd_write    <= #DELAY 1'b0;
        aq_state     <= #DELAY `AQ_WAITING;
     end
     else
       case (aq_state)
         `AQ_WAITING: begin
            if (sdram_wr_ptr[SDRAM_ADDRESS_WIDTH-1]) begin
               cmd_request <= #DELAY 1'b1;
               cmd_write   <= #DELAY 1'b0;
               cmd_address <= #DELAY sdram_rd_ptr[SDRAM_ADDRESS_WIDTH-2:0];
               aq_state    <= #DELAY `AQ_READBACK;
            end
            else if (bb_empty_n) begin
               cmd_request <= #DELAY 1'b1;
               cmd_write   <= #DELAY 1'b1;
               cmd_address <= #DELAY sdram_wr_ptr[SDRAM_ADDRESS_WIDTH-2:0];
               aq_state    <= #DELAY `AQ_BUFFERING;
            end
         end // case: `AQ_WAITING

         `AQ_BUFFERING: begin
            if (cmd_accept) begin
               cmd_request  <= #DELAY 1'b0;
               sdram_wr_ptr <= #DELAY sdram_wr_ptr + 1'b1;
               aq_state     <= #DELAY `AQ_WAITING;
            end
         end // case: `AQ_BUFFERING

         `AQ_READBACK: begin
            if (cmd_accept) begin
               cmd_request  <= #DELAY 1'b0;
               sdram_rd_ptr <= #DELAY sdram_rd_ptr + 1'b1;
               aq_state     <= #DELAY `AQ_IDLE;
            end
         end // case: `AQ_READBACK

         `AQ_IDLE: begin
            cmd_address <= #DELAY sdram_rd_ptr[SDRAM_ADDRESS_WIDTH-2:0];
            if (sdram_rd_ptr[SDRAM_ADDRESS_WIDTH-1])
              aq_state <= #DELAY `AQ_FINISHED;
            else if (aq_rd_req_i) begin
               cmd_request <= #DELAY 1'b1;
               cmd_write   <= #DELAY 1'b0;
               cmd_address <= #DELAY sdram_rd_ptr[SDRAM_ADDRESS_WIDTH-2:0];
               aq_state    <= #DELAY `AQ_READBACK;
            end
         end // case: `AQ_IDLE

         `AQ_FINISHED: begin
            $display("Finished.");
         end // case: `AQ_FINISHED

       endcase // case (aq_state)


endmodule // fifo_control
