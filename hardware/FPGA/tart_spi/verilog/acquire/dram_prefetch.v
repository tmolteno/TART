`timescale 1ns/100ps
/*
 * Module      : verilog/dram_prefetch.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Prefetches streaming data from the SDRAM controller.
 * 
 * TODO:
 * 
 */

// Data prefetcher states.
`define PRE_EMPTY 2'h0
`define PRE_WORD1 2'h1
`define PRE_WORD2 2'h2
   
module dram_prefetch
  #( parameter WIDTH = 24,
     parameter MSB   = WIDTH-1,
     parameter DELAY = 3)
   ( // DRAM and bus clock
     input              clock_i,
     input              reset_i,

     // Streaming data interface
     // NOTE: Doesn't need to initiate transfers, but data is valid whenever
     //   `data_ready` is asserted.
     input              dram_ready,
     output reg         dram_request = 1'b0,
     input [MSB:0]      dram_data,
     input              data_sent,
     output reg [MSB:0] fetched_data = {WIDTH{1'b0}}
     );

   //-------------------------------------------------------------------------
   //  
   //  Data prefetch (from SDRAM) logic.
   //  
   //-------------------------------------------------------------------------
   reg [23:0] prefetch_data = 24'b0;
   reg [1:0]  pre_state = `PRE_EMPTY;

   // Data-prefetch state machine.
   // NOTE: There should never be more than two words being stored or fetched.
   always @(posedge clock_i)
     if (reset_i)
       pre_state <= `PRE_EMPTY;
     else
       case (pre_state)
         `PRE_EMPTY: pre_state <= dram_ready ? `PRE_WORD1 : pre_state ;
         `PRE_WORD1: pre_state <= dram_ready && !data_sent ? `PRE_WORD2 :
                                  !dram_ready && data_sent ? `PRE_EMPTY : pre_state ;
         `PRE_WORD2: pre_state <= data_sent  ? `PRE_WORD1 : pre_state ;
       endcase // case (pre_state)

   always @(posedge clock_i)
     if (dram_ready) begin
        if (!data_sent && pre_state == `PRE_EMPTY)
          fetched_data <= dram_data;
        else if (data_sent && pre_state == `PRE_WORD1)
          fetched_data <= dram_data;
        else if (!data_sent && pre_state == `PRE_WORD1)
          fetched_data <= prefetch_data;
`ifdef __icarus
        else
          $error ("%5t: data arrived while in an incompatible state.", $time);
`endif
     end
     else if (data_sent && pre_state == `PRE_WORD2)
       fetched_data <= prefetch_data;

   always @(posedge clock_i)
     if (dram_ready)
       prefetch_data <= dram_data;

   always @(posedge clock_i)
     if (reset_i || dram_request)
       dram_request <= 0;
     else if (pre_state == `PRE_EMPTY && dram_ready)
       dram_request <= 1;
     else if (pre_state == `PRE_WORD2 && data_sent)
       dram_request <= 1;


endmodule // dram_prefetch
