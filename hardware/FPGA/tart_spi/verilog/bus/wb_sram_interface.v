`timescale 1ns/100ps
/*
 * Module      : verilog/bus/wb_sram_interface.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Single-port SRAM interface, with Wishbone-like interconnect, and
 * parameterised by size and bit-width.
 * 
 * NOTE:
 *  + just the interface, the SRAM memories are assumed to be external;
 *  + doesn't support "classic" Wishbone cycles, but does support pipelined
 *    transactions;
 *  + supports burst transfers -- these are indicated by `bst_i` being
 *    asserted at the beginning of a transaction, and deasserted one cycle
 *    before the final (pipelined) `ack_o`;
 * 
 * TODO:
 * 
 */

module wb_sram_interface
  #(parameter ABITS = 10,       // Address bit-width
    parameter ASB   = ABITS-1,  // MSB of address
    parameter USEBE = 1,        // Use individual byte-enables (0/1)?
    parameter BYTES = WIDTH>>3, // Number of byte-enables
    parameter BSB   = BYTES-1,  // MSB of byte-enables
    parameter WIDTH = 32,       // Data bit-width
    parameter MSB   = WIDTH-1,  // MSB of data
    parameter PIPED = 1,        // SRAM supports pipelined transfers (0/1)?
    parameter TICKS = 1,        // Read latency, in cycles
    parameter ASYNC = TICKS==0, // Asynchronous reads
    parameter TSB   = TICKS-1,
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input          clk_i,
    input          rst_i,
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output         ack_o,
    output         wat_o, // Equiv. to WB `STALL_O`
    output         rty_o,
    output         err_o,
    input [ASB:0]  adr_i,
    input [BSB:0]  sel_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    output         sram_ce_o,
    output         sram_we_o,
    output [ASB:0] sram_adr_o,
    output [BSB:0] sram_bes_o,
    input [MSB:0]  sram_dat_i,
    output [MSB:0] sram_dat_o
    );


   //-------------------------------------------------------------------------
   //  Output (shift-)registers.
   //-------------------------------------------------------------------------
   reg [TSB:0]     ack_out = {TICKS{1'b0}};
   reg [TSB:0]     wat_out = {TICKS{1'b0}};


   //-------------------------------------------------------------------------
   //  Line drivers for the WB and SRAM interfaces.
   //-------------------------------------------------------------------------
   assign ack_o = ASYNC ? cyc_i && stb_i : ack_out[TSB];
   assign wat_o = TICKS > 1 && !PIPED ? wat_out : 1'b0; // TODO
   assign rty_o = 1'b0;
   assign err_o = 1'b0;
   assign dat_o = sram_dat_i;

   assign sram_ce_o  = cyc_i && stb_i;
   assign sram_we_o  = we_i;
   assign sram_adr_o = adr_i;
   assign sram_bes_o = sel_i;
   assign sram_dat_o = dat_i;


   //-------------------------------------------------------------------------
   //  Wishbone bus interface.
   //-------------------------------------------------------------------------
   wire [TICKS:0]  ack_nxt = {ack_out, cyc_i && stb_i};

   always @(posedge clk_i)
     if (rst_i)
       ack_out <= #DELAY {TICKS{1'b0}};
     else
       ack_out <= #DELAY ack_nxt[TSB:0];


endmodule // wb_sram_interface
