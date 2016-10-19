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
  #(parameter WIDTH = 32,
    parameter MSB   = WIDTH-1,
    parameter ABITS = 10,
    parameter ASB   = ABITS-1,
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input          clk_i,
    input          rst_i,
    input          cyc_i,
    input          stb_i,
    input          we_i,
    input          bst_i, // Bulk Sequential Transfer?
    output reg     ack_o = 0,
    output         wat_o,
    output         err_o,
    input [ASB:0]  adr_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    output         sram_ce_o,
    output         sram_we_o,
    output [ASB:0] sram_adr_o,
    input [MSB:0]  sram_dat_i,
    output [MSB:0] sram_dat_o
    );

   assign wat_o = 1'b0;
   assign err_o = 1'b0;
   assign dat_o = sram_dat_i;

   assign sram_ce_o = cyc_i && stb_i;
   assign sram_we_o = we_i;
   assign sram_adr_o = adr_i;
   assign sram_dat_o = dat_i;


   //-------------------------------------------------------------------------
   //  Wishbone bus interface.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i) ack_o <= #DELAY 0;
     else       ack_o <= #DELAY cyc_i && stb_i;

   /*
   // SRAM reads, as writes are passed directly through.
   always @(posedge clk_i)
     if (!rst_i && cyc_i && stb_i)
       dat_o <= #DELAY sram_dat_i;
    */


endmodule // wb_sram_interface
