`timescale 1ns/100ps
/*
 * Module      : rtl/wb_reset.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
 * 
 * Simple reset circuit, connected via a Wishbone-compatible interconnect.
 * 
 * NOTE:
 *  + supports both classic and pipelined transfers;
 * 
 * TODO:
 * 
 */

module wb_reset
  #(parameter WIDTH = 8,
    parameter MSB   = WIDTH-1,
    parameter RTIME = 4,
    parameter RSB   = RTIME-1,

    //  Simulation-only settings:
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input          clk_i,
    input          rst_i,
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output reg     ack_o = 1'b0,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    input          reset_ni, // from optional external source
    output reg     reset_o = 1'b0
    );

   reg             reset  = 1'b0;
   reg [RTIME-1:0] resets = {RTIME{1'b0}};
   wire            reset_w;


   assign dat_o   = {MSB{1'b0}, reset_o};
   assign reset_w = cyc_i && stb_i && we_i && dat_i[0];
//    assign reset_w = cyc_i && stb_i && we_i && !ack_o && dat_i[0];


   //-------------------------------------------------------------------------
   //  Logic for the Wishbone-like interconnect.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       ack_o <= #DELAY 1'b0;
     else
       ack_o <= #DELAY cyc_i && stb_i && !ack_o;


   //-------------------------------------------------------------------------
   //  Reset logic.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       reset <= #DELAY 1'b0;
     else
       reset <= #DELAY reset ? ~reset_ni : reset_w;
//        reset <= #DELAY reset || reset_o ? 1'b0 : (!reset_ni || reset_w);

   always @(posedge clk_i) begin
      reset_o <= #DELAY |resets;
      resets  <= #DELAY {resets[RTIME-2:0], reset};
   end


endmodule // wb_reset
