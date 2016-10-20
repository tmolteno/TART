`timescale 1ns/100ps
/*
 * Module      : verilog/tart_bank_switch.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Counts the number of correlations for the current bank, and then signals a
 * bank-switch once the counter reaches its maximum.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

module tart_bank_switch
  #( parameter COUNT = 24,
     parameter MSB   = COUNT-1,
     parameter DELAY = 3)
   (
    input         clk_x, // correlator clock
    input         clk_i, // bus clock
    input         rst_i,
    input         ce_i,

    input [MSB:0] bcount_i, // block size - 1
    input         strobe_i, // new antenna data
    output        swap_x,   // NOTE: correlator domain
    output        swap_o    // NOTE: bus domain
    );

   reg                 sw = 1'b0, switch = 1'b0;
   reg                 sw_x = 0, sw_d = 0; // correlator domain
   reg                 sw_b = 1'b0;        // bus domain

   assign swap_x = sw;
   assign swap_o = switch;


   //-------------------------------------------------------------------------
   //  
   //  Hardware-correlator bank-switching control logic.
   //  
   //-------------------------------------------------------------------------
   wire [COUNT:0]      next_count = wrap_count ? {(COUNT+1){1'b0}} : count+1;
   wire                wrap_count = count == bcount_i;
   reg [MSB:0]         count = {COUNT{1'b0}};
   reg [3:0]           delays = 4'h0;

   //-------------------------------------------------------------------------
   //  Count the number of correlations for the current bank, and signal a
   //  bank-switch when the counter reaches its maximum.
   always @(posedge clk_x)
     if (rst_i) begin
        sw    <= #DELAY 1'b0;
        count <= #DELAY {COUNT{1'b0}};
     end
     else if (ce_i) begin
        sw    <= #DELAY delays[3] && wrap_count; // signal an upcoming bank-swap
        count <= #DELAY strobe_i ? next_count[MSB:0] : count;
     end
     else begin
        sw    <= #DELAY 1'b0;
        count <= #DELAY count;
     end

   always @(posedge clk_x)
     delays <= #DELAY {delays[2:0], !rst_i && strobe_i};


   //-------------------------------------------------------------------------
   //  Synchronise the bank-switching signal to the bus domain.
   always @(posedge clk_x)
     if (rst_i || strobe_i) sw_x <= #DELAY 1'b0;
     else if (sw)           sw_x <= #DELAY 1'b1;
     else                   sw_x <= #DELAY sw_x;

   always @(posedge clk_x)
     if (rst_i) sw_d <= #DELAY 0;
     else       sw_d <= #DELAY sw_x && strobe_i;

   always @(posedge clk_i or posedge sw_d)
     if (sw_d)                 sw_b <= #DELAY 1'b1;
     else if (rst_i || switch) sw_b <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (rst_i) switch <= #DELAY 1'b0;
     else       switch <= #DELAY sw_b && !switch;


endmodule // tart_bank_switch
