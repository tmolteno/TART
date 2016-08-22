`timescale 1ns/100ps
/*
 * Module      : verilog/fake_telescope.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Generates either incremental or pseudorandom values, for filling the TART
 * SDRAM with debug data.
 * 
 * Changelog:
 *  + ??/??/2013  --  initial file;
 *  + 22/08/2016  --  added MFSR data-generation;
 * 
 * NOTE:
 *  + MFSR stands for Multiple-Feedback Shift Register;
 * 
 * TODO:
 * 
 */

module fake_telescope
  #(parameter WIDTH = 24,
    parameter MSB   = WIDTH-1,
    parameter RNG   = 1,
    parameter DELAY = 3)
   (
    input          write_clk,
    output [MSB:0] write_data
    );

   wire [31:0]     mfsr_new;
   reg [31:0]      mfsr_reg = RNG;
   reg [MSB:0]     data_reg = RNG;

   assign write_data = RNG == 0 ? data_reg : mfsr_reg[MSB:0];

   always @(posedge write_clk) begin
      data_reg <= #DELAY data_reg + 1;
      mfsr_reg <= #DELAY mfsr_new;
   end


   //-------------------------------------------------------------------------
   //  One of Roy's MFSR's (which is similar to a LFSR, but fewer gates).
   //-------------------------------------------------------------------------
   mfsr32 MFSR32 (.count_i(mfsr_reg), .count_o(mfsr_new));


endmodule // fake_telescope
