`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/delay_data_sampling_clk.v
 * Copyright   : (C) Tim Molteno     2013
 *             : (C) Max Scheel      2013
 * License     : LGPL3
 * 
 * Maintainer  : Max Scheel
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Adds a variable delay to the given clock.
 * 
 */

module delay_data_sampling_clk
  (
   input       fast_clk,
   input [2:0] data_sample_delay,
   output wire slow_clk
	 );

	 reg [5:0]   out = 6'b111000;

	 assign slow_clk = out[5-data_sample_delay];

	 always @(posedge fast_clk)
     out <= {out[4:0], out[5]};

endmodule // delay_data_sampling_clk
