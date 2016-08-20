`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    17:25:20 07/31/2015 
// Design Name: 
// Module Name:    delay_data_sampling_clk 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////
module delay_data_sampling_clk(
      input fast_clk,
      input [2:0] data_sample_delay,
      output wire slow_clk
		);

		reg [5:0] out = 6'b111000;
		always @(posedge fast_clk) out <= {out[4:0], out[5]};
		assign slow_clk = out[5-data_sample_delay];
	
endmodule
