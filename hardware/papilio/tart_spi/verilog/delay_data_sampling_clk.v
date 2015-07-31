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

module delay_data_sampling_clk_tb();
   reg fast_clk = 0; always #5 fast_clk = ~fast_clk;
   reg [2:0] data_sample_delay = 3'b000;
   wire slow_clk;

   delay_data_sampling_clk inst(
   .fast_clk(fast_clk),
   .data_sample_delay(data_sample_delay),
   .slow_clk(slow_clk)
   );

   initial begin
      #2000 data_sample_delay <= 3'b000;
		#2000 data_sample_delay <= 3'b001;
		#2000 data_sample_delay <= 3'b010;
		#2000 data_sample_delay <= 3'b011;
		#2000	data_sample_delay <= 3'b100;
		#2000 data_sample_delay <= 3'b101;
		#2000
      $finish();
   end
endmodule
