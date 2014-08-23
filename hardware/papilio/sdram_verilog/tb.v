`timescale 1ns / 1ps

////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer:
//
// Create Date:   17:53:53 08/21/2014
// Design Name:   SDRAM_Controller
// Module Name:   C:/Users/User/Documents/GitHub/projects/TART/hardware/verilog/sdram_verilog/sdram_verilog/tb.v
// Project Name:  sdram_verilog
// Target Device:  
// Tool versions:  
// Description: 
//
// Verilog Test Fixture created by ISE for module: SDRAM_Controller
//
// Dependencies:
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
////////////////////////////////////////////////////////////////////////////////



module tb();
   reg clk;
   always #15.625 clk = ~clk;	
   count_to_100 cnt100(.fpga_clk_32(clk),.led(led));
	initial begin
		clk = 0;
		// Wait for RAM to get ready
		#2200000;
		$finish();
	end
      
endmodule

