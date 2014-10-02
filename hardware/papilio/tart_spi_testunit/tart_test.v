`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    10:14:50 09/11/2014 
// Design Name: 
// Module Name:    tart_test 
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
module tart_test(
   input CLK32,
   output reg [23:0] antenna,
   output reg tx_clk_16
    );
   reg [23:0] antenna_int = 24'b0;
   reg tx_clk_16_int = 0;
   
   always @(posedge CLK32)
      begin
         tx_clk_16_int <= ~tx_clk_16_int;
         tx_clk_16 <= tx_clk_16_int;
      end
   always @(negedge tx_clk_16_int)
      begin
         antenna_int <= antenna_int + 1'b1;
         antenna <= antenna_int;
      end

   //assign tx_clk_16 = tx_clk_16_int;
   //assign antenna = antenna_int;

endmodule

module tart_test_tb();
   reg  CLK32 = 0; always #15.625 CLK32 = ~CLK32;
   wire [23:0] antenna;
   wire tx_clk_16;
   
   tart_test uut(.CLK32(CLK32), .antenna(antenna), .tx_clk_16(tx_clk_16));
   initial begin
         #10000 $finish();
      end
endmodule
