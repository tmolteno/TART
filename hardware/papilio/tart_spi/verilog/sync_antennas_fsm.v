`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    10:55:39 09/03/2014 
// Design Name: 
// Module Name:    sync_antennas 
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
module sync_antennas_fsm(
      input fast_clk,
      input input_signal,
      output reg slow_clk=1,
      output reg sampled_bit
    );

reg [8:0] sampled = 9'b0;
reg [4:0] slice = 5'b0;
reg [5:0] when_to_output = 6'b000001;
reg [2:0] when_to_clk = 3'b001;

always @(posedge fast_clk)
   begin
      //------------------------------------------
      //-- Add another bit onto the shift register
      //------------------------------------------
      sampled <= {sampled[7:0] , input_signal};
      
      when_to_output <= {when_to_output[4:0], when_to_output[5]};
      if (when_to_output[0] == 1'b1) sampled_bit <= slice[2];
      
      when_to_clk <= {when_to_clk[1:0], when_to_clk[2]};
      if (when_to_clk[0] == 1'b1) slow_clk <= ~slow_clk;

      case (slice)
          5'b00000, 5'b11111: slice <= sampled[6:2];    // All good.
          5'b00001, 5'b11110: slice <= sampled[5:1];    // Phase OK, need to sample one bit earlier.
          5'b00011, 5'b11100: slice <= sampled[4:0];    // Phase marginal need to sample one bit earlier
          5'b00111, 5'b11000: slice <= sampled[8:4];    // Phase marginal need to sample one bit later
          5'b01111, 5'b10000: slice <= sampled[7:3];    // Phase OK, need to sample one bit later
          default: slice <= sampled[6:2]; //do not adjust. bit errors..
      endcase;
   end
endmodule

module sync_antennas_fsm_tb();
   wire sampled_bit;
   wire slow_clk;
   
   reg fast_clk = 0; always #5 fast_clk = ~fast_clk;
   reg rx_clk = 0; always #30 rx_clk = ~rx_clk;
   reg input_signal = 0;

   integer i=0;
   reg [79:0] rdmlist = 80'habcdebcafbcdfbddfbbd;
   sync_antennas_fsm  inst(
   .fast_clk(fast_clk),
   .input_signal(input_signal),
   .slow_clk(slow_clk),
   .sampled_bit(sampled_bit)
   );
   always @(negedge rx_clk)
      begin
         i <= i+1;
         input_signal <= rdmlist[79-i];
      end
   initial begin
      #23 rx_clk = 1;
      #4800
      #200
      $finish();
   end
endmodule

