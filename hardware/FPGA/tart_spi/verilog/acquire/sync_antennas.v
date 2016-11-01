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
module sync_antennas(
      input fast_clk,
      input input_signal,
      output reg sampled_bit,
      output reg lost_alignment
    );

reg [4:0] sampled = 0;
reg [5:0] when_to_output = 6'b000001;
reg [5:0] slipping_bits = 0;

always @(posedge fast_clk)
   begin
      //----------------------------------------------------------------
      // as lost_alignment is '1' for at six cycles, making it crossed
      // to the slow domain nicely and will not be accidentally missed
      //----------------------------------------------------------------
      lost_alignment <= slipping_bits[5];
      slipping_bits  <= {slipping_bits[4:0], 1'b0};
      //------------------------------------------
      //-- Add another bit onto the shift register
      //------------------------------------------
      sampled <= {sampled[3:0] , input_signal};
      //-----------------------------------------
      // Now's the time to output the next bit
      //-----------------------------------------
      if (when_to_output[0] == 1'b1)
         begin // as this is usually set only one in six cycles, it crosses domains correctly.
            sampled_bit <= sampled[3];
            if (sampled[4:2] == 3'b000 || sampled[4:2] == 3'b111)
              // looks like we are properly aligned
              when_to_output <= {when_to_output[4:0], when_to_output[5]};
            else
              slipping_bits <= 6'b111111;
           // don't advance when_to_output until we see three identical bits in a row.
           // Also tell the slower domain that phase sync has been lost
         end
      else
        when_to_output <= {when_to_output[4:0], when_to_output[5]};
          
   end
endmodule // sync_antennas

module sync_antennas_tb();
   wire lost_alignment;
   wire sampled_bit;

   reg fast_clk = 0; always #5 fast_clk = ~fast_clk;
   reg rx_clk = 0; always #30 rx_clk = ~rx_clk;
   reg input_signal = 0;

   integer i;

   sync_antennas  inst(
   .fast_clk(fast_clk),
   .input_signal(input_signal),
   .sampled_bit(sampled_bit),
   .lost_alignment(lost_alignment)
   );
   always @(negedge rx_clk) input_signal <= ~input_signal;
   initial begin
      #23 rx_clk = 1;
      #1000
      $finish();
   end
endmodule

