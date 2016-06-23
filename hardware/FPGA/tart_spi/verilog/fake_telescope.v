`timescale 1ns/100ps
module fake_telescope
  #(parameter WIDTH = 24,
    parameter MSB   = WIDTH-1,
    parameter DELAY = 3)
   (
    input              write_clk,
    output reg [MSB:0] write_data = 0
   );

//    reg [MSB:0] rng_data_counter = 0;

   // Looks like radio data is valid on negative edge of the clock. Therefore,
   // set data on pos. edge of the clock.
   always @(posedge write_clk) begin
      write_data <= #DELAY write_data + 1;
//       rng_data_counter <= rng_data_counter + 1'b1;
//       write_data       <= rng_data_counter;
   end

endmodule // fake_telescope
