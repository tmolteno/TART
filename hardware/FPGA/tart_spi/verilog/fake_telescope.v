module fake_telescope(input write_clk, output reg [23:0] write_data);
   reg [23:0] rng_data_counter = 0;
   always @(posedge write_clk)  // looks like radio data is valid on negative edge of the clock. therefore set data on pos. edge of the clock.
      begin
         rng_data_counter <= rng_data_counter+ 1'b1;
         write_data <= rng_data_counter;
      end
endmodule
