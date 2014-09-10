module fake_telescope(input write_clk, output reg [23:0] write_data);
   reg [23:0] rng_data_counter = 0;
   always @(negedge write_clk)
      begin
         rng_data_counter <= rng_data_counter+ 1'b1;
         write_data <= rng_data_counter;
      end
endmodule
