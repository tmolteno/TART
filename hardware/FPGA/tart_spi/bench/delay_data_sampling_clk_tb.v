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
