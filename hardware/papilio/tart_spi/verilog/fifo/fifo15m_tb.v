module fifo15m_tb;
	
	reg		clock		= 1;
	reg		reset		= 0;
	reg		read		= 0;
	reg		write		= 0;
	
	reg		[17:0]	wr_data	= 0;
	wire	[17:0]	rd_data;
	
	wire	almost_full;
	wire	almost_empty;
	wire	not_empty;
	
	always	#5	clock	<= ~clock;
	
	
	initial begin : Sim
		$display("Time CLK RST RD WR A_FULL A_EMPTY EMPTY# R_DATA W_DATA");
		$monitor("%5t %b %b %b %b %b %b %b %h %h", $time, clock, reset,
			read, write, almost_full, almost_empty, not_empty, rd_data, wr_data);
		
		#2
		reset	= 1;
		
		#10
		reset	= 0;
		
/*		#10
		write	= 1;
		wr_data	= 18'h30F0;
		
		#10
		read	= 1;
		wr_data	= 18'h0A0A;
		
		#10
		read	= 0;
		write	= 0;
		*/
		#300
		$finish;
	end	//	Sim
	
	
	always @(posedge clock)
	begin
		if (reset)
			write	<= 0;
		else
		begin
			write	<= 1;
			wr_data	<= $random;
		end
	end
	
	
	always @(almost_full, reset)
	begin
		if (reset)
			read	<= 0;
		else
			read	<= almost_full;
	end
	
	
	//	18-bit wide, 15 entry FIFO, ultra high-speed FIFO
	fifo15m #('d18) FIFO0(
		.read_clock_i(clock),
		.write_clock_i(clock),
		.reset_i(reset),
		.read_i(read),
		.write_i(write),
		.data_i(wr_data),
		.data_o(rd_data),
		.almost_full_o(almost_full),
		.almost_empty_o(almost_empty),
		.not_empty_o(not_empty)
	);
	
	
endmodule	//	fifo16_tb
