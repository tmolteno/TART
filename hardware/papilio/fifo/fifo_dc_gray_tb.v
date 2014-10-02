`timescale 1ns / 100ps
module fifo_dc_gray_tb;
	
	reg	rclk	= 1;
	reg	wclk	= 1;
	reg	reset	= 0;
	reg	read	= 0;
	reg	write	= 0;
	
	reg	[17:0]	wr_data	= 0;
	wire	[17:0]	rd_data;
	
	wire	[1:0]	rlev, wlev;
	wire	full, empty;
	
	always	#10	rclk	<= ~rclk;
	always	#5	wclk	<= ~wclk;
	
	
	initial begin : Sim
		// Create a VCD file
		$write ("%% ");	// Prevent from breaking my display tool
		$dumpfile ("mm.vcd");
		$dumpvars;
		
		#2
		reset	= 1;
		
		#20	// 22
		reset	= 0;
		
		#30
		write	= 1;
		wr_data	= 18'h30F0;
		
		#10	// 62
		wr_data	= 18'h0A0A;
		
		#10
		write	= 0;
		
		#30	// 102
		read	= 1;
		
		#20	// 122
		read	= 0;
		
		#30
		$finish;
	end	//	Sim
	
	
	fifo_dc_gray #(18,4,16) FIFO0 (
		.rd_clk		(rclk),
		.wr_clk		(wclk),
		.rst		(1),
		.clr		(reset),
		.din		(wr_data),
		.we		(write),
		
		.dout		(rd_data),
		.re		(read),
		.full		(full),
		.empty		(empty),
		.wr_level	(wlev),
		.rd_level	(rlev)
	);
	
	
endmodule	// module fifo_dc_gray_tb
