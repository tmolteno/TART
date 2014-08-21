`timescale 1ns/100ps
module user_fifos_tb;
	
	reg	clock	= 1;
	always	#5	clock	<= ~clock;
	
	reg	reset	= 0;
	
	reg	rd_req		= 0;
	reg	[1:0]	rd_owner	= 0;
	reg	rd_block	= 0;
	reg	[22:0]	rd_addr	= 0;
	
	wire	raf_full;
	reg	raf_read	= 0;
	wire	raf_block;
	wire	raf_empty_n;
	wire	[1:0]	raf_owner;
	wire	[14:0]	raf_addr;
	
	
	initial begin : Sim
		// Create a VCD file that
		$write ("%% ");	// Prevent from breaking my display tool
		$dumpfile ("fifos.vcd");
		$dumpvars;
		
/*		$display ("Time CLK RST Start Done");
		$monitor ("%5t  %b  %b  %b    %b  ", $time, clock, reset, start, done);
		*/
		#5
		reset	<= 1;
		
		#10
		reset	<= 0;
		
		
		// Single word read
		#10
		rd_req		<= 1;
		rd_owner	<= 2'b10;
		rd_block	<= 0;
		rd_addr		<= 10;
		
		#10
		rd_req		<= 'bx;
		rd_block	<= 'bx;
		rd_owner	<= 'bx;
		rd_addr		<= 57;
		
		#20
		raf_read	<= 1;
		
		#10
		raf_read	<= 0;
		
		#50
		$finish;
	end	// Sim
	
	
	user_fifos UFIFO0 (
		.clock_i	(clock),
		.reset_i	(reset),
		
		// User interface.
		.rd_req_i	(rd_req),
		.rd_block_i	(rd_block),
		.rd_busy_o	(raf_full),
		.rd_owner_i	(rd_owner),
		.rd_addr_i	(rd_addr),
		
		.wr_req_i	(0),
		.wr_busy_o	(),
		.wr_addr_i	(),
		.wr_bytes_i	(),
		.wr_data_i	(),
		
		// FIFO interface (to command sequencer).
		.raf_read_i	(raf_read),
		.raf_block_o	(raf_block),
		.raf_empty_no	(raf_empty_n),
		.raf_owner_o	(raf_owner),
		.raf_addr_o	(raf_addr),
		
		// Address and data are seperate FIFOs.
		.waf_read_i	(0),
		.waf_empty_no	(),
		.waf_addr_o	(),
		
		.wdf_read_i	(0),
		.wdf_bytes_o	(),
		.wdf_data_o	()
	);
	
endmodule	// user_fifos_tb
