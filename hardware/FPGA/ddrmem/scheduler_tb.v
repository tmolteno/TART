`timescale 1ns/100ps
module	scheduler_tb;
	
	reg	clock	= 1;
	always	#5	clock	<= ~clock;
	
	reg	reset	= 0;
	// reg	enable	= 0;
	
	reg	raf_empty_n	= 0;
	reg	raf_block	= 0;
	wire	raf_read;
	reg	[22:0]	raf_addr	= 0;
	
	reg	exec	= 0;
	reg	active	= 0;
	
	initial begin : Sim
		// Create a VCD file
		$write ("%% ");	// Prevent from breaking my display tool
		$dumpfile ("testsched.vcd");
		$dumpvars;
		
		#5
		reset	<= 1;
		
		#10
		reset	<= 0;
		
		
		// Single word read
		#10
		raf_empty_n	<= 1;
		raf_block	<= 0;
		raf_addr	<= {2'b01, 13'h132, 7'h23};
		
		#200
		$finish;
	end	// Sim
	
	
	always @(posedge clock)
	begin
		if (raf_read)
			raf_empty_n	<= 0;
	end
	
	
	wire	start, read, last;
	wire	[1:0]	bank;
	wire	[12:0]	row;
	wire	[7:0]	col;
	scheduler SHED0 (
		.clock_i	(clock),
		.reset_i	(reset),
		.enable_i	(1'b1),
		
		.raf_empty_ni	(raf_empty_n),
		.raf_block_i	(raf_block),
		.raf_read_o	(raf_read),
		.raf_addr_i	(raf_addr),
		
		.wf_empty_ni	(0),
		.wf_block_i	(),
		.wf_read_o	(),
		.wf_addr_i	(),
		
		.cmd_start_o	(start),
		.cmd_read_o	(read),
		.cmd_last_o	(last),	// Auto-precharge after read/write if set
		.cmd_bank_o	(bank),
		.cmd_row_o	(row),
		.cmd_col_o	(col),
		
		.ctl_exec_i	(exec),
		.ctl_active_i	(active),
		
		.rfc_req_i	(0),	// The controller requests a refresh
		.rfc_ack_o	(),
		.rfc_end_i	()
	);
	
endmodule	// cmd_sequencer
