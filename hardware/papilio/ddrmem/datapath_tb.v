`timescale 1ns/100ps
module datapath_tb;
	
	reg	clock	= 1;
	always	#5	clock	<= ~clock;
	
	reg	reset	= 0;
	
	reg	[31:0]	data_to;
	reg	[1:0]	owner_in;
	
	wire	[31:0]	data_from;
	wire	[1:0]	owner_out;
	wire	ready;
	
	reg	start	= 0;
	reg	suspend	= 0;
	reg	read	= 0;
	reg	write	= 0;
	
	wire	ddr_send;
	wire	[31:0]	ddr_data_to;
	reg	[31:0]	ddr_data_from;
	
	
	initial begin : Sim
		$write ("%% ");	// Prevent from breaking my display tool
		$dumpfile ("testdp.vcd");
		$dumpvars;
		
		#5
		// data_in		<= 129;
		owner_in	<= 1;
		start		<= 1;	// ACTIVE
		
		#10
		start		<= 0;
		
		#10
		read		<= 1;	// READ
		
		#10
		read		<= 0;
		
		#20
		start		<= 1;
		
		#10
		start		<= 0;
		
		#10
		write		<= 1;
		data_to		<= 1298;
		ddr_data_from	<= 2543;
		
		#10
		write		<= 0;
		data_to		<= 'bx;
		ddr_data_from	<= 'bx;
		
		#50
		$finish;
	end	// Sim
	
	
	datapath DP0 (
		.clock_i	(clock),
		.reset_i	(reset),
		
		.usr_data_i	(data_to),
		.usr_owner_i	(owner_in),
		.usr_data_o	(data_from),
		.usr_owner_o	(owner_out),
		.usr_ready_o	(ready),
		
		.ctl_start_i	(start),
		.ctl_block_i	(block),
		.ctl_suspend_i	(suspend),
		.ctl_read_i	(read),
		.ctl_write_i	(write),
		
		.ddr_send_o	(ddr_send),
		.ddr_data_o	(ddr_data_to),
		.ddr_data_i	(ddr_data_from)
	);
	
endmodule	// datapath_tb
