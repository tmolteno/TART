`timescale 1ns/100ps
module controller_tb;
	
	reg	clock	= 1;
	always	#5	clock	<= ~clock;
	
	reg	reset	= 1;
	
	// Signals to the DDR SDRAM
	wire	ddr_cke;
	wire	ddr_cs_n;
	wire	ddr_ras_n;
	wire	ddr_cas_n;
	wire	ddr_we_n;
	wire	[1:0]	ddr_ba;
	wire	[12:0]	ddr_a;
	
	wire	ap	= ddr_a [10];	// Auto-Precharge
	
	wire	init_done;
	
	reg	cmd_start	= 0;
	reg	cmd_read	= 0;
	reg	cmd_last	= 0;
	wire	cmd_exec;
	wire	cmd_active;
	reg	[1:0]	cmd_bank	= 0;
	reg	[12:0]	cmd_row		= 0;
	reg	[7:0]	cmd_col		= 0;
	
	wire	data_read_o;
	wire	data_write_o;
	
	wire	rfc_req;
	reg	rfc_start	= 0;
	wire	rfc_done;
	
	
	initial begin : Sim
/*		$write ("%% ");
		$dumpfile ("cntrl.vcd");
		$dumpvars;// (1, CNTRL0, ddr_cke);
		*/
		#15
		reset	<= 0;
		
		#5
		while (!init_done) #10 ;
		
		$write ("%% ");
		$dumpfile ("cntrl.vcd");
		$dumpvars;// (1, CNTRL0, ddr_cke);
		
		
/*		$display ("Time CLK RST  start read last  RAS# CAS# WE# AP");
		$monitor ("%5t  %b  %b   %b    %b   %b    %b   %b   %b  %b",
			$time, clock, reset,
			cmd_start, cmd_read, cmd_last,
			ddr_ras_n, ddr_cas_n, ddr_we_n, ap
		);
		*/
		
		// Issue a read command
		#10
		cmd_start	<= 1;
		cmd_read	<= 1;
		
		#10
		cmd_start	<= 0;
		
		#30
		
		#10
		cmd_last	<= 1;
		
		
		#100
		$finish;
	end	// Sim
	
	
	always @(posedge clock)
	begin
		if (cmd_exec)
			cmd_col		<= cmd_col + 1;
	end
	
	
	controller CNTRL0 (
		.clock_i	(clock),	// 133 MHz system clock
		.reset_i	(reset),	// Goes high once signal
		
		.ddr_cke_o	(ddr_cke),	// DDR controls and address signals
		
		.ddr_cs_no	(ddr_cs_n),
		.ddr_ras_no	(ddr_ras_n),
		.ddr_cas_no	(ddr_cas_n),
		.ddr_we_no	(ddr_we_n),
		
		.ddr_ba_o	(ddr_ba),
		.ddr_a_o	(ddr_a),
		
		.init_done_o	(init_done),
		
		.cmd_start_i	(cmd_start),	// Commands from the sequencing logic
		.cmd_read_i	(cmd_read),
		.cmd_last_i	(cmd_last),
		.cmd_exec_o	(cmd_exec),
		.cmd_active_o	(cmd_active),
		.cmd_bank_i	(cmd_bank),
		.cmd_row_i	(cmd_row),
		.cmd_col_i	(cmd_col),
		
		.data_read_o	(data_read),
		.data_write_o	(data_write),
		
		.rfc_req_o	(rfc_req),
		.rfc_start_i	(rfc_start),
		.rfc_done_o	(rfc_done)
	);
	
endmodule	// controller_tb
