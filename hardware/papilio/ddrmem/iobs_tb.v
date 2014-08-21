// This should be able to test all IOB configs, irregardless of the
// underlying hardware.
`timescale 1ns/100ps
module iobs_tb;
	
	reg	clock	= 1;
	always	#5	clock	<= ~clock;
	
	reg	reset	= 0;
	
	reg	cke	= 0;	// Control signals
	reg	cs_n	= 1;
	reg	ras_n	= 1;
	reg	cas_n	= 1;
	reg	we_n	= 1;
	
	reg	[1:0]	ba	= 0;	// Address signals
	reg	[12:0]	a	= 0;
	
	reg	[3:0]	dm	= 0;	// Data signals
	reg	[31:0]	data_i	= 0;
	reg	[3:0]	dqs	= 0;
	reg	send	= 0;
	wire	[31:0]	data_o;
	
	
	// DDR clocks.
	wire	ddr_ck, ddr_ck_n;
	
	// DDR control signals.
	wire	ddr_cke, ddr_cs_n, ddr_ras_n, ddr_cas_n, ddr_we_n;
	
	// DDR address signals.
	wire	[1:0]	ddr_ba;
	wire	[12:0]	ddr_a;
	
	// DDR data signals.
	wire	[1:0]	ddr_dm;
	wire	[15:0]	ddr_dq;
	wire	[1:0]	ddr_dqs;
	
	// AUTO PRECHARGE
	// wire	ap	= ddr_a [10];
	
	initial begin : Sim
		
		$write ("%% ");
		$dumpfile ("iobs.vcd");
		$dumpvars;
		
		$display ("Time clock CK CK# RST  CKE CS# RAS# CAS# WE#   BA A ");
		$monitor ("%5t  %b    %b %b  %b   %b  %b  %b   %b   %b    %b %h",
			$time, clock, ddr_ck, ddr_ck_n, reset,
			ddr_cke, ddr_cs_n, ddr_ras_n, ddr_cas_n, ddr_we_n,
			ddr_ba, ddr_a
		);
		
		
		#1
		reset		<= 1;
		
		#10
		cke		<= 1;
		
		#10
		reset		<= 0;
		
		#10
		// ACTIVE command
		cs_n	<= 0;
		ras_n	<= 0;
		ba	<= 2'b01;
		a	<= 13'h023;
		
		#10
		// NOP
		ras_n	<= 1;
		
		#10
		// Read column
		cas_n	<= 0;
		ba	<= 2'b01;
		a	<= 13'h0409;	// AP
		
		#10
		cas_n	<= 1;
		
		
		#200
		$finish;
	end	// Sim
	
	
	iobs IOBS0 (
		.clock_i	(clock),
		.reset_i	(reset),
		
		// Signals to/from DDR controller.
		.cntrl_cke_i	(cke),
		
		.cntrl_cs_ni	(cs_n),
		.cntrl_ras_ni	(ras_n),
		.cntrl_cas_ni	(cas_n),
		.cntrl_we_ni	(we_n),
		
		.cntrl_ba_i	(ba),
		.cntrl_a_i	(a),
		
		.cntrl_dm_i	(dm),
		.cntrl_data_i	(data_i),
		.cntrl_dqs_i	(dqs),
		.cntrl_send_i	(send),
		.cntrl_data_o	(data_o),
		
		// Signals to/from DDR device.
		.ddr_ck_o	(ddr_ck),
		.ddr_ck_no	(ddr_ck_n),
		.ddr_cke_o	(ddr_cke),
		
		.ddr_cs_no	(ddr_cs_n),
		.ddr_ras_no	(ddr_ras_n),
		.ddr_cas_no	(ddr_cas_n),
		.ddr_we_no	(ddr_we_n),
		
		.ddr_ba_o	(ddr_ba),
		.ddr_a_o	(ddr_a),
		
		.ddr_dm_o	(ddr_dm),
		.ddr_dq_io	(ddr_dq),
		.ddr_dqs_io	(ddr_dqs)
	);
	
endmodule	// iobs_tb
