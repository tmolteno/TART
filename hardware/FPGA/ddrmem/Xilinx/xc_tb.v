module xilinx_controller_tb;
	
	reg	clock	= 1;
	always	#5	clock	<= ~clock;
	reg	reset	= 0;
	
	reg	[22:0]	user_addr;
	reg	[1:0]	user_ba;
	reg	config
	reg	[2:0]	command;
	
	controller controller0 (
		.dip1               ( CKE),	// CKE control
		.clk                ( clock), 
		.rst0               ( reset),
		.rst180             ( reset),
		
		.address            ( user_input_address),
		.bank_address       ( user_bank_address),
		.config_register    ( user_config_register),
		.command_register   ( user_command_register),
		.burst_done         ( burst_done),
		
		.ddr_rasb_cntrl     ( ddr_rasb_cntrl),
		.ddr_casb_cntrl     ( ddr_casb_cntrl),
		.ddr_web_cntrl      ( ddr_web_cntrl),
		.ddr_ba_cntrl       ( ddr_ba_cntrl),
		.ddr_address_cntrl  ( ddr_address_cntrl),
		.ddr_cke_cntrl      ( ddr_cke_cntrl),
		.ddr_csb_cntrl      ( ddr_csb_cntrl),
		.dqs_enable         ( dqs_enable),
		.dqs_reset          ( dqs_reset),
		.write_enable       ( write_enable),
		.rst_calib          ( rst_calib),
		.rst_dqs_div_int    ( rst_dqs_div_int),
		.rst_dqs_div_int1   ( rst_dqs_div_int1),
		.cmd_ack            ( user_cmd_ack),
		.init               ( init_val),
		.ar_done            ( ar_done)
		
	);
	
endmodule // xilinx_controller_tb
