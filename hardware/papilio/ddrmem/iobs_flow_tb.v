`timescale 1ns/100ps
module iobs_flow_tb;

reg	clock	= 1;
always	#5 clock	<= ~clock;

reg	reset_n;

reg	[1:0]	dm	= 2'b11;
reg	send	= 0;
wire	[1:0]	dqs;

wire	[1:0]	ddr_dm;
wire	[1:0]	ddr_dqs;


initial begin : Sim
	$write ("%% ");
	$dumpfile ("iobstb.vcd");
	$dumpvars;
	
	#1
	reset_n	<= 0;
	
	#20
	reset_n	<= 1;
	
	#20
	dm	<= 2'b00;
	send	<= 1;
	
	#10
	send	<= 0;
	
	#40
	$finish;
end	// Sim


iobs_flow FLOWIOBS0 (
	.clock_i	(clock),
	.reset_ni	(reset_n),
	
	.ctl_dm_i	(dm),
	.ctl_dqs_i	({send, send}),
	.ctl_dqs_o	(dqs),
	
	.ddr_dm_o	(ddr_dm),
	.ddr_dqs_io	(ddr_dqs)
);


endmodule	// iobs_flow_tb
