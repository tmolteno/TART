`timescale 1ns/100ps
module iobs_data_tb;

reg	clock	= 1;
always	#5	clock	<= ~clock;

reg	capture	= 0;
reg	send	= 0;

reg	[31:0]	data_to;
wire	[31:0]	data_from;

wire	[15:0]	dq_iobs;


initial begin : Sim
	$write ("%% ");
	$dumpfile ("iobstb.vcd");
	$dumpvars;
	
	#19
	data_to	<= 32'hFFAA_0033;
	send	<= 1;
	
	#10
	send	<= 0;
	
	#100
	$finish;
end	// Sim


iobs_data DATAIOBS (
	.clock_i	(clock),
	
	.send_i		(send),
	.capture_i	({capture, capture}),
	.data_i		(data_to),
	.data_o		(data_from),
	
	.ddr_dq_io	(dq_iobs)
);


endmodule	// iobs_data_tb
