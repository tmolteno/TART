`timescale 1ns/100ps
module sfifo2k_tb;

reg	clock	= 1;
reg	reset_n	= 1;
reg	read	= 0;
reg	write	= 0;

reg	[17:0]	wr_data	= 0;
wire	[17:0]	rd_data;

wire	full;
wire	empty;

always	#5 clock	<= ~clock;


integer	ii;
initial begin : Sim
	$write ("%% ");
	$dumpfile ("tb.vcd");
	$dumpvars;
	
	
	#2
	reset_n	= 0;
	
	#10
	reset_n	= 1;
	
	for (ii=0; ii<20; ii=ii+1) begin
		#10 write	= 1;
		wr_data		= ii;
	end
	
	#10
	read	= 1;
	write	= 0;
	wr_data	= ii;
	
	#220
	read	= 0;
	write	= 0;
	
	#30
	$finish;
end	//	Sim


// 18-bit wide, 1024 entry FIFO.
sfifo2k #(
	.WIDTH	(18),
	.SIZE	(16),
	.PWIDTH	(4)
) FIFO0 (
	.clock_i	(clock),
	.reset_ni	(reset_n),
	.read_i		(read),
	.write_i	(write),
	.data_i		(wr_data),
	.data_o		(rd_data),
	
	.empty_o	(empty),
	.full_o		(full)
);


endmodule	// sfifo2k_tb
