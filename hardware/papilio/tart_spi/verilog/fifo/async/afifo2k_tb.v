`timescale 1ns/100ps
module afifo2k_tb;

reg	rclk		= 1;
reg	wclk		= 1;

reg	reset		= 0;
reg	read		= 0;
reg	write		= 0;

reg	[17:0]	wr_data	= 0;
wire	[17:0]	rd_data;

wire	full;
wire	empty;

always	#15	rclk	<= ~rclk;
always	#5	wclk	<= ~wclk;


initial begin : Sim
	$dumpfile ("tb.vcd");
	$dumpvars;
	
	#2
	reset	<= 1;
	
	#30
	reset	<= 0;
	
	#30
	read	<= 0;
	write	<= 0;
	
	#10
	write	<= 1;
// 	wr_data	<= 18'h30F0;
/*		
	#10
	wr_data	<= 18'h0A0A;
	
	#10	// 62
	wr_data	<= $random;
	
	#10
	wr_data	<= $random;
	
	#10
	write	<= 0;
	
	#10	// 92
	read	<= 1;
	
	#30	// 122
	read	<= 0;
	
	#30	// 152
	read	<= 1;
	
	#60	// 212
	read	<= 0;
	*/
	#200
	read	<= 1;
	
	#30
	write	<= 0;
	
	#600
	$finish;
end	//	Sim


// Fill the FIFO.
always @(posedge wclk)
	wr_data	<= #5 $random;


// 18-bit wide, 15 entry FIFO, ultra high-speed FIFO
afifo2k #('d18) FIFO0(
	.reset_ni	(~reset),
	
	.rd_clk_i	(rclk),
	.rd_en_i	(read),
	.rd_data_o	(rd_data),
	
	.wr_clk_i	(wclk),
	.wr_en_i	(write),
	.wr_data_i	(wr_data),
	
	.wfull_o	(full),
	.rempty_o	(empty)
);


endmodule	// afifo2k_tb
