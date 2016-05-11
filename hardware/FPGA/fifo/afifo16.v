`timescale 1ns/100ps
module afifo16 (
		reset_ni,
		
		rd_clk_i,
		rd_en_i,
		rd_data_o,
		
		wr_clk_i,
		wr_en_i,
		wr_data_i,
		
		rempty_o,
		wfull_o
	);
	
	parameter	WIDTH	= 16;
	
	input	reset_ni;
	
	input	rd_clk_i;
	input	rd_en_i;
	output	[WIDTH-1:0]	rd_data_o;
	
	input	wr_clk_i;
	input	wr_en_i;
	input	[WIDTH-1:0]	wr_data_i;
	
	output	rempty_o;
	output	wfull_o;
	
	fifo_dc_gray #(WIDTH,4,16) FIFO0 (
		.rd_clk		(rd_clk_i),
		.wr_clk		(wr_clk_i),
		.rst		(reset_ni),
		.clr		(0),
		.din		(wr_data_i),
		.we		(wr_en_i),
		
		.dout		(rd_data_o),
		.re		(rd_en_i),
		.full		(wfull_o),
		.empty		(rempty_o),
		.wr_level	(),
		.rd_level	()
	);
	
endmodule	// afifo16
