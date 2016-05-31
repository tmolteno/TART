`timescale 1ns/100ps
module testbench;
	
	reg	clock	= 1;
	always	#5	clock	<= ~clock;	// 100 MHz
	
	reg	clock_270	= 0;
	always @(clock)
		clock_270	<= #7.5 clock;
	
/*	wire	clock_270;
	assign	#7.5	clock_270	= clock;
	*/
	reg	reset	= 0;
	
	// Wire connecting the DDR to the controller.
	wire	ddr_clk, ddr_clk_n, ddr_cke;
	wire	ddr_cs_n, ddr_ras_n, ddr_cas_n, ddr_we_n;
	wire	[1:0]	ddr_ba;
	wire	[12:0]	ddr_address;
	wire	[1:0]	ddr_dm;
	wire	[15:0]	ddr_dq;
	wire	[1:0]	ddr_dqs;
	
	// User interface signals.
	reg	rd_req		= 0;
	reg	rd_block	= 0;
	reg	[1:0]	rd_owner_to	= 0;
	wire	rd_busy;
	reg	[22:0]	rd_addr		= 23'd0;	// 32-bit aligned
	wire	[1:0]	rd_owner_from;
	wire	[31:0]	rd_data;
	
	reg	wr_req		= 0;
	reg	wr_block	= 0;
	wire	wr_busy;
	reg	[3:0]	wr_bes_n	= 0;
	reg	[22:0]	wr_addr		= 23'd0;
	reg	[31:0]	wr_data		= 0;
	
	
	initial begin : Sim
		#5
		reset	<= 1;
		
		#30
		reset	<= 0;
		
		#2500	// Initialisation sequence complete
		rd_addr		<= 'bx;
		
		// Create a VCD file
		$write ("%% ");	// Prevent from breaking my display tool
		$dumpfile ("tb.vcd");
		$dumpvars;
		
		#10
		wr_req		<= 1;
		wr_addr		<= 197;
		wr_bes_n	<= 4'b0010;
		wr_data		<= 32'h56_a3_xx_9e;
		
		#10
		wr_req		<= 0;
		wr_addr		<= 'bx;
		wr_bes_n	<= 'bx;
		wr_data		<= 'bx;
		
		// Read some data back. Two back-to-back reads.
		#10
		rd_req		<= 1;
		rd_addr		<= 197;
		
		#10
		rd_addr		<= 198;
		
		#10
		rd_req		<= 0;
		rd_addr		<= 'bx;
		
		#300
		$finish;
	end	// Sim
	
	
	always @(posedge clock)
	begin
		if (rd_ready && !reset)
			$display ("%%\tDATA = %h", rd_data);
	end
	
	
	// The DDR controller module needs a better name.
	ddrtop TOP0 (
		.clock_i	(clock),
		.clock_270i	(clock_270),
		.reset_i	(reset),
		
		// User interface
		.rd_req_i	(rd_req),
		.rd_block_i	(rd_block),
		.rd_owner_i	(rd_owner_to),
		.rd_busy_o	(rd_busy),
		.rd_addr_i	(rd_addr),
		.rd_owner_o	(rd_owner_from),
		.rd_data_o	(rd_data),
		.rd_ready_o	(rd_ready),
		
		.wr_req_i	(wr_req),
		.wr_block_i	(wr_block),
		.wr_busy_o	(wr_busy),
		.wr_bes_ni	(wr_bes_n),
		.wr_addr_i	(wr_addr),
		.wr_data_i	(wr_data),
		
		// DDR pins
		.ddr_clk_po	(ddr_clk),
		.ddr_clk_no	(ddr_clk_n),
		.ddr_cke_o	(ddr_cke),
		
		.ddr_cs_no	(ddr_cs_n),
		.ddr_ras_no	(ddr_ras_n),
		.ddr_cas_no	(ddr_cas_n),
		.ddr_we_no	(ddr_we_n),
		
		.ddr_ba_o	(ddr_ba),
		.ddr_a_o	(ddr_address),
		
		.ddr_dm_o	(ddr_dm),
		.ddr_dq_io	(ddr_dq),
		.ddr_dqs_io	(ddr_dqs)
	);
	
	ddr DDR0 (
		.Dq	(ddr_dq),
		.Dqs	(ddr_dqs),
		.Addr	(ddr_address),
		.Ba	(ddr_ba),
		.Clk	(ddr_clk),
		.Clk_n	(ddr_clk_n),
		.Cke	(ddr_cke),
		.Cs_n	(ddr_cs_n),
		.Ras_n	(ddr_ras_n),
		.Cas_n	(ddr_cas_n),
		.We_n	(ddr_we_n),
		.Dm	(ddr_dm)
	);
	
endmodule	// testbench
