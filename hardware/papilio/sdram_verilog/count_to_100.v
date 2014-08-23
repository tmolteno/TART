`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    09:22:17 08/23/2014 
// Design Name: 
// Module Name:    count_to_100 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////

module count_to_100(
   input fpga_clk_32,
   output wire led,
	output wire SDRAM_CLK,
	output wire SDRAM_CKE,
	output wire SDRAM_CS,
	output wire SDRAM_RAS,
	output wire SDRAM_CAS,
	output wire SDRAM_WE,
	output wire [1:0] SDRAM_DQM,
	output wire [12:0] SDRAM_ADDR,
	output wire [1:0] SDRAM_BA,
   inout  wire [15:0] SDRAM_DQ
   );
  
  
   dcm clkmgr
   (.CLK_IN1(fpga_clk_32),      // IN
    .CLK_OUT1(clk96));          // OUT

   parameter SDRAM_ADDRESS_WIDTH = 22;
	parameter SDRAM_COLUMN_BITS   = 8;
	parameter SDRAM_STARTUP_CYCLES= 10100;
	parameter CYCLES_PER_REFRESH  = (64000*100)/4196-1; 


	// Inputs
	reg reset;

	reg cmd_enable;
	reg cmd_wr;
	reg [SDRAM_ADDRESS_WIDTH-2:0] cmd_address;
	reg [31:0] cmd_data_in;


	// Outputs
	wire cmd_ready;
	wire [31:0] data_out;
   wire data_out_ready;
   
   assign led = (data_out==32'hdeadbeef);


	SDRAM_Controller #(
      .sdram_address_width(SDRAM_ADDRESS_WIDTH),
      .sdram_column_bits   (SDRAM_COLUMN_BITS),
      .sdram_startup_cycles(SDRAM_STARTUP_CYCLES),
      .cycles_per_refresh  (CYCLES_PER_REFRESH)
    )
	 uut(
		.clk(clk96), 
		.reset(reset), 
		.cmd_ready(cmd_ready), 
		.cmd_enable(cmd_enable), 
		.cmd_wr(cmd_wr), 
		.cmd_address(cmd_address), 
		.cmd_byte_enable(4'b1111), 
		.cmd_data_in(cmd_data_in), 
		.data_out(data_out), 
		.data_out_ready(data_out_ready), 
		.SDRAM_CLK(SDRAM_CLK), 
		.SDRAM_CKE(SDRAM_CKE), 
		.SDRAM_CS(SDRAM_CS), 
		.SDRAM_RAS(SDRAM_RAS), 
		.SDRAM_CAS(SDRAM_CAS), 
		.SDRAM_WE(SDRAM_WE), 
		.SDRAM_DQM(SDRAM_DQM), 
		.SDRAM_ADDR(SDRAM_ADDR), 
		.SDRAM_BA(SDRAM_BA), 
		.SDRAM_DATA(SDRAM_DQ)
	);

	reg [SDRAM_ADDRESS_WIDTH-2:0] wr_address = 21'b0;
	reg [31:0] data_to_write = 0;
	
   reg [SDRAM_ADDRESS_WIDTH-2:0] rd_address = 21'b0;
   reg [31:0] read_data;
   
   reg reading = 0;
	reg writing = 0;
   reg [16:0] init_delay = 1;
   reg [26:0] delay = 27'b0;

	always @(posedge clk96)
    begin 
       delay <= delay+1'b1;
       if (writing==0) init_delay   <= init_delay + 1'b1;
       if (init_delay==0) writing   <= 1;
       if (wr_address > 99 && delay==0) reading <= 1;
       else reading <= 0;

       if (cmd_ready)
         if (cmd_enable)
          begin
            cmd_enable <= 0; // avoid issuing two commands back2back
          end
         else
            begin
               if (writing && (wr_address < 100))
                 begin
                  cmd_enable    <= 1;
                  cmd_wr        <= 1;
                  cmd_data_in   <= (wr_address[0]) ? 32'hdeadbeef : 32'b0;
                  cmd_address   <= wr_address;
                  wr_address    <= wr_address+1;
                  $display("WRITE %b", cmd_data_in);
                 end
               else if (reading && (rd_address < 100))
                 begin
                  cmd_enable    <= 1;
                  cmd_wr        <= 0;
                  cmd_address   <= rd_address;
                  rd_address    <= rd_address+1;
                 end
           end
   end
    
endmodule
