`timescale 1ns/1ps

//  _____      _      ____    _____
// |_   _|    / \    |  _ \  |_   _|
//   | |     / _ \   | |_) |   | |
//   | |    / ___ \  |  _ <    | |
//   |_|   /_/   \_\ |_| \_\   |_|

module tart
  (
	 // SHOW DELAYED CLOCK:
	 output wire        rx_clk_test_pin,
     
   // PAPILIO
   output wire        led,
  
   // SDRAM
   output wire        SDRAM_CLK,
   output wire        SDRAM_CKE,
   output wire        SDRAM_CS,
   output wire        SDRAM_RAS,
   output wire        SDRAM_CAS,
   output wire        SDRAM_WE,
   output wire [1:0]  SDRAM_DQM,
   output wire [12:0] SDRAM_ADDR,
   output wire [1:0]  SDRAM_BA,
   inout wire [15:0]  SDRAM_DQ,
  
   // SPI
   input              spi_sck,
   input              spi_ssel,
   input              spi_mosi,
   output wire        spi_miso,
  
   // TELESCOPE
   input              rx_clk_16, // 16.368 MHz receiver master clock
   input [23:0]       antenna // Radio Data Interface
   );

`ifdef __512Mb_SDRAM
   parameter SDRAM_COLUMN_BITS    = 10;     // 8 for standard papilio pro
   parameter SDRAM_ADDRESS_WIDTH  = 25;    // 22 for standard papilio pro
   parameter CYCLES_PER_REFRESH   = 780;  // (64000*100)/8192-1 Cycled as  (64ms @ 100MHz)/ 8192 rows
`else
   parameter SDRAM_COLUMN_BITS    = 8;     // 8 for standard papilio pro
   parameter SDRAM_ADDRESS_WIDTH  = 22;    // 22 for standard papilio pro
   parameter CYCLES_PER_REFRESH   = 1524;  // = (64000*100)/4096-1 Cycled as  (64ms @ 100MHz)/ 4096 rows
`endif // !`ifdef __512Mb_SDRAM
   parameter SDRAM_STARTUP_CYCLES = 10100; // -- 100us, plus a little more, @ 100MHz

   //-------------------------------------------------------------------------
   //     RESET AND STATUS LOGIC
   //-------------------------------------------------------------------------
   wire               reset, reset_n, spi_reset;
   reg                reset_0 = 0, reset_1 = 0, reset_r = 0;
   wire               debug_o;

   // TODO: Remove the unwanted combinational delays.
   // TODO: Make sure the reset is asserted for several clock-cycles.
   assign reset = reset_r | ~reset_n;

   always @(posedge fpga_clk)
     begin
        reset_0 <= spi_reset;
        reset_1 <= reset_0;
        reset_r <= reset_1;
     end

   // assign led = debug_o;
   assign led = (tart_state>=2);

   //-------------------------------------------------------------------------
   //     GENERATE DIFFERENT CLOCK DOMAINS
   //-------------------------------------------------------------------------
   //fake_tart_clk clknetwork(.CLK_IN1(fpga_clk_32), .CLK_OUT1(fpga_clk), .CLK_OUT2(fake_rx_clk), .CLK_OUT3(clk320));
   tart_clk_generator clknetwork
     (
      .CLKIN(rx_clk_16),               // 16.368 MHZ
      .CLKOUT0(rx_clk_16_buffered),    // 16.368 MHZ buffered
      .CLKOUT1(fpga_clk),               // 16.368x6 = 98.208 MHz
      .reset_n(reset_n)
      );

   //-------------------------------------------------------------------------
   //     DATA CAPTURE
   //-------------------------------------------------------------------------
   
   //     HOOK UP IO REGISTER TO INTERNAL LOGIC
   reg [23:0] real_antenna;
   always @(posedge fpga_clk) real_antenna <= antenna;
   
   //     GENERATE FAKE DATA (24 BIT COUNTER) FOR DEBUGGING
   wire [23:0] fake_antenna;
   //fake_telescope fake_tart (.write_clk(fake_rx_clk), .write_data(fake_antenna));
   fake_telescope fake_tart (.write_clk(rx_clk_16_buffered), .write_data(fake_antenna));

   //     TRI STATE FOR CHOOSING REAL DATA OR FAKE DATA
   wire        spi_debug;
   wire        sel_rx_clk = rx_clk_16_buffered;
   //wire rx_clk = (spi_debug) ? fake_rx_clk  : rx_clk_16;
   wire [23:0] sel_antenna_data = (spi_debug) ? fake_antenna : real_antenna;

   wire [23:0] antenna_data;
   wire rx_clk;
   //sync_antennas_to_clock sync_ant_int(
   // .fast_clk(fpga_clk),
   // .data_in(sel_antenna_data),
   // .slow_clk(rx_clk),
   // .data_out(antenna_data)  // data valid on the rising edge of the clock.
   // );
   wire [2:0] data_sample_delay;

	 delay_data_sampling_clk delay_rx_clk
     (
	    .fast_clk(fpga_clk),
		  .data_sample_delay(data_sample_delay),
		  .slow_clk(rx_clk)
	    );
	 assign rx_clk_test_pin = rx_clk;
	
   // assign rx_clk = sel_rx_clk;
   
	assign antenna_data = sel_antenna_data;

   //-------------------------------------------------------------------------
   //     AQUISITION BLOCK
   //-------------------------------------------------------------------------
   wire [23:0] aq_write_data;
   wire [23:0] aq_read_data;
   wire [8:0] aq_bb_rd_address;
   wire [8:0] aq_bb_wr_address;

   block_buffer aq_bb
     (
      .read_data(aq_read_data),
      .write_data(antenna_data),
      .clk(fpga_clk),
      .write_address(aq_bb_wr_address),
      .read_address(aq_bb_rd_address)
      );

   //-------------------------------------------------------------------------
   //      STORAGE BLOCK
   //-------------------------------------------------------------------------
   wire [SDRAM_ADDRESS_WIDTH-2:0] cmd_address;
   wire [2:0] tart_state;

   wire [31:0] cmd_data_in;
   wire [31:0] data_out;

   wire request_from_spi;
  
   fifo_sdram_fifo_scheduler
     #(.SDRAM_ADDRESS_WIDTH(SDRAM_ADDRESS_WIDTH))
   scheduler
     ( .clk(rx_clk),
       .clk6x(fpga_clk),
       .rst(reset),

       .aq_bb_wr_address(aq_bb_wr_address),
       .aq_bb_rd_address(aq_bb_rd_address),
       .aq_read_data(aq_read_data),

       .spi_start_aq(spi_start_aq),
       .spi_buffer_read_complete(request_from_spi),

       .cmd_data_in(cmd_data_in),
       .cmd_ready(cmd_ready),
       .cmd_enable(cmd_enable),
       .cmd_wr(cmd_wr),
       .cmd_address(cmd_address),
       .tart_state(tart_state)
       );

   SDRAM_Controller_v
   #(
      .sdram_address_width(SDRAM_ADDRESS_WIDTH),
      .sdram_column_bits(SDRAM_COLUMN_BITS),
      .sdram_startup_cycles(SDRAM_STARTUP_CYCLES),
      .cycles_per_refresh(CYCLES_PER_REFRESH)
   )
   hamster_sdram(
      .clk(fpga_clk),
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

   //-------------------------------------------------------------------------
   //     TRANSMISSION BLOCK
   //     SPI SLAVE
   //-------------------------------------------------------------------------
   tart_spi TART_SPI0
     ( .clk(fpga_clk),
       .rst(reset),
      
       .data_ready  (data_out_ready),
       .data_request(request_from_spi),
       .data_in     (data_out[23:0]),

       .debug_o(debug_o),

       .spi_status({1'b1, debug_o, request_from_spi, spi_start_aq, spi_debug, tart_state[2:0]}),
			 .data_sample_delay(data_sample_delay),
       .spi_reset(spi_reset),
       .spi_start_aq(spi_start_aq),
       .spi_debug(spi_debug),
      
       .SCK (spi_sck),
       .MOSI(spi_mosi),
       .MISO(spi_miso),
       .SSEL(spi_ssel)
       );
   
endmodule // tart

