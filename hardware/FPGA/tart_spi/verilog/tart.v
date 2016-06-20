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
   // assign led = ~spi_ssel;

   //-------------------------------------------------------------------------
   //     GENERATE DIFFERENT CLOCK DOMAINS
   //-------------------------------------------------------------------------
// `define __USE_OLD_CLOCKS
`ifdef __USE_OLD_CLOCKS
   tart_clk_generator clknetwork
     (
      .CLKIN(rx_clk_16),               // 16.368 MHZ
      .CLKOUT0(rx_clk_16_buffered),    // 16.368 MHZ buffered
      .CLKOUT1(fpga_clk),               // 16.368x6 = 98.208 MHz
      .reset_n(reset_n)
      );
`else
   tart_dcm TART_DCM0
     ( .clk_pin(rx_clk_16),               // 16.368 MHZ
       .clk_rst(0),

       .clk(rx_clk_16_buffered), // 16.368 MHZ buffered
       .reset_n(reset_n),
       .status_n(status_n),
       .clk6x(fpga_clk)         // 16.368x6 = 98.208 MHz
       );
`endif // __USE_OLD_CLOCKS

   
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


`ifdef __USE_WISHBONE_CORES
   //-------------------------------------------------------------------------
   //
   //  TART's system-wide, Wishbone-like interconnect and peripherals.
   //
   //-------------------------------------------------------------------------
   parameter WIDTH = 8;         // bus parameters
   parameter MSB   = WIDTH-1;
   parameter ASB   = WIDTH-2;
   parameter DELAY = 3;

   parameter ACCUM = 32;        // correlator/visibilities parameters
   parameter BLOCK = ACCUM;
   parameter BSB   = BLOCK-1;

   wire [MSB:0] b_dtx, b_drx;   // bus master's signals
   wire [ASB:0] b_adr;
   wire         b_clk = fpga_clk;
   wire         b_rst = 1'b0;
   wire         b_cyc, b_stb, b_bst, b_we, b_ack;

   wire [MSB:0] r_drx, r_dtx;   // reset handler's signals
   wire         r_stb, r_ack;

   wire [MSB:0] a_drx, a_dtx;   // data-aquisition controller's signals
   wire [2:0]   a_adr = b_adr[2:0];
   wire         a_stb, a_ack;

   assign r_dtx = b_drx;        // redirect output-data to slaves
   assign a_dtx = b_drx;

   assign a_stb = b_adr[6:3] == 4'h0 && b_stb; // decoder for aquire
   assign r_stb = b_adr == 7'h0f && b_stb; // address decoder for reset unit


   //-------------------------------------------------------------------------
   //     TRANSMISSION BLOCK
   //     SPI SLAVE & WB MASTER
   //-------------------------------------------------------------------------
   spi_slave #( .WIDTH(WIDTH) ) SPI_SLAVE0
     ( .clk_i(b_clk),
       .rst_i(b_rst),
       .cyc_o(b_cyc),
       .stb_o(b_stb),
       .bst_o(b_bst),
       .we_o (b_we),
       .ack_i(b_ack),
       .adr_o(b_adr),
       .dat_i(b_dtx),
       .dat_o(b_drx),

       .status_i(stat),
       .overflow_o(oflow),
       .underrun_o(uflow),
       
       .SCK_pin(spi_sck),
       .MOSI(spi_mosi),
       .MISO(spi_miso),
       .SSEL(spi_ssel)
       );

   //-------------------------------------------------------------------------
   //     RESET HANDLER
   //-------------------------------------------------------------------------
   wb_reset #( .WIDTH(WIDTH), .RTIME(4) ) SPI_RESET0
     ( .clk_i(b_clk),
       .rst_i(b_rst),
       .cyc_o(b_cyc),
       .stb_o(r_stb),
       .we_o (b_we),
       .ack_i(r_ack),
       .dat_i(r_dtx),
       .dat_o(r_drx),

       .reset_ni(reset_n),
       .reset_o(reset)
       );

   //-------------------------------------------------------------------------
   //     DATA-AQUISITION CONTROL AND READ-BACK.
   //-------------------------------------------------------------------------
   wb_aquire #( .WIDTH(WIDTH) ) SPI_AQUIRE0
     ( .clk_i(b_clk),
       .rst_i(reset),
       .cyc_o(b_cyc),
       .stb_o(a_stb),
       .we_o (b_we),
       .ack_i(a_ack),
       .adr_i(a_adr),
       .dat_i(a_dtx),
       .dat_o(a_drx),

       .aq_debug_mode(spi_debug),
       .aq_enabled(spi_start_aq),
       .aq_sample_delay(data_sample_delay)
       );


   //-------------------------------------------------------------------------
   //     
   //     CORRELATOR / VISIBILITIES BLOCK.
   //     
   //-------------------------------------------------------------------------
   //  Correlator functional unit.
   //-------------------------------------------------------------------------
   tart_correlator
     #(  .BLOCK (BLOCK),
         .DELAY (DELAY)
         ) TART_CORRELATOR0
       ( .clk_x(clk_x),         // 12x data-rate sampling clock
         .rst  (reset),
         .clk_i(b_clk),

         .cyc_i(c_cyc),         // the correlator connects to the read-back
         .stb_i(c_stb),         // unit for the visibilities, via this bus
         .we_i (c_we),
         .bst_i(c_bst),
         .ack_o(c_ack),
         .adr_i(c_adr),
         .dat_i(c_rdx),
         .dat_o(c_tdx),

         .enable(en),           // begins correlating once asserted
         .strobe(strobe),       // indicates arrival of a new sample
         .antenna(antenna),     // antenna data
         .switch(sw)            // asserts on bank-switch (sample domain)
         );

   //-------------------------------------------------------------------------
   //  Visibilities read-back unit.
   //-------------------------------------------------------------------------
   tart_visibilities
     #(  .BLOCK (BLOCK),
         .DELAY (DELAY)
         ) TART_VISIBILITIES0
       ( .clk_i(b_clk),
         .rst_i(reset),

         .cyc_i(v_cyc),         // this bus accesses the prefetched bank of
         .stb_i(v_stb),         // visibilities -- which are prefetched after
         .we_i (v_we),          // every bank-switch
         .bst_i(v_bst),
         .ack_o(v_ack),
         .adr_i(v_adr),
         .dat_i(v_tdx),
         .dat_o(v_rdx),

         .cyc_o(c_cyc),         // master interface that connects to the
         .stb_o(c_stb),         // correlators, to read back their computed
         .we_o (c_we),          // visibilities
         .bst_o(c_bst),
         .ack_i(c_ack),
         .adr_o(c_adr),
         .dat_i(c_tdx),
         .dat_o(c_rdx),

         .switched(switched),   // strobes at every bank-switch (bus domain)
         .accessed(accessed)    // asserts once visibilities are read back
         );

   //-------------------------------------------------------------------------
   //  Streaming, visibilities-read-back logic-core.
   //-------------------------------------------------------------------------
   wb_stream
     #(  .BLOCK (BLOCK),
         .DELAY (DELAY)
         ) WB_STREAM0
       ( .clk_i(b_clk),
         .rst_i(reset),

         .m_cyc_o(v_cyc),       // this bus prefetches visibilities, and
         .m_stb_o(v_stb),       // sequentially
         .m_we_o (v_we),
         .m_bst_o(v_bst),
         .m_ack_i(v_ack),
         .m_adr_o(v_adr),
         .m_dat_i(v_tdx),
         .m_dat_o(v_rdx),

         .s_cyc_i(b_cyc),       // visibilities are streamed from here to the
         .s_stb_i(s_stb),       // SPI module
         .s_we_i (b_we),
         .s_bst_i(s_bst),
         .s_ack_o(s_ack),
         .s_dat_i(s_tdx),
         .s_dat_o(s_rdx)
         );


`else // !__USE_WISHBONE_CORES
   //-------------------------------------------------------------------------
   //     TRANSMISSION BLOCK
   //     SPI SLAVE & WB MASTER
   //     
   //     OBSOLETE: Replacing with individual cores.
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
`endif // __USE_WISHBONE_CORES


endmodule // tart
