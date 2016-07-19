`timescale 1ns/1ps

//  _____      _      ____    _____
// |_   _|    / \    |  _ \  |_   _|
//   | |     / _ \   | |_) |   | |
//   | |    / ___ \  |  _ <    | |
//   |_|   /_/   \_\ |_| \_\   |_|
//

`include "tartcfg.v"

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
   input              SPI_SCK,
   input              SPI_SSEL,
   input              SPI_MOSI,
   output wire        SPI_MISO,
  
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

   //  Simulation settings:
   parameter DELAY = 3;

   wire               reset_n, reset;

   assign led = tart_state >= 2; // asserted when data can be read back


   //-------------------------------------------------------------------------
   //     GENERATE DIFFERENT CLOCK DOMAINS
   //-------------------------------------------------------------------------
   (* PERIOD = "10.18 ns" *)
   wire               fpga_clk;

`ifdef __USE_OLD_CLOCKS
   tart_clk_generator clknetwork
     (
      .CLKIN(rx_clk_16),               // 16.368 MHZ
      .CLKOUT0(rx_clk_16_buffered),    // 16.368 MHZ buffered
      .CLKOUT1(fpga_clk),               // 16.368x6 = 98.208 MHz
      .reset_n(reset_n)
      );
`else
   (* PERIOD = "5.091 ns" *)
   wire               clk_x;

   tart_dcm TART_DCM0
     ( .clk_pin(rx_clk_16),               // 16.368 MHZ
       .clk_rst(0),

       .clk(rx_clk_16_buffered), // 16.368 MHZ buffered
       .reset_n(reset_n),
       .status_n(status_n),
       .clk6x(fpga_clk),         // 16.368x6 = 98.208 MHz
       .clk12x(clk_x)         // 16.368x6 = 98.208 MHz
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
   fake_telescope #( .WIDTH(24) ) FAKE_TART0
     ( .write_clk(rx_clk_16_buffered), .write_data(fake_antenna) );

   // Antenna source MUX, for choosing real data or fake data
   wire        spi_debug;
   wire [23:0] antenna_data = spi_debug ? fake_antenna : real_antenna;

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
   SCHEDULER0
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
   //  Visibilities/correlator settings.
   parameter ACCUM = `ACCUM_BITS; // Bit-width of the accumulators
   parameter BLOCK = ACCUM;       // Maximum #bits of the block-size
   parameter MSB   = BLOCK-1;     // Data transfer MSB
   parameter TRATE = `TMUX_RATE;  // Time-multiplexing rate
   parameter COUNT = `VISB_LOG2;  // (1 << 3) - 1;
   parameter NREAD = `READ_COUNT; // Number of visibilities to read back
   parameter RBITS = `READ_BITS;  // = ceiling{log2(NREAD)};
   parameter RSB   = RBITS-1;     // MSB of read-back address

   //  Wishbone settings.
   parameter BBITS = `WBBUS_BITS; // Bit-width of the SoC Wishbone bus
   parameter BSB   = BBITS-1;     // Bus data MSB
   parameter WSB   = BBITS-2;     // SPI -> WB bus address-width
   parameter ABITS = `WBADR_BITS; // Correlator bus address bit-width
   parameter ASB   = ABITS-1;     // Address MSB
   parameter XBITS = `BLOCK_BITS; // Bit-width of the block-counter
   parameter XSB   = XBITS-1;     // MSB of the block-counter

   //  SPI -> WB bus signals.
   wire [BSB:0] b_dtx, b_drx;   // bus master's signals
   wire [WSB:0] b_adr;
   wire         b_cyc, b_stb, b_we, b_ack;

   wire [BSB:0] r_drx, r_dtx;   // reset handler's signals
   wire         r_stb, r_ack;

   wire [BSB:0] a_drx, a_dtx;   // data-aquisition controller's signals
   wire [2:0]   a_adr = b_adr[2:0];
   wire         a_stb, a_ack;

   reg          r_sel = 0, a_sel = 0;

   //  WB signals for the streaming interface to the visibilities data.
   wire [BSB:0] s_dat;
   wire [XSB:0] v_blk;
   wire         s_cyc, s_stb, s_we, s_ack;

   //  Aquisition-unit signals.
   wire         newblock, streamed, accessed, available;
   wire [MSB:0] checksum, blocksize;

   //  Remap system signals to WB signals.
   assign b_clk = fpga_clk;
   assign b_rst = reset;


   //-------------------------------------------------------------------------
   //     TRANSMISSION BLOCK
   //     SPI SLAVE & WB MASTER
   //-------------------------------------------------------------------------
   wire debug_spi = oflow || uflow;
   wire spi_busy;
   wire [7:0] spi_status = {1'b1, debug_spi, request_from_spi, spi_start_aq,
                            spi_debug, tart_state[2:0]};

   assign r_dtx = b_drx;        // redirect output-data to slaves
   assign a_dtx = b_drx;

   //  Address decoders for the Wishbone(-like) bus:
   assign a_stb = b_adr[6:3] == 4'h0 && b_stb; // decoder for aquire
   assign r_stb = b_adr[6:2] == 5'h03 && b_stb; // address decoder for reset unit

   assign b_ack = r_ack || a_ack;
 `ifdef __icarus
   assign b_dtx = r_stb || r_sel ? r_drx : (a_stb || a_sel ? a_drx : 'bz);
 `else
   assign b_dtx = r_stb || r_sel ? r_drx : a_drx;
 `endif

   //-------------------------------------------------------------------------
   //  Keep the selected device active until the transaction has been
   //  acknowledged.
   always @(posedge b_clk)
     if (b_rst || !b_cyc)
       {a_sel, r_sel} <= #DELAY 2'b00;
     else begin
        r_sel <= #DELAY r_sel ? !r_ack || r_stb : r_stb;
        a_sel <= #DELAY a_sel ? !a_ack || a_stb : a_stb;
     end


   //-------------------------------------------------------------------------
   //     SPI SLAVE CORE with a WISHBONE(-like) INTERCONNECT
   //-------------------------------------------------------------------------
   spi_slave #( .WIDTH(BBITS) ) SPI_SLAVE0
     ( .clk_i(b_clk),
       .rst_i(b_rst),
       .cyc_o(b_cyc),
       .stb_o(b_stb),
       .we_o (b_we),
       .ack_i(b_ack),
       .adr_o(b_adr),
       .dat_i(b_dtx),
       .dat_o(b_drx),

       .active_o(spi_busy),
       .status_i(spi_status),
       .overflow_o(oflow),
       .underrun_o(uflow),
       
       .SCK_pin(SPI_SCK),
       .MOSI(SPI_MOSI),
       .MISO(SPI_MISO),
       .SSEL(SPI_SSEL)
       );

   //-------------------------------------------------------------------------
   //     RESET HANDLER
   //-------------------------------------------------------------------------
   tart_control #( .WIDTH(BBITS), .RTIME(4) ) TART_CONTROL0
     ( .clk_i(b_clk),
       .rst_i(b_rst),
       .cyc_i(b_cyc),
       .stb_i(r_stb),
       .we_i (b_we),
       .ack_o(r_ack),
       .adr_i(b_adr[1:0]),
       .dat_i(r_dtx),
       .dat_o(r_drx),

       .status_i(spi_status),
       .overflow_i(oflow),
       .underrun_i(uflow),
       .reset_ni(reset_n),
       .reset_o (reset)
       );


   //-------------------------------------------------------------------------
   //     DATA-AQUISITION CONTROL AND READ-BACK.
   //-------------------------------------------------------------------------
   tart_aquire #( .WIDTH(BBITS), .ACCUM(ACCUM) ) TART_AQUIRE0
     ( // WB-like bus between SPI and TART's data:
       .clk_i(b_clk),
       .rst_i(reset),
       .cyc_i(b_cyc),
       .stb_i(a_stb),
       .we_i (b_we),
       .ack_o(a_ack),
       .adr_i(a_adr),
       .dat_i(a_dtx),
       .dat_o(a_drx),

       //  DRAM (streaming) read-back signals:
       .data_ready  (data_out_ready),
       .data_request(request_from_spi),
       .data_in     (data_out[23:0]),

       .spi_busy(spi_busy),

       //  Visibilities status & data access:
       .vx_cyc_o(s_cyc),     // WB-like bus for visibilities read-back
       .vx_stb_o(s_stb),
       .vx_we_o (s_we ),
       .vx_ack_i(s_ack),
       .vx_blk_o(v_blk),
       .vx_dat_i(s_dat),
       .newblock(newblock), // strobes when new block ready
       .streamed(streamed), // has an entire block finished streaming?
       .accessed(accessed), // asserts once visibilities are read back
       .available(available),
       .checksum(checksum),
       .blocksize(blocksize),

       //  Antenna capture & aquisition controls:
       .aq_debug_mode(spi_debug),
       .aq_enabled(spi_start_aq),
       .aq_sample_delay(data_sample_delay)
       );


 `ifdef __USE_CORRELATORS
   //-------------------------------------------------------------------------
   //     
   //     CORRELATOR / VISIBILITIES BLOCK.
   //     
   //-------------------------------------------------------------------------

   //  WB signals to access the prefetched visibilities (that have been
   //  stored within a block SRAM).
   wire [BSB:0] v_drx, v_dtx;
   wire [ASB:0] v_adr;
   wire         v_cyc, v_stb, v_bst, v_we, v_ack, v_wat;

   //  WB signals between the visibilities-prefetch logic-core and the block
   //  of correlators.
   wire [RSB+XBITS:0] c_adr;
   wire [MSB:0]       c_drx, c_dtx;
   wire               c_cyc, c_stb, c_bst, c_we, c_ack, c_wat;


   //-------------------------------------------------------------------------
   //  Streaming, visibilities-read-back logic-core.
   //-------------------------------------------------------------------------
   wb_stream
     #(  .WIDTH (BBITS),
         .WORDS (NREAD << 2),
         .WBITS (ABITS),
         .DELAY (DELAY)
         ) WB_STREAM0
       ( .clk_i(b_clk),
         .rst_i(reset),

         .m_cyc_o(v_cyc),       // this bus prefetches visibilities, and
         .m_stb_o(v_stb),       // sequentially
         .m_we_o (v_we),
         .m_bst_o(v_bst),
         .m_ack_i(v_ack),
         .m_wat_i(v_wat),
         .m_adr_o(v_adr),
         .m_dat_i(v_drx),
         .m_dat_o(v_dtx),

         .s_cyc_i(s_cyc),       // visibilities are streamed from here to the
         .s_stb_i(s_stb),       // SPI module
         .s_we_i (s_we),
         .s_bst_i(1'b0),
         .s_ack_o(s_ack),
         .s_dat_i('bx),
         .s_dat_o(s_dat),

         .wrapped(streamed)     // strobes when block has been streamed
         );

   //-------------------------------------------------------------------------
   //  Visibilities read-back unit.
   //-------------------------------------------------------------------------
   tart_visibilities
     #(  .BLOCK (BLOCK),
         .COUNT (NREAD),
         .TRATE (TRATE),
         .DELAY (DELAY)
         ) TART_VISIBILITIES0
       ( .clk_i(b_clk),
         .rst_i(reset),

         .cyc_i(v_cyc),         // this bus accesses the prefetched bank of
         .stb_i(v_stb),         // visibilities -- which are prefetched after
         .we_i (v_we),          // every bank-switch
         .bst_i(v_bst),
         .ack_o(v_ack),
         .wat_o(v_wat),
  `ifdef __USE_SDP_DSRAM
         .adr_i({v_blk, v_adr}),
  `else
         .adr_i(v_adr),
  `endif
         .byt_i(v_dtx),
         .byt_o(v_drx),

         .cyc_o(c_cyc),         // master interface that connects to the
         .stb_o(c_stb),         // correlators, to read back their computed
         .we_o (c_we),          // visibilities
         .bst_o(c_bst),
         .ack_i(c_ack),
//          .wat_i(c_wat),
         .adr_o(c_adr),
         .dat_i(c_drx),
         .dat_o(c_dtx),

         .switching(switching), // strobes at every bank-switch (bus domain)
         .available(newblock),  // strobes when new block is available
         .checksum (checksum)   // computed checksum of block
         );

   //-------------------------------------------------------------------------
   //     TOP-LEVEL CORRELATORS-BLOCK FUNCTIONAL UNIT.
   //-------------------------------------------------------------------------
   reg [3:0]    q_cnt = 0;
   reg          strobe = 0;

   always @(posedge clk_x)
     if (reset) q_cnt <= #DELAY 0;
     else       q_cnt <= #DELAY q_cnt == TRATE-1 ? 0 : q_cnt + 1;

   always @(posedge clk_x)
     strobe <= #DELAY q_cnt == TRATE >> 1;

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
         .dat_i(c_dtx),
         .dat_o(c_drx),

         .enable(spi_start_aq), // begins correlating once asserted
         .blocksize(blocksize), // number of samples per visibility sum
         .strobe(strobe),       // indicates arrival of a new sample
         .antenna(antenna),     // antenna data
         .switch(switching)     // asserts on bank-switch (sample domain)
         );
 `endif // __USE_CORRELATORS

`else //  __USE_WISHBONE_CORES

   //-------------------------------------------------------------------------
   //     RESET AND STATUS LOGIC
   //-------------------------------------------------------------------------
   wire               spi_reset;
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
   assign led = tart_state >= 2;
   // assign led = ~SPI_SSEL;


   //-------------------------------------------------------------------------
   //     TRANSMISSION BLOCK
   //     SPI SLAVE & WB MASTER
   //     
   //     OBSOLETE: Replacing with individual cores.
   //-------------------------------------------------------------------------
   wire spi_status = {1'b1, debug_o, request_from_spi, spi_start_aq, spi_debug, tart_state[2:0]};

   tart_spi
     #( .ADDR_READ_DATA1  (4'h0),
        .ADDR_READ_DATA2  (4'h1),
        .ADDR_READ_DATA3  (4'h2),
        .ADDR_SAMPLE_DELAY(4'h5),
        .ADDR_DEBUG       (4'h6),
        .ADDR_STARTAQ     (4'h7),
        .ADDR_STATUS      (4'he),
        .ADDR_RESET       (4'hf)
        ) TART_SPI0
       (.clk(fpga_clk),
        .rst(reset),
        
        .data_ready  (data_out_ready),
        .data_request(request_from_spi),
        .data_in     (data_out[23:0]),

        .debug_o(debug_o),

        .spi_status(spi_status),
			  .data_sample_delay(data_sample_delay),
        .spi_reset(spi_reset),
        .spi_start_aq(spi_start_aq),
        .spi_debug(spi_debug),
        
        .SCK (SPI_SCK),
        .MOSI(SPI_MOSI),
        .MISO(SPI_MISO),
        .SSEL(SPI_SSEL)
        );
`endif // !__USE_WISHBONE_CORES


endmodule // tart
