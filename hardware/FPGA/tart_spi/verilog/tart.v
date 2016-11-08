`timescale 1ns/100ps

//  _____      _      ____    _____
// |_   _|    / \    |  _ \  |_   _|
//   | |     / _ \   | |_) |   | |
//   | |    / ___ \  |  _ <    | |
//   |_|   /_/   \_\ |_| \_\   |_|
//

/*
 * Module      : verilog/tart.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
 * 
 * TART hardware description for data-acquisition, and real-time computation
 * of visibilities.
 * 
 * Changelog:
 *  + ??/??/2013  --  initial file;
 *  + 11/05/2016  --  rebuilt the SPI module to be faster, have a Wishbone-
 *                    like interconnect, and to separate out some TART-
 *                    specific functionality;
 *  + 09/06/2016  --  started adding the hardware correlators;
 *  + 25/06/2016  --  finished refactoring the top-level modules, for the new
 *                    SPI and correlators;
 *  + 05/09/2016  --  floorplanning complete, and now meets timing;
 *  + 15/10/2016  --  numerous improvements to get it to a releasable state;
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

`include "tartcfg.v"

module tart
  #(// Memory controller parameters:
    parameter SDRAM_STARTUP_CYCLES = 10100, // -- 100us, plus a little more, @100MHz
`ifdef __512Mb_SDRAM
    parameter SDRAM_COLUMN_BITS    = 10, // 8 for standard papilio pro
    parameter SDRAM_ADDRESS_WIDTH  = 25, // 22 for standard papilio pro
    parameter CYCLES_PER_REFRESH   = 780, // (64000*100)/8192-1 Cycled as (64ms @100MHz)/8192 rows
`else
    parameter SDRAM_COLUMN_BITS    = 8,     // 8 for standard papilio pro
    parameter SDRAM_ADDRESS_WIDTH  = 22,    // 22 for standard papilio pro
    parameter CYCLES_PER_REFRESH   = 1524,  // (64000*100)/4096-1 Cycled as (64ms @100MHz)/4096 rows
`endif // !`ifdef __512Mb_SDRAM
    parameter SSB                  = SDRAM_ADDRESS_WIDTH-2,

    //  Antenna/signal parameters:
    parameter ANTENNAE = `NUM_ANTENNA,
    parameter NSB      = ANTENNAE-1,
    parameter ALIGN    = `USE_ALIGN,

    //  Fake antenna-data options:
    parameter DEBUG    = `USE_DEBUG,
    parameter MULTI    = `MULTI_SOURCE,
    parameter RNG      = `RANDOM_DATA,
    parameter CONST    = `CONST_DATA,
    parameter CDATA    = `CONST_WORD,

    //  Wishbone mode settings/parameters:
    parameter ASYNC    = 0,     // asynchronous WB transactions (0/1)?
    parameter PIPED    = 1,     // pipelined BURST transactions (0/1)?
    parameter RESET    = 1,     // fast-reset enable (0/1)?
    parameter CHECK    = 1,     // bus-signal sanity-checking (0/1)?
    parameter VIZWR    = 0,     // enable writes to viz-buffer (0/1)?

    //  Simulation-only parameters:
    parameter DELAY    = `DELAY)     // Simulation gate-delay setting
   (
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
    input [NSB:0]      antenna, // Radio Data Interface

    // MISCELLANEOUS
	  output wire        rx_clk_test_pin, // show delayed clock
    output wire        led              // Papilio LED
    );



   //-------------------------------------------------------------------------
   //
   //     ADDITIONAL TART SETTINGS
   //
   //-------------------------------------------------------------------------
   //  Visibilities/correlator settings.
   parameter ACCUM = `ACCUM_BITS; // Bit-width of the accumulators
   parameter BLOCK = ACCUM;       // Maximum #bits of the block-size
   parameter MSB   = BLOCK-1;     // Data transfer MSB
   parameter TRATE = `TMUX_RATE;  // Time-multiplexing rate
   parameter TBITS = `TMUX_BITS;   // TMUX bits
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

   //  Settings for the visibilities data banks:
   parameter XBITS = `BANK_BITS;  // Bit-width of the block-counter
   parameter XSB   = XBITS-1;     // MSB of the block-counter

   //  Internal correlator data-bus settings:
   parameter CBITS = XBITS+RBITS; // Correlator-bus address bit-width
   parameter CSB   = CBITS-1;



   //-------------------------------------------------------------------------
   //
   //  TART SIGNALS
   //
   //-------------------------------------------------------------------------
   (* PERIOD = "10.18 ns" *) wire fpga_clk;
   (* PERIOD = "10.18 ns" *) wire rx_clk;
   (* PERIOD = "5.091 ns" *) wire clk_x;
   (* KEEP   = "TRUE"     *) wire reset;

   wire                reset_n;

   //  SDRAM memory-controller signals:
   wire                cmd_enable, cmd_ready, cmd_wr;
   wire [SSB:0]        cmd_address;
   wire [31:0]         cmd_data_in;
   wire [31:0]         data_out;

   wire [2:0]          tart_state;

   //  Visibilities-unit signals:
   wire                vx_enabled, vx_available, vx_overflow, vx_overwrite;
   wire                vx_newblock, vx_streamed, vx_switching, vx_accessed;
   wire [MSB:0]        vx_blocksize, vx_checksum;
   wire [XSB:0]        c_bank_x;
   wire                vx_stuck, vx_limp;      // additional debug signals

   //  Acquisition (of antenna raw-data) signals:   
   wire aq_enabled;
   wire [2:0] aq_delay;
   wire aq_shift, aq_count, aq_debug, aq_valid;
   wire [NSB:0] ax_dat;

   //-------------------------------------------------------------------------
   //  Wishbone bus signals to/from the off-chip, SPI I/O interface.
   wire         b_clk, b_rst;               // WB system signals
   wire         b_cyc, b_stb, b_we;         // WB master signals
   wire         b_ack, b_wat, b_rty, b_err; // WB slave signals
   wire [WSB:0] b_adr;                      // WB address signal
   wire [BSB:0] b_dtx, b_drx;               // WB data signals

   //  Wishbone signals for the reset/control module.
   reg          r_sel = 1'b0;
   wire         r_stb, r_mux;               // Select the reset unit?
   wire         r_ack, r_wat, r_rty, r_err; // Respone signals
   wire [BSB:0] r_drx, r_dtx;

   //  Wishbone signals for the data-acquisition device.
   reg          a_sel = 1'b0;
   wire         a_stb, a_mux;
   wire         a_ack, a_wat, a_rty, a_err;
   wire [BSB:0] a_drx, a_dtx;
   wire [3:0]   a_adr = b_adr[3:0];

   //  WB signals for the streaming interface to the visibilities data.
   wire [BSB:0] s_drx, s_dtx;
   wire [XSB:0] s_adr;
   wire         s_cyc, s_stb, s_we;
   wire         s_ack, s_wat, s_rty, s_err;

   //-------------------------------------------------------------------------
   //  SPI and system-status signals.
   wire         spi_busy, request_from_spi;
   wire         spi_uflow, spi_oflow;

   wire [7:0]   sys_status = {spi_uflow, spi_oflow, request_from_spi,
                              aq_enabled, aq_debug, tart_state[2:0]};
   wire [7:0]   viz_status = {aq_debug, vx_enabled, vx_available,
                              vx_overflow, s_adr};


   //-------------------------------------------------------------------------
   //  SPI <-> WB bus signals.
   assign b_clk = fpga_clk;
   assign b_rst = reset;

   assign b_ack = a_ack || r_ack;
   assign b_wat = a_wat || r_wat;
   assign b_rty = a_rty || r_rty;
   assign b_err = a_err || r_err;
`ifdef __icarus
   assign b_dtx = r_mux ? r_drx : (a_mux ? a_drx : 'bz);
`else
   assign b_dtx = r_mux ? r_drx : a_drx;
`endif

   //  Reset unit decoder and signals.
   assign r_stb = b_adr[6:3] == 4'hf && b_stb;
   assign r_mux = ASYNC ? r_stb || r_sel : r_sel;
   assign r_dtx = b_drx;

   //  Acquisition unit decoder and signals.
   assign a_stb = b_adr[6:4] == 3'h0 && b_stb;
   assign a_mux = ASYNC ? a_stb || a_sel : a_sel;
   assign a_adr = b_adr[3:0];
   assign a_dtx = b_drx;


   //-------------------------------------------------------------------------
   //  Miscellaneous assignments.
`ifndef __RELEASE_BUILD
	 assign rx_clk_test_pin = rx_clk_16_buf; // ????, Pat @02/11/2016
`else
	 assign rx_clk_test_pin = 1'b0;
`endif
   
   //-------------------------------------------------------------------------
   //  Additional (optional) debugging outputs.
`ifdef __RELEASE_BUILD
   assign led = 1'b0;
`else
   reg          blink = 1'b0;

//    assign led = tart_state >= 2; // asserted when data can be read back
   assign led = blink;

   always @(posedge b_clk)
     if (vx_newblock) blink <= #DELAY ~blink;
`endif // !`ifdef __RELEASE_BUILD


   //-------------------------------------------------------------------------
   //
   //  SYSTEM ADDRESS DECODERS
   //
   //-------------------------------------------------------------------------
   //  Keep the selected device active until the transaction has been
   //  acknowledged.
   always @(posedge b_clk)
     if (b_rst && RESET || !b_cyc && CHECK)
       {a_sel, r_sel} <= #DELAY 2'b00;
     else if (b_cyc || !CHECK)
       {a_sel, r_sel} <= #DELAY {a_stb, r_stb};
     else
       {a_sel, r_sel} <= #DELAY {a_sel, r_sel};


   //-------------------------------------------------------------------------
   //
   //  GENERATE TART SYSTEM CLOCKS
   //
   //-------------------------------------------------------------------------
`ifdef __USE_OLD_CLOCKS
   tart_clk_generator clknetwork
     (
      .CLKIN(rx_clk_16),        // 16.368 MHz
      .CLKOUT0(rx_clk_16_buf),  // 16.368 MHz buffered
      .CLKOUT1(fpga_clk),       // 16.368x6 = 98.208 MHz
      .reset_n(reset_n)
      );

`else
   tart_dcm TART_DCM0
     ( .clk_pin(rx_clk_16),     // 16.368 MHZ
       .clk_rst(1'b0),
       .clk(rx_clk_16_buf),     // 16.368 MHz buffered
       .reset_n(reset_n),
       .status_n(status_n),
       .clk6x(fpga_clk),        // 16.368x6  =  98.208 MHz
       .clk12x(clk_x)           // 16.368x12 = 196.416 MHz
       );
`endif // !__USE_OLD_CLOCKS



   //-------------------------------------------------------------------------
   //  
   //  TART ANTENNA DATA CAPTURE BLOCK
   //  
   //-------------------------------------------------------------------------
   wire         aq_align = 1'b0;
   wire         aq_clear = 1'b0;
   wire         aq_retry = 1'b0;

   tart_capture
     #(.AXNUM(ANTENNAE),
       .ABITS(SDRAM_ADDRESS_WIDTH),
       // fake-data options:
       .MULTI(MULTI),
       .RNG  (RNG),
       .CONST(CONST),
       .CDATA(CDATA),
       // use additional data-capture and alignment circuitry?
       .ALIGN(ALIGN),
       .RATIO(TRATE),
       .RBITS(TBITS),
       // simulation-only settings:
       .DELAY(DELAY)
       ) CAP0
     ( .clk_i     (fpga_clk),
       .clk_x     (clk_x),
       .clk_e     (rx_clk_16_buf),
       .rst_i     (reset),

       //  External antenna data:
       .signal_e_i(antenna),

       //  Wishbone (SPEC B4) interconnect:
       .cyc_i(b_cyc),
       .stb_i(c_stb),
       .we_i (b_we),
       .ack_o(c_ack),
       .wat_o(c_wat),
       .rty_o(c_rty),
       .err_o(c_err),
       .adr_i(c_adr),
       .dat_i(c_dtx),
       .dat_o(c_drx),

       //  Memory controller signals (bus-domain):
       .mcb_ce_o  (cmd_enable),
       .mcb_wr_o  (cmd_wr),
       .mcb_rdy_i (cmd_ready),
       .mcb_adr_o (cmd_address),
       .mcb_dat_o (cmd_data_in),

       //  Bus-domain acquisition control & status signals:
       .vx_ce_i   (vx_enabled),
       .aq_ce_i   (aq_enabled),
       .aq_delay_i(aq_delay),   // capture delay (in bus-clocks)
       .aq_align_i(aq_align),   // enable signal alignment
       .aq_valid_o(aq_valid),   // valid, aligned signal?
       .aq_error_o(aq_error),   // signal tracking lost?
       .aq_clear_i(aq_clear),   // clear error-flag and continue?
       .aq_retry_i(aq_retry),   // reacquire a lost signal?
       
       .aq_debug_i(aq_debug),   // fake-data setting inputs
       .aq_shift_i(aq_shift),
       .aq_count_i(aq_count),

       //  Correlator-domain acquisition data & status signals:
       .ax_vld_x_o(ax_vld),     // acquired (and oversampled) data
       .ax_new_x_o(ax_new),     // outputs
       .ax_dat_x_o(ax_dat),

       //  Data & miscellaneous signals:
       .request_i (request_from_spi),
       .tart_state(tart_state)
       );


   //  No acquisition means that the memory-controller isn't needed.
`ifdef __USE_ACQUISITION
   //-------------------------------------------------------------------------
   //  
   //  SDRAM CONTROLLER FOR THE RAW ANTENNA DATA
   //  
   //-------------------------------------------------------------------------
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
`else // !`ifdef __USE_ACQUISITION

   //  Drive zeros onto the unused pins:
   assign cmd_ready = 1'b0;
   assign data_out_ready = 1'b0;
   assign data_out = 32'b0;

   assign SDRAM_CS   = 1'b1;
   assign SDRAM_CKE  = 1'b0;
   (* PULLUP = "TRUE" *) assign SDRAM_CLK  = 1'bz;
   (* PULLUP = "TRUE" *) assign SDRAM_WE   = 1'bz;
   (* PULLUP = "TRUE" *) assign SDRAM_RAS  = 1'bz;
   (* PULLUP = "TRUE" *) assign SDRAM_CAS  = 1'bz;
   (* PULLUP = "TRUE" *) assign SDRAM_BA   = 2'bz;
   (* PULLUP = "TRUE" *) assign SDRAM_ADDR = 13'bz;
   (* PULLUP = "TRUE" *) assign SDRAM_DQM  = 2'bz;
   (* PULLUP = "TRUE" *) assign SDRAM_DQ   = 16'bz;

`endif // !`ifdef __USE_ACQUISITION


   //-------------------------------------------------------------------------
   //
   //     TART SYSTEM-WIDE, WISHBONE (SPEC B4) INTERCONNECT AND PERIPHERALS.
   //
   //-------------------------------------------------------------------------

   //-------------------------------------------------------------------------
   //     WISHBONE (SPEC B4) SPI SLAVE CORE.
   //-------------------------------------------------------------------------
   spi_slave_wb
     #( .WIDTH(BBITS),
        .ASYNC(1),
        .PIPED(1),
        .CHECK(1)
        ) SPI0
       (
        .clk_i(b_clk),
        .rst_i(b_rst),
        .cyc_o(b_cyc),
        .stb_o(b_stb),
        .we_o (b_we),
        .ack_i(b_ack),
        .wat_i(b_wat),
        .rty_i(b_rty),
        .err_i(b_err),
        .adr_o(b_adr),
        .dat_i(b_dtx),
        .dat_o(b_drx),

        .active_o(spi_busy),
        //        .status_i(sys_status),
        .status_i(viz_status),
        .overflow_o(spi_oflow),
        .underrun_o(spi_uflow),
        
        .SCK_pin(SPI_SCK),
        .MOSI(SPI_MOSI),
        .MISO(SPI_MISO),
        .SSEL(SPI_SSEL)
        );


   //-------------------------------------------------------------------------
   //     RESET HANDLER
   //-------------------------------------------------------------------------
   tart_control
     #(  .WIDTH(BBITS),
         .RTIME(4)
         ) TART_CONTROL0
       ( .clk_i(b_clk),
         .rst_i(b_rst),
         .cyc_i(b_cyc),
         .stb_i(r_stb),
         .we_i (b_we),
         .ack_o(r_ack),
         .wat_o(r_wat),
         .rty_o(r_rty),
         .err_o(r_err),
         .adr_i(b_adr[1:0]),
         .dat_i(r_dtx),
         .dat_o(r_drx),

         .status_i  (sys_status),
         .reset_ni  (reset_n),
         .reset_o   (reset),
         .checksum_i(vx_checksum)
         );


   //-------------------------------------------------------------------------
   //     DATA-ACQUISITION CONTROL AND READ-BACK.
   //-------------------------------------------------------------------------
   tart_acquire
     #(  .ACCUM(ACCUM),
         .AXNUM(ANTENNAE),
         .BBITS(BBITS),
         .XBITS(XBITS),
         .PIPED(1),             // Wishbone mode settings
         .CHECK(1),
         .VIZWR(VIZWR),
         .DELAY(DELAY)
         ) TART_ACQUIRE0
       ( .clk_i(b_clk),
         .rst_i(b_rst),

         //  Wishbone (SPEC B4) bus for raw-data and visibilities:
         .cyc_i(b_cyc),
         .stb_i(a_stb),
         .we_i (b_we),
         .ack_o(a_ack),
         .wat_o(a_wat),
         .rty_o(a_rty),
         .err_o(a_err),
         .adr_i(a_adr),
         .dat_i(a_dtx),
         .dat_o(a_drx),

         //  Antenna data-capture & acquisition controls:
         .aq_enabled_o(aq_enabled),
         .aq_valid_i  (aq_valid),
         .aq_debug_o  (aq_debug),
         .aq_shift_o  (aq_shift),
         .aq_count_o  (aq_count),
         .aq_delay_o  (aq_delay),
         //        .aq_adr_i(cmd_address),
         .aq_adr_i({21'h0, s_adr}),

         //  DRAM (streaming) read-back signals:
         .data_ready  (data_out_ready),
         .data_request(request_from_spi),
         .data_in     (data_out[MSB:0]),

         //  SPI bus status input:
         .spi_busy_i(spi_busy),

         //  Wishbone (SPEC B4) bus for accessing visibilities data:
         .vx_cyc_o(s_cyc),
         .vx_stb_o(s_stb),
         .vx_we_o (s_we ),
         .vx_ack_i(s_ack),
         .vx_wat_i(s_wat),
         .vx_rty_i(s_rty),
         .vx_err_i(s_err),
         .vx_adr_o(s_adr),
         .vx_dat_i(s_drx),
         .vx_dat_o(s_dtx),

         //  Visibilities status & control signals:
         .vx_enabled_o  (vx_enabled),
         .vx_overwrite_o(vx_overwrite), // overwrite when buffers full?
         .overflow_i    (vx_overflow),  // visibilities overflow?
         .newblock_i    (vx_newblock),  // strobes when new block ready
         .streamed_i    (vx_streamed),  // has a bank finished streaming?
         .accessed_o    (vx_accessed),  // have visibilities been accessed?
         .available_o   (vx_available), // new bank available?
         .checksum_i    (vx_checksum),  // bank checksum
         .blocksize_o   (vx_blocksize), // size of block of correlations
         .vx_stuck_i    (vx_stuck),     // bus signals stuck?
         .vx_limp_i     (vx_limp)       // or, refusing to cooperate?
         );


`ifndef __USE_CORRELATORS
   assign vx_streamed  = 1'b0;
   assign vx_newblock  = 1'b0;
   assign vx_available = 1'b0;
   assign vx_checksum  = 24'h0ff1ce;

`else
   //-------------------------------------------------------------------------
   //     
   //     CORRELATOR / VISIBILITIES BLOCK.
   //     
   //-------------------------------------------------------------------------
   tart_dsp
     #(.AXNUM(ANTENNAE),        // number of attached antennae
       .ACCUM(ACCUM),           // accumulator bit-width
       .TRATE(TRATE),           // time multiplexing (TMUX) rate
       .TBITS(TBITS),           // TMUX counter bit-width
       .NREAD(NREAD),           // visibilities read-count
       .RBITS(RBITS),
       .XBITS(XBITS),           // visibilities-bank address bit-width
       .CBITS(CBITS),           // correlator address bit-width
       .PIPED(PIPED),           // Wishbone pipelined mode?
       .CHECK(CHECK),           // bus sanity-checking?
       .VIZWR(VIZWR),           // bidirectional streaming access?
       .DELAY(DELAY)            // simulation-only settings
       ) DSP
     ( .clk_x(clk_x),           // correlator clock
       .clk_i(b_clk),           // Wishbone/system clock
       .rst_i(b_rst),           // bus/system reset

       //  Wishbone (SPEC B4) bus between DSP and acquisition unit:
       .cyc_i(s_cyc),
       .stb_i(s_stb),
       .we_i (s_we),
       .ack_o(s_ack),
       .wat_o(s_wat),
       .rty_o(s_rty),
       .err_o(s_err),
       .adr_i(s_adr),
       .dat_i(s_dtx),
       .dat_o(s_drx),

       //  Correlator control & status signals:
       .antenna_i  (ax_dat),
       .vx_enable_i(vx_enabled),
       .vx_stream_i(spi_busy),
       .vx_bank_x_o(c_bank_x),
       .overwrite_i(vx_overwrite),
       .blocksize_i(vx_blocksize),
       .switching_o(vx_switching),
       .overflow_o (vx_overflow),
       .newblock_o (vx_newblock),
       .checksum_o (vx_checksum),
       .streamed_o (vx_streamed),

       //  Miscellaneous debugging/status signals:
       .stuck_o    (vx_stuck),
       .limp_o     (vx_limp)
       );
 `endif //  `ifdef __USE_CORRELATORS


endmodule // tart
