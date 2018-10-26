`timescale 1ns/100ps


//--------------------------------------------------------------------------//
//--------------------------------------------------------------------------//
//                                                                          //
 //                    _____      _      ____    _____                     //
 //                   |_   _|    / \    |  _ \  |_   _|                    //
 //                     | |     / _ \   | |_) |   | |                      //
 //                     | |    / ___ \  |  _ <    | |                      //
 //                     |_|   /_/   \_\ |_| \_\   |_|                      //
 //                                                                        //
//                                                                          //
//--------------------------------------------------------------------------//
//--------------------------------------------------------------------------//


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
 * 
 * This file is part of TART.
 * 
 * TART is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * TART is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser Public License along with
 * TART.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * 
 * Description:
 * TART hardware description for data-acquisition, and real-time computation
 * of visibilities.
 * 
 * TART has the data-flow (from the point-of-view of the signal data):
 * 
 *   \|/                         +---------+          +---------+
 *    |                          |         |          |         |
 *    |                    +---> |  DSP    |----o---> |  SPI    | <===> I/O
 *    |     +---------+    |     |         |    |     |         |
 *    |     |         |    |     +---------+    |     +---------+
 *    +---> | CAPTURE |----o                    |
 *          |         |    |     +---------+    |
 *          +---------+    |     |         |----+
 *                         +---> | ACQUIRE |     
 *                               |         | <---o
 *                               +---------+     |
 *          +---------+                          |    +---------+
 *          |         |                          |    |         |
 *          |  SDRAM  | <------------------------+    | CONTROL |
 *          |         |                               |         |
 *          +---------+                               +---------+  ;
 * 
 * and where only the SDRAM controller doesn't have registers that can be
 * set using the off-chip, SPI interface (in the above diagram). The Wishbone
 * data-flow (both register settings, and signal data) is controlled by the
 * 'tart_wishbone' core.
 * 
 * The above top-level modules are available at:
 *  + verilog/spi/spi_slave_wb.v
 *  + verilog/acquire/tart_capture.v
 *  + verilog/acquire/tart_acquire.v
 *  + verilog/correlator/tart_dsp.v
 *  + verilog/tart_control.v
 *  + verilog/SDRAM_Controller_v.v
 * 
 * 
 * The default address ranges for the SPI slaves are:
 *  + 7'b000_00xx -- signal-capture unit;
 *  + 7'b010_00xx -- raw-data acquisition unit;
 *  + 7'b100_00xx -- DSP/visibilities unit; and
 *  + 7'b110_00xx -- (system) control unit.
 * 
 * 
 * Raw-acquisition start-up sequence:
 *  1/ enable the capture unit ('capture/tart_capture.v');
 *  2/ enable the acquisition unit ('acquire/tart_acquire.v');
 *  3/ wait until 'state[2:0] > 2'; and
 *  4/ stream back the raw data (via SPI),
 * and as an example, this mode can be launched via a Python script:
 * 
 *  > sudo python tart_testbench.py --bramexp=21 --internal --counter
 * 
 * 
 * Real-time visibilities calculation mode:
 *  1/ enable the capture unit ('capture/tart_capture.v');
 *  2/ set the 'blocksize' (correlator-sums / bank) register;
 *  2/ enable the DSP unit ('dsp/tart_dsp.v');
 *  3/ poll for a new bank of visibilities to become 'available';
 *  4/ stream back the visibilities data (via SPI); and
 *  5/ go to step 3/,
 * and as an example, this mode can be launched via a Python script:
 * 
 *  > sudo python low_level_dsp.py --blocksize=22 --monitor --capture
 * 
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
 *  + 15/10/2016  --  fixed many clock-domain crossing issues, and upgraded to
 *                    Wishbone SPEC B4 Pipelined transactions;
 *  + 17/11/2016  --  new data-capture circuits;
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
    parameter RESET    = 0,     // fast-reset enable (0/1)?
    parameter CHECK    = 1,     // bus-signal sanity-checking (0/1)?
//     parameter CHECK    = 0,     // bus-signal sanity-checking (0/1)?
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
   (* PERIOD = "5.091 ns" *) wire clk_x;
   (* KEEP   = "TRUE"     *) wire reset;

   wire                clock_n, reset_n;
   wire                clock_b, reset_b; // WB system signals

   (* KEEP   = "TRUE"    *)
   wire                rx_clk_16_buf;


   //-------------------------------------------------------------------------
   //  Wishbone bus signals to/from the off-chip, SPI I/O interface.
   wire         spi_cyc, spi_stb, spi_we;
   wire         spi_ack, spi_wat, spi_rty, spi_err;
   wire [WSB:0] spi_adr;
   wire [BSB:0] spi_dtx, spi_drx;

   //-------------------------------------------------------------------------
   //  Wishbone signals for the raw-data capture & recovery module.
   wire         cap_cyc, cap_stb, cap_we;
   wire         cap_ack, cap_wat, cap_rty, cap_err;
   wire [1:0]   cap_adr;
   wire [BSB:0] cap_drx, cap_dtx;

   //  Wishbone signals for the data-acquisition device.
   wire         acq_cyc, acq_stb, acq_we;
   wire         acq_ack, acq_wat, acq_rty, acq_err;
   wire [1:0]   acq_adr;
   wire [BSB:0] acq_drx, acq_dtx;

   //  Wishbone signals for the DSP (visibilities) core.
   wire         dsp_cyc, dsp_stb, dsp_we;
   wire         dsp_ack, dsp_wat, dsp_rty, dsp_err;
   wire [1:0]   dsp_adr;
   wire [BSB:0] dsp_drx, dsp_dtx;

   //  Wishbone signals for the reset/control unit.
   wire         sys_cyc, sys_stb, sys_we;
   wire         sys_ack, sys_wat, sys_rty, sys_err;
   wire [1:0]   sys_adr;
   wire [BSB:0] sys_drx, sys_dtx;


   //-------------------------------------------------------------------------
   //  SDRAM memory-controller signals:
   //-------------------------------------------------------------------------
   wire                cmd_enable, cmd_ready, cmd_write, data_out_ready;
   wire [SSB:0]        cmd_address;
   wire [31:0]         cmd_data_in;
   wire [31:0]         data_out;

   //-------------------------------------------------------------------------
   //  Visibilities-unit signals:
   wire                vx_enabled, vx_pending, vx_overflow, vx_overwrite;
   wire                vx_newblock, vx_streamed, vx_switching, vx_accessed;
   wire [MSB:0]        vx_checksum;
   wire                vx_stuck, vx_limp;      // additional debug signals
   wire [XSB:0]        vx_bank;

   //-------------------------------------------------------------------------
   //  Capture-unit info signals:
   wire                cx_enabled, cx_debug;
   wire                cx_strobe, cx_middle, cx_locked;
   wire [MSB:0]        cx_signal;

   //-------------------------------------------------------------------------
   //  Acquisition (of antenna raw-data) signals:   
   wire                aq_enabled;
   wire [2:0]          aq_state;

   //-------------------------------------------------------------------------
   //  SPI and system-status signals.
   wire                spi_busy, request_from_spi;
   wire                spi_uflow, spi_oflow;
   wire [7:0]          sys_status, spi_status;


   //-------------------------------------------------------------------------
   //  Miscellaneous assignments.
   //-------------------------------------------------------------------------
   assign clock_b = fpga_clk;
   assign reset_b = reset;

   //-------------------------------------------------------------------------
   //  Compose the status-signal from the most important status-signals of
   //  TART's subsystems.
   assign sys_status = {vx_enabled, vx_pending, // visibilities status
                        cx_enabled,   cx_debug, // capture status
                        aq_enabled,  aq_state}; // acquisition status
   assign spi_status = {spi_oflow, spi_uflow, 5'h0, spi_busy};
   
   //-------------------------------------------------------------------------
   //  Additional (optional) debugging outputs.
`ifdef __RELEASE_BUILD
   assign led = 1'b0;
`else
   reg          blink = 1'b0;

//    assign led = aq_state >= 2; // asserted when data can be read back
   assign led = blink;

   always @(posedge clock_b)
     if (vx_newblock) blink <= #DELAY ~blink;
`endif // !`ifdef __RELEASE_BUILD



   //-------------------------------------------------------------------------
   //
   //  GENERATE TART SYSTEM CLOCKS
   //
   //-------------------------------------------------------------------------
`ifdef __USE_OLD_CLOCKS
   tart_dcm TART_DCM0
     ( .clk_pin_i(rx_clk_16),     // 16.368 MHZ
       .clk_rst_i(1'b0),
       .clk_ext_o(rx_clk_16_buf), // 16.368 MHz buffered
       .clk6x_o  (fpga_clk),      // 16.368x6  =  98.208 MHz
       .clk6n_o  (clock_n),       // 16.368x6  =  98.208 MHz
       .clk12x_o (clk_x),         // 16.368x12 = 196.416 MHz
       .reset_no (reset_n),
       .status_no()
       );

`else
   //  NOTE: This is the DEFAULT.
   tart_dual_dcm TART_DCM0
     ( .clk_pin_i(rx_clk_16),     // 16.368 MHZ
       .clk_rst_i(1'b0),
       .clk_ext_o(rx_clk_16_buf), // 16.368 MHz buffered
       .clk6x_o  (fpga_clk),      // 16.368x6  =  98.208 MHz
       .clk6n_o  (clock_n),       // 16.368x6  =  98.208 MHz
       .clk12x_o (clk_x),         // 16.368x12 = 196.416 MHz
       .reset_no (reset_n),
       .status_no()
       );
`endif // !__USE_OLD_CLOCKS



   //-------------------------------------------------------------------------
   //  
   //  ANTENNA RAW-DATA CAPTURE BLOCK
   //  
   //-------------------------------------------------------------------------
   //  No acquisition means that the memory-controller isn't needed.

`ifdef __USE_ACQUISITION
   //-------------------------------------------------------------------------
   //  
   //  SDRAM CONTROLLER FOR THE RAW ANTENNA DATA
   //  
   //-------------------------------------------------------------------------
   (* NOMERGE *) reg reset_sdram = 1'b1;

   always @(posedge clock_b)
     reset_sdram <= #DELAY reset_b;

   SDRAM_Controller_v
     #( .sdram_address_width (SDRAM_ADDRESS_WIDTH),
        .sdram_column_bits   (SDRAM_COLUMN_BITS),
        .sdram_startup_cycles(SDRAM_STARTUP_CYCLES),
        .cycles_per_refresh  (CYCLES_PER_REFRESH)
        ) HAMSTER_SDRAM
       (
        .clk            (clock_b),
        .reset          (reset_sdram),

        .cmd_ready      (cmd_ready),     // (O) MCB has initialised?
        .cmd_enable     (cmd_enable),    // (I) start a MCB operation
        .cmd_wr         (cmd_write),     // (I) signal a write transaction
        .cmd_address    (cmd_address),   // (I) data address
        .cmd_byte_enable(4'b1111),       // (I) individual byte write-selects
        .cmd_data_in    (cmd_data_in),   // (I) write data
        .data_out       (data_out),      // (O) requested data
        .data_out_ready (data_out_ready),// (O) requested data ready?

        .SDRAM_CLK      (SDRAM_CLK),
        .SDRAM_CKE      (SDRAM_CKE),
        .SDRAM_CS       (SDRAM_CS),
        .SDRAM_RAS      (SDRAM_RAS),
        .SDRAM_CAS      (SDRAM_CAS),
        .SDRAM_WE       (SDRAM_WE),
        .SDRAM_DQM      (SDRAM_DQM),
        .SDRAM_ADDR     (SDRAM_ADDR),
        .SDRAM_BA       (SDRAM_BA),
        .SDRAM_DATA     (SDRAM_DQ)
        );


`else // !`ifdef __USE_ACQUISITION
   //-------------------------------------------------------------------------
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
   //  TART SYSTEM-WIDE, WISHBONE (SPEC B4) INTERCONNECT AND PERIPHERALS.
   //
   //-------------------------------------------------------------------------
   //  Local synchronous reset for the Wishbone controller.
   (* NOMERGE *) reg reset_tartwb = 1'b1;

   always @(posedge clock_b)
     reset_tartwb <= #DELAY reset_b;

   //-------------------------------------------------------------------------
   //  The TART Wishbone bus connects the lone Wishbone master (the SPI slave
   //  core) to the:
   //   + raw-data capture unit;
   //   + raw-data acquisition and buffer unit;
   //   + DSP core (that computes the visibilities); and
   //   + TART system-controller.
   //  
   //  TODO:
   //   + parameterise moar stuffs;
   //  
   tart_wishbone
     #(  .XBITS(2),
         .ASYNC(1),
         .RESET(RESET),
         .PIPED(PIPED),
         .CHECK(CHECK),
         // supported peripherals:
         .CAPEN(1),
         .ACQEN(1),
         .DSPEN(1),
         .SYSEN(1),
         // simulation-only options:
         .DELAY(DELAY)
         ) ARB
       ( .bus_clk_i(clock_b),
         .bus_rst_i(reset_tartwb),

         //-------------------------------------------------------------------
         //  SPI Wishbone master.
         .master_cyc_i(spi_cyc),
         .master_stb_i(spi_stb),
         .master_we_i (spi_we),
         .master_ack_o(spi_ack),
         .master_wat_o(spi_wat),
         .master_rty_o(spi_rty),
         .master_err_o(spi_err),
         .master_adr_i(spi_adr),
         .master_dat_i(spi_drx),
         .master_dat_o(spi_dtx),

         //-------------------------------------------------------------------
         //  Capture-unit Wishbone slave.
         .cap_cyc_o(cap_cyc),
         .cap_stb_o(cap_stb),
         .cap_we_o (cap_we),
         .cap_ack_i(cap_ack),
         .cap_wat_i(cap_wat),
         .cap_rty_i(cap_rty),
         .cap_err_i(cap_err),
         .cap_adr_o(cap_adr),
         .cap_dat_i(cap_drx),
         .cap_dat_o(cap_dtx),

         //  Acquisition-unit Wishbone slave.
         .acq_cyc_o(acq_cyc),
         .acq_stb_o(acq_stb),
         .acq_we_o (acq_we),
         .acq_ack_i(acq_ack),
         .acq_wat_i(acq_wat),
         .acq_rty_i(acq_rty),
         .acq_err_i(acq_err),
         .acq_adr_o(acq_adr),
         .acq_dat_i(acq_drx),
         .acq_dat_o(acq_dtx),

         //  DSP-unit Wishbone slave.
         .dsp_cyc_o(dsp_cyc),
         .dsp_stb_o(dsp_stb),
         .dsp_we_o (dsp_we),
         .dsp_ack_i(dsp_ack),
         .dsp_wat_i(dsp_wat),
         .dsp_rty_i(dsp_rty),
         .dsp_err_i(dsp_err),
         .dsp_adr_o(dsp_adr),
         .dsp_dat_i(dsp_drx),
         .dsp_dat_o(dsp_dtx),

         //  System-control-unit Wishbone slave.
         .sys_cyc_o(sys_cyc),
         .sys_stb_o(sys_stb),
         .sys_we_o (sys_we),
         .sys_ack_i(sys_ack),
         .sys_wat_i(sys_wat),
         .sys_rty_i(sys_rty),
         .sys_err_i(sys_err),
         .sys_adr_o(sys_adr),
         .sys_dat_i(sys_drx),
         .sys_dat_o(sys_dtx)
         //-------------------------------------------------------------------
         );



   //-------------------------------------------------------------------------
   //
   //  SPI SLAVE CORE.
   //
   //-------------------------------------------------------------------------
   //  Local synchronous reset for the SPI interface.
   (* NOMERGE *) reg reset_spi = 1'b1;

   always @(posedge clock_b)
     reset_spi <= #DELAY reset_b;

   //-------------------------------------------------------------------------
   //  NOTE:
   //   + this module is the lone, Wishbone (SPEC B4), master device;
   //   + electrically capable of operating at above 64 MHz, but currently
   //     limited to 32 MHz, due to the latency of some of the attached cores;
   spi_slave_wb
     #( .WIDTH(BBITS),
        .ASYNC(1),
        .PIPED(PIPED),
        .CHECK(CHECK)
        ) SPI0
       (
        .clk_i     (clock_b),
        .rst_i     (reset_spi),

        //  Wishbone master interface.
        .cyc_o     (spi_cyc),
        .stb_o     (spi_stb),
        .we_o      (spi_we),
        .ack_i     (spi_ack),
        .wat_i     (spi_wat),
        .rty_i     (spi_rty),
        .err_i     (spi_err),
        .adr_o     (spi_adr),
        .dat_i     (spi_dtx),
        .dat_o     (spi_drx),

        .active_o  (spi_busy),
        .status_i  (sys_status),
        // .status_i  (viz_status),
        .overflow_o(spi_oflow),
        .underrun_o(spi_uflow),
        
        .SCK_pin   (SPI_SCK),
        .MOSI      (SPI_MOSI),
        .MISO      (SPI_MISO),
        .SSEL      (SPI_SSEL)
        );



   //-------------------------------------------------------------------------
   //
   //  RAW-DATA CAPTURE & CLOCK-RECOVERY.
   //
   //-------------------------------------------------------------------------
   //  Local synchronous reset for the data-capture module.
   (* NOMERGE *) reg reset_capture = 1'b1;

   always @(posedge clock_b)
     reset_capture <= #DELAY reset_b;

   //-------------------------------------------------------------------------
   //  Raw-data generally requires clock-recovery, as each antenna's clock
   //  has an unknown phase-delay associated with it. Clock-recovery for each
   //  antenna/channel removes these delays from each channel.
   //  
   //  NOTE:
   //   + capture registers are mapped to '0b00nnnnn'.
   //   + for testing purposes, registers can be set so that fake data is
   //     generated;
   //  
   tart_capture
     #( .AXNUM(ANTENNAE),
        // use additional data-capture and alignment circuitry?
        .ALIGN(ALIGN),
        .RATIO(TRATE),
        .RBITS(TBITS),
        .TICKS(4),              // to match pipeline-register delays
        // Wishbone mode settings
        .RESET(RESET),
        .PIPED(PIPED),
        .CHECK(CHECK),
        // fake-data options:
        .MULTI(MULTI),
        .RNG  (RNG),
        .CONST(CONST),
        .CDATA(CDATA),
        // simulation-only settings:
        .DELAY(DELAY)
        ) CAPTURE
       (//--------------------------------------------------------------------
        //  Global clocks & resets:
        .clock_e   (rx_clk_16_buf),
        .clock_n   (clock_n),   // negated system-clock
        .clock_i   (clock_b),
        .reset_i   (reset_capture),

        //--------------------------------------------------------------------
        //  Wishbone (SPEC B4) interconnect:
        .cyc_i     (cap_cyc),
        .stb_i     (cap_stb),
        .we_i      (cap_we ),
        .ack_o     (cap_ack),
        .wat_o     (cap_wat),
        .rty_o     (cap_rty),
        .err_o     (cap_err),
        .adr_i     (cap_adr),
        .dat_i     (cap_dtx),
        .dat_o     (cap_drx),

        //--------------------------------------------------------------------
        //  External antenna data:
        .signal_e_i(antenna),

        //-------------------------------------------------------------------------
        //  Source signals, and debug/info:
        .enabled_o(cx_enabled),
        .strobe_o (cx_strobe),
        .middle_o (cx_middle),
        .signal_o (cx_signal),
        .centred_o(cx_locked),
        .debug_o  (cx_debug)
        );



   //-------------------------------------------------------------------------
   //
   //  RAW-DATA ACQUISITION-CONTROL & READ-BACK.
   //
   //-------------------------------------------------------------------------
   //  Local synchronous reset for the raw-data acquisition unit.
   (* NOMERGE *) reg reset_acquire = 1'b1;

   always @(posedge clock_b)
     reset_acquire <= #DELAY reset_b;

   //-------------------------------------------------------------------------
   //  This module controls the raw-data acquisition unit.
   //  
   //  NOTE:
   //   + acquisition registers are mapped to '0b01nnnnn'.
   //  
   tart_acquire
     #(  .AXNUM    (ANTENNAE),
         .ABITS    (SDRAM_ADDRESS_WIDTH),
         .BBITS    (BBITS),
         .PIPED    (PIPED),     // Wishbone mode settings
         .RESET    (0),
         .CHECK    (CHECK),
         .DELAY    (DELAY)
         ) ACQUIRE
       ( .clock_i  (clock_b),
         .reset_i  (reset_acquire),

         //  Raw-data inputs.
         .locked_i (cx_enabled),
         .strobe_i (cx_strobe),
         .middle_i (cx_middle),
         .signal_i (cx_signal),

         //  Wishbone (SPEC B4) bus for raw-data and visibilities.
         .cyc_i    (acq_cyc),
         .stb_i    (acq_stb),
         .we_i     (acq_we),
         .ack_o    (acq_ack),
         .wat_o    (acq_wat),
         .rty_o    (acq_rty),
         .err_o    (acq_err),
         .adr_i    (acq_adr),
         .dat_i    (acq_dtx),
         .dat_o    (acq_drx),

         .io_busy_i(spi_busy),

         //  Memory controller signals (bus-domain).
         .mcb_ce_o (cmd_enable),
         .mcb_wr_o (cmd_write),
         .mcb_rdy_i(cmd_ready),
         .mcb_ack_i(data_out_ready),
         .mcb_adr_o(cmd_address),
         .mcb_dat_i(data_out),
         .mcb_dat_o(cmd_data_in),

         //  Debug signals.
         .enabled_o(aq_enabled),
         .state_o  (aq_state)
         );



`ifdef __USE_CORRELATORS
   //-------------------------------------------------------------------------
   //     
   //  CORRELATOR / VISIBILITIES BLOCK.
   //     
   //-------------------------------------------------------------------------
   //  Local synchronous reset for the DSP functional unit.
   (* NOMERGE *) reg reset_dsp = 1'b1;

   always @(posedge clock_b)
     reset_dsp <= #DELAY reset_b;

   //-------------------------------------------------------------------------
   //  NOTE:
   //   + system-control registers are mapped to '0b10nnnnn'.
   //  
   tart_dsp
     #( .AXNUM(ANTENNAE),       // number of attached antennae
        .ACCUM(ACCUM),          // accumulator bit-width
        .TRATE(TRATE),          // time multiplexing (TMUX) rate
        .TBITS(TBITS),          // TMUX counter bit-width
        .NREAD(NREAD),          // visibilities read-count
        .RBITS(RBITS),
        .XBITS(XBITS),          // visibilities-bank address bit-width
        .CBITS(CBITS),          // correlator address bit-width
        //  Wishbone settings:
        .PIPED(PIPED),          // Wishbone pipelined mode?
        .CHECK(CHECK),          // bus sanity-checking?
        .VIZWR(VIZWR),          // bidirectional streaming access?
        //  Simulation-only options:
        .DELAY(DELAY)           // simulation-only settings
        ) DSP
       (//--------------------------------------------------------------------
        .clk_x(clk_x),          // correlator clock
        .clk_i(clock_b),        // Wishbone/system clock
        .rst_i(reset_dsp),      // bus/system reset

        //--------------------------------------------------------------------
        //  Captured, oversampled antenna control & data signals:
        .vld_i(cx_enabled),
        .new_i(cx_strobe),      // strobes before each new sample
        .sig_i(cx_signal),      // recovered signal

        //--------------------------------------------------------------------
        //  Wishbone (SPEC B4) bus between DSP and acquisition unit:
        .cyc_i(dsp_cyc),
        .stb_i(dsp_stb),
        .we_i (dsp_we),
        .ack_o(dsp_ack),
        .wat_o(dsp_wat),
        .rty_o(dsp_rty),
        .err_o(dsp_err),
        .adr_i(dsp_adr),
        .dat_i(dsp_dtx),
        .dat_o(dsp_drx),

        //--------------------------------------------------------------------
        //  Stream Core-Enable:
        .sce_i(spi_busy),

        //  Correlator control & status signals:
        .enabled_o  (vx_enabled ),
        .pending_o  (vx_pending ),
        .newblock_o (vx_newblock),
        .checksum_o (vx_checksum),
        .streamed_o (vx_streamed),

        //--------------------------------------------------------------------
        //  Miscellaneous debugging/status signals:
        .bank_o     (vx_bank),
        .overflow_o (vx_overflow),
        .stuck_o    (vx_stuck),
        .limp_o     (vx_limp)
        );


`else // !`ifdef __USE_CORRELATORS
   //-------------------------------------------------------------------------
   //  DSP not used, so clamp some signals.
   //-------------------------------------------------------------------------
   assign vx_streamed = 1'b0;
   assign vx_newblock = 1'b0;
   assign vx_pending  = 1'b0;
   assign vx_checksum = 24'h0ff1ce;

`endif //  !`ifdef __USE_CORRELATORS



   //-------------------------------------------------------------------------
   //
   //  RESET HANDLER.
   //
   //-------------------------------------------------------------------------
   //  NOTE:
   //   + system-control registers are mapped to '0b11nnnnn'.
   //  
   tart_control
     #( .WIDTH(BBITS),
        .RTIME(4),
        .CHECK(0),
        .PIPED(1),
        .DELAY(DELAY)
        ) CONTROL
       (
        .clk_i(clock_b),
        .rst_i(reset_b),

        .cyc_i(sys_cyc),
        .stb_i(sys_stb),
        .we_i (sys_we),
        .ack_o(sys_ack),
        .wat_o(sys_wat),
        .rty_o(sys_rty),
        .err_o(sys_err),
        .adr_i(sys_adr),
        .dat_i(sys_dtx),
        .dat_o(sys_drx),

        .status_i(sys_status),
        .extra_i (spi_status),
        .reset_ni(reset_n),
        .reset_o (reset)
        );



endmodule // tart
