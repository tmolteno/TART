`timescale 1ns/100ps
/*
 * Module      : bench/tart_acquire_tb.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
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
 * Simulates two visibility calculations, using TART's 'tart_dsp' logic core,
 * and then reads them back, via a streaming data-transfer interface.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

module tart_acquire_tb;


   //-------------------------------------------------------------------------
   //
   //  SETTINGS.
   //
   //-------------------------------------------------------------------------
   //  Antenna bit-widths and sampling-rates:
   parameter AXNUM = 24;        // Number of antenna inputs
   parameter MSB   = AXNUM-1;
   parameter TRATE = 12;        // Time-multiplexing rate
   parameter RATIO = TRATE;
   parameter TBITS = 4;         // TMUX bits
   parameter CBITS = 6;         // acquire '2^CBITS' samples
   parameter CSB   = CBITS-1;   // MSB of sample-counter
   parameter COUNT = 1<<CBITS;  // acquire '2^CBITS' samples

   //-------------------------------------------------------------------------
   //  External Wishbone-like bus setttings:
   parameter BBITS = 8;         // Wishbone data bit-width
   parameter BSB   = BBITS-1;   // MSB of Wishbone data
   parameter SBITS = 2;         // acquisition-unit, register-select bits
   parameter SSB   = SBITS-1;   // MSB of register-select

   //-------------------------------------------------------------------------
   //  Fake MCB values:
   parameter START = 1000;      // startup delay
   parameter TICKS = 7;         // read-latency (in clock-cycles)
   parameter ABITS = 10;        // address bit-width
   parameter ASB   = ABITS-1;   // address MSB

   //-------------------------------------------------------------------------
   //  Extra simulation options:
   parameter SHAKE = 2;         // signal noise (jitter + offset)
   parameter PHASE = 0;         // phase-delay for the generated signal
   parameter HAMEN = 1;         // use Hamster SDRAM (0/1)?
   parameter NOISY = 0;         // extra debug info?
   parameter DELAY = 3;         // simulated combinational-delay (ns)


   //-------------------------------------------------------------------------
   //
   //  SIGNALS.
   //
   //-------------------------------------------------------------------------
   //  Antenna signals:
   wire [MSB:0] sig_x, sig_e;
   wire         vld_e, new_e;
   wire         vld_x, new_x;
   reg          ce_e = 1'b0;

   //-------------------------------------------------------------------------
   //  Clock signals:
   reg          clk_x = 1'b1, clk_e = 1'b1; // signal & sample clocks
   reg          b_clk = 1'b1, b_rst = 1'b0; // bus clock & reset
   wire         n_clk = ~b_clk;
   wire         rst_x = b_rst;

   //-------------------------------------------------------------------------
   //  Wishbone signals:
   wire [BSB:0] dat, val, drx;
   wire         cyc, stb, we, ack, wat, rty, err;
   reg [SSB:0]  adr;
   reg [BSB:0]  dtx;

   //-------------------------------------------------------------------------
   //  Signals for driving the WB bus-transfer functional unit.
   reg          rd = 1'b0, wr = 1'b0, wt = 1'b0;
   reg          read;
   wire         busy, done, fail;

   //-------------------------------------------------------------------------
   //  Fake MCB signals:
   wire [ASB:0] cmd_address;    // memory-controller signals (unused)
   wire         cmd_avail, cmd_request, cmd_write, cmd_ready;
   wire [31:0]  cmd_data_in, cmd_data_out;

   wire         mcb_avail, mcb_ready;
   wire [31:0]  mcb_dat_i;

   //-------------------------------------------------------------------------
   //  Read-back stuffs:
   reg          io_busy = 1'b0;
   wire         aq_enabled, oflow;
   wire [2:0]   cap_state;



   //-------------------------------------------------------------------------
   //
   //  SIMULATION CLOCKS.
   //
   //-------------------------------------------------------------------------
   parameter CLKX  = 2.5;        // half-period of the sample clock
   parameter CLKB  = 5;
   parameter CLKE  = 30;

   always #CLKE clk_e <= ~clk_e; // external signal clock
   always #CLKB b_clk <= ~b_clk; // bus clock
   always #CLKX clk_x <= ~clk_x; // correlator clock

   parameter DE    = CLKE*2;
   parameter DB    = CLKB*2;

   parameter BIGLY = DE*128;



   //-------------------------------------------------------------------------
   //
   //  SIMULATION STIMULI.
   //
   //-------------------------------------------------------------------------
   //  Length of the simulation.
   parameter LIMIT = DE << 6;


   //-------------------------------------------------------------------------
   //  Simulate data-acquisition, followed by Wishbone read-back.
   //-------------------------------------------------------------------------
   integer      num = 0;
   integer      ptr = 0;
   integer      source = 23;

   wire [MSB:0] sig_b, sig_w, sig_n, sig_p;
   reg [3:0]    delay = PHASE;

   assign sig_b = delay[0] ? sig_n : sig_p;

   initial begin : SIM_BLOCK
      if (CBITS < 7) begin
         $dumpfile ("../vcd/acq_tb.vcd");
         $dumpvars;
      end

      //----------------------------------------------------------------------
      $display("%12t:\tIssuing RESET.", $time);
      #33 b_rst = 1'b1;
      #DB io_busy = 0;
      #DE b_rst = 1'b0;

      //----------------------------------------------------------------------
      $display("%12t:\tEnabling acquisition.", $time);
      #DE wr = 1; adr = 2'b11; dtx = 8'h80;
      #DB while (!done) #DB;

      //----------------------------------------------------------------------
//       #DE; #DE;
//       #DE $finish;
   end // block: SIM_BLOCK


   //-------------------------------------------------------------------------
   //  Exit if the simulation appears to have stalled.
   //-------------------------------------------------------------------------
   initial begin : SIM_FAILED
      $display("%12t:\tSimulation TIMEOUT limit:\t%12d", $time, LIMIT);
      #LIMIT $display ("\nTIMEOUT!\n");
      $finish;
   end // SIM_FAILED


   //-------------------------------------------------------------------------
   //  Generate capture-unit control-signals.
   //-------------------------------------------------------------------------
   always @(posedge b_clk)
     if (b_rst || rd || wr)
       {wr, rd} <= #DELAY 2'b00;

   //  Keep track of bus-mode.
   always @(posedge b_clk)
     if (rd)
       read <= #DELAY 1'b1;
     else if (wr)
       read <= #DELAY 1'b0;
     else
       read <= #DELAY read;


   //-------------------------------------------------------------------------
   //  Synchronise the strobe across domains.
   //-------------------------------------------------------------------------
   reg             strobe = 1'b0, strobe_1 = 1'b0, strobe_0 = 1'b0;
   reg             locked = 1'b0;
   reg [MSB:0]     signal;
   reg [3:0]       mcount = 0;
   wire            middle;

   assign middle = mcount == 2;

   always @(posedge b_clk or posedge new_e)
     if (new_e)
       strobe_0 <= #DELAY 1'b1;
     else
       strobe_0 <= #DELAY 1'b0;

   always @(posedge b_clk)
     {strobe, strobe_1} <= #DELAY {strobe_1, strobe_0};

   always @(posedge b_clk)
     if (strobe || b_rst)
       mcount <= #DELAY 0;
     else
       mcount <= #DELAY mcount + locked;

   always @(posedge b_clk)
     if (strobe) begin
        locked <= #DELAY vld_e;
        signal <= #DELAY sig_w;
     end
     else begin
        locked <= #DELAY locked;
        signal <= #DELAY signal;
     end



   //-------------------------------------------------------------------------
   //     
   //     DATA-ACQUISITION CONTROL AND READ-BACK (DACARB).
   //     
   //-------------------------------------------------------------------------
   assign mcb_avail = HAMEN ? ham_avail : cmd_avail;
   assign mcb_ready = HAMEN ? ham_ready : cmd_ready;
   assign mcb_dat_i = HAMEN ? ham_data_out : cmd_data_out;

   tart_acquire
     #( .AXNUM(AXNUM),
        .ABITS(ABITS),
        .BBITS(BBITS),
        .DELAY(DELAY)
        ) ACQ
       (
        .clock_i  (b_clk),      // bus-domain clock & reset
        .reset_i  (b_rst),

        .locked_i (locked),
        .strobe_i (strobe),
        .middle_i (middle),
        .signal_i (signal),

        .cyc_i    (cyc),        // Wishbone (SPEC B4) interconnect
        .stb_i    (stb),
        .we_i     (we),
        .ack_o    (ack),
        .wat_o    (wat),
        .rty_o    (rty),
        .err_o    (err),
        .adr_i    (adr),
        .dat_i    (dtx),
        .dat_o    (drx),

        .io_busy_i(io_busy),

        .mcb_ce_o (cmd_request), // memory controller signals (bus-domain)
        .mcb_rdy_i(mcb_avail),
        .mcb_wr_o (cmd_write),
        .mcb_ack_i(mcb_ready),
        .mcb_adr_o(cmd_address),
        .mcb_dat_i(mcb_dat_i),
        .mcb_dat_o(cmd_data_in),

        .enabled_o(aq_enabled), // status & debug info
        .oflow_o  (oflow),
        .state_o  (cap_state)
        );



   //-------------------------------------------------------------------------
   //
   //  NOT UNDER TEST (NUT).
   //
   //-------------------------------------------------------------------------
   //  Drive the bus signals, for setting the capture-unit's parameters.
   wb_transfer
     #( .ASYNC(1), .CHECK(1), .PIPED(1), .READ(1), .WRITE(1), .FRAME(0),
        .DELAY(DELAY)
        ) XFER
       (
        .clk_i  (b_clk),
        .rst_i  (b_rst),

        .cyc_o  (cyc),
        .stb_o  (stb),
        .we_o   (we ),
        .ack_i  (ack),
        .wat_i  (wat),
        .rty_i  (rty),
        .err_i  (err),

        .frame_i(1'b0),
        .read_i (rd),
        .write_i(wr),
        .busy_o (busy),
        .done_o (done),
        .fail_o (fail)
        );



   //-------------------------------------------------------------------------
   //
   //  FAKE MEMORY-CONTROLLER (FaMeCo).
   //
   //-------------------------------------------------------------------------
   mcb_dummy
     #( .WIDTH   (32), .ABITS(ABITS), .TICKS(TICKS),
        .START(START), .DELAY(DELAY)
        ) MCB
       (
        .clock_i  (b_clk),
        .reset_i  (b_rst),
        .active_o (cmd_avail),
        .request_i(cmd_request),
        .write_i  (cmd_write),
        .ready_o  (cmd_ready),
        .address_i(cmd_address[ASB:0]),
        .bytes_i  (4'b1111),
        .data_i   (cmd_data_in),
        .data_o   (cmd_data_out)
        );



   //-------------------------------------------------------------------------
   //
   //  HAMSTER AMAZING MEMORY INTERFACE-SLASH-CONTROLLER (HAMISC).
   //
   //-------------------------------------------------------------------------
   wire [20:0] ham_address;
   wire        ham_avail, ham_request, ham_write, ham_ready;
   wire [31:0] ham_data_in, ham_data_out;

   wire        SDRAM_CLK, SDRAM_CKE, SDRAM_CS, SDRAM_RAS, SDRAM_CAS, SDRAM_WE;
   wire [1:0]  SDRAM_DQM, SDRAM_BA;
   wire [12:0] SDRAM_ADDR;
   wire [15:0] SDRAM_DQ;

   assign ham_request = cmd_request;
   assign ham_write   = cmd_write;
   assign ham_data_in = cmd_data_in;
   assign ham_address = {{20-ABITS{1'b0}}, cmd_address};

   SDRAM_Controller_v
     #( .sdram_address_width (22),
        .sdram_column_bits   (8),
        .sdram_startup_cycles(80),
        .cycles_per_refresh  (10000)
        ) HAMSDRAM
       (
        .clk            (b_clk),
        .reset          (b_rst),

        .cmd_ready      (ham_avail),     // (O) MCB has initialised?
        .cmd_enable     (ham_request),   // (I) start a MCB operation
        .cmd_wr         (ham_write),     // (I) signal a write transaction
        .cmd_address    (ham_address),   // (I) data address
        .cmd_byte_enable(4'b1111),       // (I) individual byte write-selects
        .cmd_data_in    (ham_data_in),   // (I) write data
        .data_out       (ham_data_out),  // (O) requested data
        .data_out_ready (ham_ready),     // (O) requested data ready?

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



   //-------------------------------------------------------------------------
   //
   //  GENERATE RANDOM ACQUISITION DATA (GRAD).
   //
   //-------------------------------------------------------------------------
   //  Start acquisition after a reset.
   always @(posedge clk_e)
     ce_e <= #DELAY !b_rst ? 1 : 0 ;

   fake_signal
     #( .WIDTH(AXNUM),          // signal bit-width
        .RATIO(RATIO),
        .SHAKE(SHAKE),          // jitter & offset parameter
        .DELAY(DELAY)           // combinational simulation-delay
        ) FAKE
     (  .clock (clk_x),
        .reset (b_rst),
        .enable(ce_e ),
        .locked(vld_e),
        .strobe(new_e),
        .signal(sig_e)
        );



   //-------------------------------------------------------------------------
   //
   //  IOB, DDR, SIGNAL-CAPTURE REGISTERS.
   //
   //-------------------------------------------------------------------------
   //  NOTE: This allow half-rate sampling, but at the expense of twice the
   //    usage of routing-resources.
   IDDR2
     #( .DDR_ALIGNMENT("C0"),
        .SRTYPE("SYNC")
        ) IOBS [MSB:0]
     ( .C0(b_clk),
       .C1(n_clk),
       .R (b_rst),
       .CE(vld_e),
       .D (sig_e),
       .Q0(sig_p),
       .Q1(sig_n)               // lags by 180 degrees
       );

   //-------------------------------------------------------------------------
   //  Programmable delay that is used to phase-shift the incoming signal.
   //-------------------------------------------------------------------------
   shift_reg
     #(  .DEPTH(8),
         .ABITS(3),
         .DELAY(DELAY)
         ) SHREG [MSB:0]
       ( .clk(b_clk),
         .ce (vld_e),
         .a  (delay[3:1]),
         .d  (sig_b),
         .q  (sig_w)
         );



endmodule // tart_acquire_tb
