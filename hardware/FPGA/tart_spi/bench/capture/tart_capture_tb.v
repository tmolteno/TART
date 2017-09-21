`timescale 1ns/100ps
/*
 * Module      : bench/acquire/tart_capture_tb.v
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
 * Testbench for TART's signal-capturing circuitry.
 * 
 * NOTE:
 *  + has a corresponding 'cap_tb.gtkw' file for showing the source, and
 *    captured, signal waveforms;
 * 
 * TODO:
 * 
 */

module tart_capture_tb;



   //-------------------------------------------------------------------------
   //
   //  SIMULATION PARAMETERS/SETTINGS.
   //
   //-------------------------------------------------------------------------
   //  Testbench-specific settings:
   parameter PHASE = 2; // simulation phase-delay
   parameter SHAKE = 2; // how much random phase-jitter + offset?
//    parameter SHAKE = 0; // how much random phase-jitter + offset?
   parameter LIMIT = 4; // simulate for '2^LIMIT' samples
   parameter DEBUG = 0; // debug/fake-data mode ON (0/1/2/3)?
   parameter NOISY = 0;
   parameter DELAY = 3;

   //  Capture-module parameters:
   parameter ALIGN = 1;
   parameter CYCLE = 1;
   parameter RATIO = 12;
   parameter RBITS = 4;
   parameter RMAX  = RATIO-1;
   parameter HALF  = RATIO>>1;
   parameter TICKS = 4;

   //  Antenna/source and address bit-widths:
   parameter WIDTH = 24;
   parameter MSB   = WIDTH-1;
   parameter ABITS = 20;
   parameter ASB   = ABITS-1;



   //-------------------------------------------------------------------------
   //
   //  SIMULATION SIGNALS.
   //
   //-------------------------------------------------------------------------
   wire [MSB:0] sig_e; // daq_x, 
   wire         vld_e; // , vld_x, new_x;
   reg          ce_e = 1'b0;

   reg          pre_x, new_x, vld_x;
   reg [MSB:0]  daq_x;
   wire         pre_w = tc_stb && !pre_x;
   wire         new_w = pre_x  && !new_x;

   wire [ASB:0] cmd_address;    // memory-controller signals (unused)
   wire         cmd_enable, cmd_write;
   reg          cmd_ready = 1'b0;
   wire [31:0]  cmd_data_in;

   reg          clk_x = 1'b1, clk_e = 1'b1; // signal & sample clocks
   reg          b_clk = 1'b1, b_rst = 1'b0; // bus clock & reset
   wire         n_clk = ~b_clk;

   //-------------------------------------------------------------------------
   //  Capture-unit control-signals.
   reg [3:0]    tc_delay;       // set the system phase-delay
   reg          tc_enable;      // enable raw-data capture

   //  Alignment-unit control-signals.
   reg [4:0]    tc_select, tc_number; // select antenna to calibrate
   reg [3:0]    tc_phase, tc_delta;   // measured phase-shift
   reg          tc_centre, tc_valid, tc_error, tc_retry;
   reg          tc_drift, tc_invert;

   //  Fake-/debug- data control-signals.
   reg          tc_debug, tc_shift, tc_count;

   //  Debug & info signals.
   wire         tc_en, tc_stb, tc_mid, tc_cen, tc_dbg;
   wire [MSB:0] tc_sig;

   //-------------------------------------------------------------------------
   //  Signals related to the raw-data acquisition unit.
   reg          spi_req_b;
   wire [2:0]   tart_state;

   //-------------------------------------------------------------------------
   //  Signals for driving the WB bus-transfer functional unit.
   reg          rd = 1'b0, wr = 1'b0, wt = 1'b0;
   reg          read;
   wire         busy, done, fail;

   //  Wishbone signals.
   wire         cyc, stb, we;  // master signals
   reg [1:0]    adr;
   reg [7:0]    dtx;
   wire         ack, wat, rty, err; // slave signals
   wire [7:0]   drx;



   //-------------------------------------------------------------------------
   //
   //  SIMULATION CLOCKS.
   //
   //-------------------------------------------------------------------------
   parameter CLKX = 2.5;        // half-period of the sample clock
   parameter CLKB = 5;
   parameter CLKE = 30;

   always #CLKE clk_e <= ~clk_e; // external signal clock
   always #CLKB b_clk <= ~b_clk; // bus clock
   always #CLKX clk_x <= ~clk_x; // correlator clock

   parameter DE = CLKE*2;
   parameter DB = CLKB*2;
   parameter DX = CLKX*2;



   //-------------------------------------------------------------------------
   //
   //  SIMULATION STIMULI.
   //
   //-------------------------------------------------------------------------
   //  Length of the simulation.
   parameter TIMEOUT = DE << LIMIT;

   //  Simulation-stimulus signals.
   reg [3:0]    delay = PHASE-1;
   reg [2:0]    sleep = 0;
   reg          check = 1'b0;
   reg          poll = 1'b0;
   reg          invert = 0;
//    reg [4:0]    source = 4;
   reg [4:0]    source = 2;
   integer      samples, errors;


   initial begin : CAP_TB
      if (LIMIT < 6) begin
         $dumpfile ("../vcd/cap_tb.vcd");
         $dumpvars;
      end

      //----------------------------------------------------------------------
      //----------------------------------------------------------------------
      $display("%12t:\tIssuing RESET.", $time);
      #33 b_rst = 1'b1;
      #DB spi_req_b = 0;
      #DE b_rst = 1'b0;


      //----------------------------------------------------------------------
      //----------------------------------------------------------------------
      if (DEBUG) begin
         $display("%12t:\tEnabling signal centring, and setting initial phase-delay (%1d).",
                  $time, delay);
         #DE wr = 1; adr = 2'b10; dtx = {1'b1, DEBUG[0], DEBUG[1], 5'h0};
         #DB while (!done) #DB;
      end


      //----------------------------------------------------------------------
      //----------------------------------------------------------------------
      $display("%12t:\tEnabling data-capture, and selecting antenna %1d.",
               $time, source);
      #DE wr = 1; adr = 2'b11; dtx = {3'h4, source};
      #DB while (!done) #DB;

      //----------------------------------------------------------------------
      $display("%12t:\tEnabling signal centring, and setting initial phase-delay (%1d).",
               $time, delay);
      #DE wr = 1; adr = 2'b00; dtx = {2'b10, invert, 1'b0, delay};
      #DB while (!done) #DB;

      //----------------------------------------------------------------------
      $display("%12t:\tReading back the status & mode registers.", $time);
      #DE rd = 1; adr = 2'b00; #DB while (!done) #DB;
      #DE rd = 1; adr = 2'b01; #DB while (!done) #DB;
      #DE rd = 1; adr = 2'b10; #DB while (!done) #DB;
      #DE rd = 1; adr = 2'b11; #DB while (!done) #DB;

      //----------------------------------------------------------------------
      #DE $display("%12t:\tEnabling signal checking.", $time);
      while (!new_x) #DX; #DX check = 1;

      $display("%12t:\tEnabling polling of phase-delay measurements.", $time);
      poll = 1;

      //----------------------------------------------------------------------
      #TIMEOUT while (!new_x) #DX; poll = 0; check = 0;
      $display("%12t:\tSignal source samples\t= \t%7d", $time, samples);
      $display("%12t:\tPhase-delay errors   \t= \t%7d", $time, errors);


      //----------------------------------------------------------------------
      //----------------------------------------------------------------------
      #DE delay = delay + 1; errors = 0;
      $display("\n");
      $display("%12t:\tSetting new phase-delay (%1d).", $time, delay);
      #DE wr = 1; adr = 2'b00; dtx = {2'b10, invert, 1'b0, delay};
      #DB while (!done) #DB;
      $display("%12t:\tBeginning capture & check.", $time);
      #DE while (!new_x) #DX; poll = 1; check = 1;

      #TIMEOUT while (!new_x) #DX; poll = 0; check = 0;
      $display("%12t:\tSignal source samples\t= \t%7d", $time, samples);
      $display("%12t:\tPhase-delay errors   \t= \t%7d", $time, errors);


      //----------------------------------------------------------------------
      //----------------------------------------------------------------------
      #DE delay = delay + 1; errors = 0;
      $display("\n");
      $display("%12t:\tSetting new phase-delay (%1d).", $time, delay);
      #DE wr = 1; adr = 2'b00; dtx = {2'b10, invert, 1'b0, delay};
      #DB while (!done) #DB;
      $display("%12t:\tBeginning capture & check.", $time);
      #DE while (!new_x) #DX; poll = 1; check = 1;

      #TIMEOUT while (!new_x) #DX; poll = 0; check = 0;
      $display("%12t:\tSignal source samples\t= \t%7d", $time, samples);
      $display("%12t:\tPhase-delay errors   \t= \t%7d", $time, errors);


      //----------------------------------------------------------------------
      //----------------------------------------------------------------------
      #DE delay = delay + 1; errors = 0;
      $display("\n");
      $display("%12t:\tSetting new phase-delay (%1d).", $time, delay);
      #DE wr = 1; adr = 2'b00; dtx = {2'b10, invert, 1'b0, delay};
      #DB while (!done) #DB;
      $display("%12t:\tBeginning capture & check.", $time);
      #DE while (!new_x) #DX; poll = 1; check = 1;

      #TIMEOUT while (!new_x) #DX; poll = 0; check = 0;
      $display("%12t:\tSignal source samples\t= \t%7d", $time, samples);
      $display("%12t:\tPhase-delay errors   \t= \t%7d", $time, errors);

      #DE $finish;
   end


   //-------------------------------------------------------------------------
   //  Start acquisition after a reset.
   always @(posedge clk_e)
     ce_e <= #DELAY !b_rst ? 1 : 0 ;

   //  Display aligned data.
   always @(posedge clk_x)
     if (stb_x && NOISY)
       $display("%12t:\tDATA = %08b (%02x)", $time, daq_x, daq_x);


   //-------------------------------------------------------------------------
   //  Lift signals into the 12x domain.
   //-------------------------------------------------------------------------
   always @(posedge clk_x)
     if (b_rst) begin
        pre_x <= #DELAY 1'b0;
        new_x <= #DELAY 1'b0;
        vld_x <= #DELAY 1'b0;
        daq_x <= #DELAY  'bz;
     end
     else begin
        pre_x <= #DELAY pre_w;
        new_x <= #DELAY new_w;
        vld_x <= #DELAY tc_en;
        daq_x <= #DELAY vld_x && new_x ? tc_sig : daq_x;
     end


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
   //
   //  MAINTAIN LOCAL VERSIONS OF THE FLAGS.
   //
   //-------------------------------------------------------------------------
   always @(posedge b_clk)
     if (b_rst) begin
        {tc_select, tc_error, tc_retry, tc_delay} <= #DELAY 'bx;
        {tc_centre, tc_drift, tc_valid, tc_phase} <= #DELAY 'bx;
        {tc_enable, tc_debug, tc_count, tc_shift} <= #DELAY 'bx;
        {tc_invert, tc_delta, tc_number         } <= #DELAY 'bx;
     end
     else if (done && read)
       case (adr)
         2'b00: {tc_centre, tc_drift, tc_invert, tc_delay} <= #DELAY {drx[7:5], drx[3:0]};
         2'b01: {                     tc_delta,  tc_phase} <= #DELAY drx;
         2'b10: {tc_debug , tc_count, tc_shift, tc_number} <= #DELAY drx;
         2'b11: {tc_enable, tc_error, tc_valid, tc_select} <= #DELAY drx;
       endcase // case (adr)


   //-------------------------------------------------------------------------
   //  Once polling is enabled, continuously read-back the phase & delta,
   //  sleep for a bit, and then repeat.
   //-------------------------------------------------------------------------
   always @(posedge b_clk)
     if (poll && sleep == 0 && !(busy || rd || wr)) begin
        rd  <= #DELAY 1'b1;
        adr <= #DELAY 2'b01;
     end

   always @(posedge b_clk)
     if (poll && rd)
       sleep <= #DELAY 3'h7;
     else if (sleep > 0)
       sleep <= #DELAY sleep - 1;



   //-------------------------------------------------------------------------
   //
   //  STORE INCOMING SIGNAL VALUES, FOR LATER COMPARISON.
   //
   //-------------------------------------------------------------------------
   reg [MSB:0] vals[0:1023];
   reg [MSB:0] caps[0:1023];
   reg [9:0]   cnt_e;
   reg [9:0]   cnt_x;
   reg         stb_e = 1'b0, stb_x = 1'b0;
   wire        new_e, disp;

   assign disp = cnt_x == 16;

   always @(posedge clk_x)
     stb_e <= #DELAY new_e;

   always @(posedge clk_x)
     if (b_rst)
       cnt_e <= #DELAY 0;
     else if (stb_e) begin
        vals[cnt_e] <= #DELAY sig_e;
        cnt_e       <= #DELAY cnt_e + 1;
     end

   always @(posedge clk_x)
     stb_x <= #DELAY new_x;

   always @(posedge clk_x)
     if (b_rst)
       cnt_x <= #DELAY 0;
     else if (stb_x) begin
        caps[cnt_x] <= #DELAY daq_x;
        cnt_x       <= #DELAY vld_x ? cnt_x + 1 : cnt_x;
     end


   //-------------------------------------------------------------------------
   //  Check the captured signal values, vs. the generated data.
   //-------------------------------------------------------------------------
   wire [MSB:0] raw_val = vals[cnt_x];

   always @(posedge clk_x)
     if (b_rst)
       errors <= #DELAY 0;
     else if (stb_x && raw_val != daq_x) begin
        if (NOISY || errors < 10)
          $display("%12t:\tGenerated and captured values disagree ('0x%6x' vs '0x%6x')",
                   $time, raw_val, daq_x);
        errors <= #DELAY check ? errors + 1 : errors;
     end

   always @(posedge clk_x)
     if (!check)
       samples <= #DELAY 0;
     else if (stb_x)
       samples <= #DELAY samples + 1;


   //-------------------------------------------------------------------------
   //  Just some formatting (for GtkWave).
   //-------------------------------------------------------------------------
   //  This shows the formatted versions of the source, and captured, antenna
   //  signals (for quick comparisons in GtkWave).
   wire [MSB:0] src_val, cap_val;
   wire         nofmt, match;
   reg [MSB:0]  src_reg = 'bz;
   reg [3:0]    fmt_cnt = 4'h0;
   wire [4:0]   nxt_cnt;

   assign nxt_cnt = fmt_cnt + 1;
   assign nofmt   = fmt_cnt > 10 || !check;
   assign match   = nofmt ? 'b0 : src_val == cap_val ? 1'b1 : 'bx;
   assign src_val = nofmt ? 'bz : src_reg;
   assign cap_val = nofmt ? 'bz : daq_x;

   always @(posedge clk_x)
     if (b_rst || stb_x)
       fmt_cnt <= #DELAY 4'h0;
     else if (vld_x)
       fmt_cnt <= #DELAY nxt_cnt[3:0];

   always @(posedge clk_x)
     if (vld_x && fmt_cnt == 11)
       src_reg <= #DELAY raw_val;



   //-------------------------------------------------------------------------
   //
   //  GENERATE RANDOM ACQUISITION DATA (GRAD).
   //
   //-------------------------------------------------------------------------
   fake_signal
     #( .WIDTH(WIDTH),          // signal bit-width
        .RATIO(RATIO),
        .SHAKE(SHAKE),          // jitter & offset parameter
        .NOISY(NOISY),
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
   //  A CORE UNDER TEST & EVALUATION (ACUTE).
   //
   //-------------------------------------------------------------------------
   //  Drive the bus signals, for setting the capture-unit's parameters.
   wb_transfer
     #( .ASYNC(1), .CHECK(1), .PIPED(1), .READ(1), .WRITE(1), .FRAME(0),
        .DELAY(DELAY)
        ) XFER
       (
        .clk_i(b_clk),
        .rst_i(b_rst),
        .cyc_o(cyc),
        .stb_o(stb),
        .we_o (we),
        .ack_i(ack),
        .wat_i(wat),
        .rty_i(rty),
        .err_i(err),

        .frame_i(1'b0),
        .read_i (rd),
        .write_i(wr),
        .busy_o (busy),
        .done_o (done),
        .fail_o (fail)
        );



   //-------------------------------------------------------------------------
   //
   //  SOPHISTICATED MODULE UNDER TEST (SMUT).
   //
   //-------------------------------------------------------------------------
   //  Align and capture the staggered data.
   tart_capture
     #( .AXNUM(24),
        .ABITS(ABITS+1),
        // fake-data options:
        .MULTI(1),
        .RNG  (1),
        .CONST(0),
        .CDATA(0),
        // use additional data-capture and alignment circuitry?
        .ALIGN(ALIGN),
        .CYCLE(CYCLE),
        .RATIO(RATIO),
        .RBITS(RBITS),
        .TICKS(TICKS),          // extra delay to add, due to pipelining
        // simulation-only settings:
        .DELAY(DELAY)
        ) CAP0
       (
        .clock_e   (clk_e),
        .clock_i   (b_clk),
        .clock_n   (n_clk),
        .reset_i   (b_rst),

        //  External antenna data:
        .signal_e_i(sig_e),

        //  Wishbone (SPEC B4) bus for reading/writing settings:
        .cyc_i     (cyc),
        .stb_i     (stb),
        .we_i      (we),
        .ack_o     (ack),
        .wat_o     (wat),
        .rty_o     (rty),
        .err_o     (err),
        .adr_i     (adr),
        .dat_i     (dtx),
        .dat_o     (drx),

        //  Debug & info outputs:
        .enabled_o (tc_en ),
        .strobe_o  (tc_stb),
        .middle_o  (tc_mid),
        .signal_o  (tc_sig),
        .centred_o (tc_cen),
        .debug_o   (tc_dbg)
        );



endmodule // tart_capture_tb
