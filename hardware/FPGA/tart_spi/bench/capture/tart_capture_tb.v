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
 * Testbench for TART's signal-capturing circuitry.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

module tart_capture_tb;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION SETTINGS.
   //
   //-------------------------------------------------------------------------
   parameter ALIGN = 1;
   parameter CYCLE = 1;
   parameter RATIO = 12;
   parameter RBITS = 4;
   parameter RMAX  = RATIO-1;
   parameter HALF  = RATIO>>1;
   parameter TICKS = 2;

   parameter WIDTH = 24;
   parameter MSB   = WIDTH-1;
   parameter ABITS = 20;
   parameter ASB   = ABITS-1;

   parameter SHAKE = 3;
   parameter NOISY = 0;
   parameter DELAY = 3;



   //-------------------------------------------------------------------------
   //
   //  SIMULATION SIGNALS.
   //
   //-------------------------------------------------------------------------
   wire [MSB:0] daq_x, sig_e;
   wire         vld_e, vld_x, new_x;
   reg          ce_e = 1'b0;

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



   //-------------------------------------------------------------------------
   //
   //  SIMULATION STIMULI.
   //
   //-------------------------------------------------------------------------
   //  Length of the simulation.
   parameter TIMEOUT = DE << 7;

   //  Simulation-stimulus signals.
   reg [3:0]    delay = 7;
   reg [2:0]    sleep = 0;
   reg          check = 1'b0;
   reg          poll = 1'b0;
   reg          invert = 1;
   reg [4:0]    source = 4;
   integer      samples, errors;


   initial begin : ACQ_TB
      $dumpfile ("../vcd/cap_tb.vcd");
      $dumpvars;

      //-------------------------------------------------------------------------
      $display("%12t:\tIssuing RESET.", $time);
      #33 b_rst = 1'b1;
      #DB spi_req_b = 0;
      #DE b_rst = 1'b0;

      //-------------------------------------------------------------------------
      $display("%12t:\tEnabling data-capture, and selecting antenna %1d.", $time, source);
      #DE wr = 1; adr = 2'b11; dtx = {3'h4, source};
      #DB while (!done) #DB;

      //-------------------------------------------------------------------------
      $display("%12t:\tEnabling signal centring, and setting initial phase-delay (%1d).",
               $time, delay);
      #DE wr = 1; adr = 2'b00; dtx = {2'b10, invert, 1'b0, delay};
      #DB while (!done) #DB;

      //-------------------------------------------------------------------------
      #DE $display("%12t:\tEnabling signal checking.", $time);
      check = 1;

      //-------------------------------------------------------------------------
      $display("%12t:\tReading back the status & mode registers.", $time);
      #DE rd = 1; adr = 2'b00; #DB while (!done) #DB;
      #DE rd = 1; adr = 2'b01; #DB while (!done) #DB;
      #DE rd = 1; adr = 2'b10; #DB while (!done) #DB;
      #DE rd = 1; adr = 2'b11; #DB while (!done) #DB;

      $display("%12t:\tEnabling polling of phase-delay measurements.", $time);
      #DE poll = 1;

      /*
      //-------------------------------------------------------------------------
      $display("%12t:\tSetting fixed delay of 5 ticks, and update-mode to DRIFT.", $time);
      #DE wr = 1; adr = 2'b00; dtx = 8'hC5; // align + drift antenna 0x04
      #DB while (!done) #DB;
       */

      #TIMEOUT poll = 0;
      $display("\n");
      $display("%12t:\tSignal source samples\t= \t%7d", $time, samples);
      $display("%12t:\tPhase-delay errors   \t= \t%7d", $time, errors);
      $finish;
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
   //  Once polling is enabled, continuously read back the phase & delta,
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
        cnt_x       <= #DELAY cnt_x + 1;
     end


   //-------------------------------------------------------------------------
   //  Check the captured signal values, vs. the generated data.
   //-------------------------------------------------------------------------
   wire [MSB:0] raw_val = vals[cnt_x];

   always @(posedge clk_x)
     if (b_rst)
       errors <= #DELAY 0;
     else if (stb_x && raw_val != daq_x) begin
        $display("%12t:\tGenerated and capture values disagree ('0x%6x' vs '0x%6x')",
                 $time, vals[cnt_x], daq_x);
        errors <= #DELAY check ? errors + 1 : errors;
     end

   always @(posedge clk_x)
     if (!check)
       samples <= #DELAY 0;
     else if (stb_x)
       samples <= #DELAY samples + 1;


   //-------------------------------------------------------------------------
   //
   //  GENERATE RANDOM ACQUISITION DATA (GRAD).
   //
   //-------------------------------------------------------------------------
   fake_signal
     #( .WIDTH(WIDTH),          // signal bit-width
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
        .clock_x   (clk_x),
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

        //  Correlator-domain acquisition data & status signals:
        .enable_x_o(vld_x),      // acquired (and oversampled) data
        .strobe_x_o(new_x),      // outputs
        .signal_x_o(daq_x)
        );



endmodule // tart_capture_tb
