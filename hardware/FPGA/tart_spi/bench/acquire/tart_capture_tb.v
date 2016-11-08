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
   parameter ALIGN = 0;
   parameter RATIO = 12;
   parameter RBITS = 4;
   parameter RMAX  = RATIO-1;
   parameter HALF  = RATIO>>1;

   parameter WIDTH = 24;
   parameter MSB   = WIDTH-1;
   parameter ABITS = 20;
   parameter ASB   = ABITS-1;

   parameter DELAY = 3;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION SIGNALS.
   //
   //-------------------------------------------------------------------------
   wire [MSB:0] daq_x, sig_e;
   reg [MSB:0]  raw_e;          // raw, random signal data
   wire         vld_x, new_x;
   reg          ce_e = 1'b0;

   wire [ASB:0] cmd_address;    // memory-controller signals (unused)
   wire         cmd_enable, cmd_write;
   reg          cmd_ready = 1'b0;
   wire [31:0]  cmd_data_in;

   reg          clk_x = 1'b1, clk_e = 1'b1; // signal & sample clocks
   reg          b_clk = 1'b1, b_rst = 1'b0; // bus clock & reset

   //-------------------------------------------------------------------------
   //  Capture-unit control-signals.
   reg [3:0]    aq_delay;       // set the system phase-delay
   reg          aq_capture;     // enable raw-data capture

   //  Alignment-unit control-signals.
   reg [4:0]    aq_select;      // select antenna to calibrate
   reg [3:0]    aq_phase;       // measured phase-shift
   reg          aq_centre, aq_locked, aq_invalid, aq_restart;
   reg          aq_drift;

   //  Fake-/debug- data control-signals.
   reg          aq_debug, aq_shift, aq_count;

   //-------------------------------------------------------------------------
   //  Signals related to the raw-data acquisition unit.
   reg          spi_req_b;
   wire [2:0]   tart_state;

   //-------------------------------------------------------------------------
   //  Signals for driving the WB bus-transfer functional unit.
   reg          rd = 1'b0, wr = 1'b0;
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
   initial begin : ACQ_TB
      $dumpfile ("../vcd/acq_tb.vcd");
      $dumpvars;

      #33 b_rst = 1'b1;
      #DB spi_req_b = 0;
      #DE b_rst = 1'b0;

      #DE wr = 1; adr = 2'b00; dtx = 8'h85;
      #DB while (!done) #DB;

//       #DE wr = 1; adr = 2'b01; dtx = 8'h84; // align antenna 0x04
      #DE wr = 1; adr = 2'b01; dtx = 8'hC4; // align + drift antenna 0x04
      #DB while (!done) #DB;

      #720; #720;
      #720; #720;
      #720 $finish;
   end


   //-------------------------------------------------------------------------
   //
   //  GENERATE RANDOM DATA TO BE ACQUIRED.
   //
   //-------------------------------------------------------------------------
   //  Start acquisition after a reset.
   always @(posedge clk_e)
     ce_e <= #DELAY !b_rst ? 1 : 0 ;

   //  Generate random data.
   always @(posedge clk_e)
     raw_e <= #DELAY $random;

   //  Display aligned data.
   always @(posedge clk_x)
     if (new_x) $display("%8t: DATA = %08b (%02x)", $time, daq_x, daq_x);


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

   //  Set local versions of the flags.
   always @(posedge b_clk)
     if (b_rst) begin
        {aq_restart, aq_select, aq_delay, aq_phase} <= #DELAY 14'hx;
        {aq_invalid, aq_locked, aq_count, aq_shift} <= #DELAY 4'hx;
        {aq_capture, aq_centre, aq_debug, aq_drift} <= #DELAY 4'h0;
     end
     else if (done && read)
       case (adr)
         2'b00: {aq_capture, aq_delay} <= #DELAY {drx[7], drx[3:0]};
         2'b01: {aq_centre, aq_drift, aq_select} <= #DELAY {drx[7:6], drx[4:0]};
         2'b10: {aq_debug, aq_count, aq_shift} <= #DELAY {drx[7], drx[1:0]};
         2'b11: {aq_invalid, aq_locked, aq_phase} <= #DELAY {drx[7:6], drx[3:0]};
       endcase // case (adr)


   //-------------------------------------------------------------------------
   //
   //  DEVICES UNDER TEST.
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
   //  Align and capture the staggered data.
   //-------------------------------------------------------------------------
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
        .RATIO(RATIO),
        .RBITS(RBITS),
        // simulation-only settings:
        .DELAY(DELAY)
        ) CAP0
       (
        .clock_x   (clk_x),
        .clock_e   (clk_e),
        .clock_i   (b_clk),
        .reset_i   (b_rst),

        //  External antenna data:
        .signal_e_i(sig_e),

        //  Wishbone (SPEC B4) bus for reading/writing settings:
        .cyc_i(cyc),
        .stb_i(stb),
        .we_i (we),
        .ack_o(ack),
        .wat_o(wat),
        .rty_o(rty),
        .err_o(err),
        .adr_i(adr),
        .dat_i(dtx),
        .dat_o(drx),

        //  Memory controller signals (bus-domain):
        .mcb_ce_o  (cmd_enable),
        .mcb_wr_o  (cmd_write),
        .mcb_rdy_i (cmd_ready),
        .mcb_adr_o (cmd_address),
        .mcb_dat_o (cmd_data_in),

        //  Correlator-domain acquisition data & status signals:
        .enable_x_o(vld_x),      // acquired (and oversampled) data
        .strobe_x_o(new_x),      // outputs
        .signal_x_o(daq_x),

        //  Data & miscellaneous signals:
        .request_i (spi_req_b),
        .state_o   (tart_state) // just used for debug & status
        );


   //-------------------------------------------------------------------------
   // Generate fake/pseudorandom offsets and jitter, for the given signals.
   //-------------------------------------------------------------------------
   signal_stagger
     #( .PHASE_JITTER(1),
        .PHASE_OFFSET(2),
        .CYCLE_JITTER(0)
        ) STAGGER0 [MSB:0]
     (  .clk(clk_x),
        .rst(b_rst),
        .ce (ce_e),
        .d  (raw_e),
        .q  (sig_e)
        );


endmodule // tart_capture_tb
