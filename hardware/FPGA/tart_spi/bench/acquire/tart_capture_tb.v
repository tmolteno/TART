`timescale 1ns/100ps
/*
 * Module      : bench/signal_capture_tb.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
 * 
 * Testbench for testing TART's signal-capturing circuitry.
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
   reg          ce_e = 1'b0;

   wire [ASB:0] cmd_address;    // memory-controller signals (unused)
   wire         cmd_enable, cmd_write;
   reg          cmd_ready = 1'b0;
   wire [31:0]  cmd_data_in;

   reg          clk_x = 1'b1, clk_e = 1'b1; // signal & sample clocks
   reg          b_clk = 1'b1, b_rst = 1'b0; // bus clock & reset
   wire         clk_d;

   reg          aq_enabled, vx_enabled, spi_req_b;
   wire         vld_x, stb_x;
   wire         aq_valid, aq_error;
   reg [2:0]    aq_delay;
   reg          aq_align, aq_clear, aq_retry, aq_debug, aq_shift, aq_count;

   wire [2:0]   tart_state;


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
      #DB aq_enabled = 0; vx_enabled = 0; aq_debug = 0;
      #DB aq_align = 0; aq_delay = 0; aq_clear = 0; aq_retry = 0;
      #DB spi_req_b = 0;
      #DE b_rst = 1'b0;

      #720; #720; #720;
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

   always @(posedge clk_e)
     if (ALIGN && aq_enabled)
       aq_align <= #DELAY 1'b1;

   //  Display aligned data.
   always @(posedge clk_x)
     if (stb_x) $display("%8t: DATA = %08b (%02x)", $time, daq_x, daq_x);

   always @(posedge b_clk)
     if (ce_e) aq_enabled <= #DELAY 1'b1;


   //-------------------------------------------------------------------------
   //
   //  DEVICES UNDER TEST.
   //
   //-------------------------------------------------------------------------

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
     ( .clk_i     (b_clk),
       .clk_x     (clk_x),
       .clk_e     (clk_e),
       .clk_d     (clk_d),
       .rst_i     (b_rst),

       //  External antenna data:
       .ax_dat_e_i(sig_e),

       //  Memory controller signals (bus-domain):
       .mcb_ce_o  (cmd_enable),
       .mcb_wr_o  (cmd_write),
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
       .ax_vld_x_o(vld_x),      // acquired (and oversampled) data
       .ax_new_x_o(stb_x),      // outputs
       .ax_dat_x_o(daq_x),

       //  Data & miscellaneous signals:
       .rd_req_i  (spi_req_b),
       .tart_state(tart_state)
       );


   //-------------------------------------------------------------------------
   // Generate fake offsets and jitter, for the given signals.
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
