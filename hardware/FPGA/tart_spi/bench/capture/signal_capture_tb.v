`timescale 1ns/100ps
/*
 * Module      : bench/capture/signal_capture_tb.v
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

module signal_capture_tb;

   parameter DELAY = 3;
   parameter CLKX  = 2.5;        // half-period of the sample clock
   parameter CLKB  = 5;
   parameter CLKE  = 30;

   parameter DE    = CLKE*2;
   parameter DB    = CLKB*2;
   parameter DX    = CLKX*2;


   //-------------------------------------------------------------------------
   //  Clock & reset signals.
   //-------------------------------------------------------------------------
   reg clk_x = 1'b1, clk_e = 1'b1, b_clk = 1'b1, reset = 1'b0;

   always #CLKE clk_e <= ~clk_e; // external signal clock
   always #CLKB b_clk <= ~b_clk; // bus clock
   always #CLKX clk_x <= ~clk_x; // correlator clock


   //-------------------------------------------------------------------------
   //  Signal jitter & offset signals.
   //-------------------------------------------------------------------------
   reg ce = 1'b0, x_ = 1'b0, x = 1'b0;

   //-------------------------------------------------------------------------
   //  Clock recovery signals;
   reg align = 1'b0, cycle = 1'b0, drift = 1'b0, retry = 1'b0;
   wire d, q, ready, locked, invalid;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION STIMULI.
   //
   //-------------------------------------------------------------------------
   initial begin : SIGNAL_TB
      $dumpfile ("../vcd/sig_tb.vcd");
      $dumpvars;

      $display("%12t: Issuing reset.", $time);
      #DELAY reset = 1; #DE; #DE reset = 0;

      $display("%12t: Beginning clock-recovery.", $time);
      #DE cycle = 1;
//       #DE align = 1;

      #720;
      #720;
      #720;
      #720 $finish;
   end


   //-------------------------------------------------------------------------
   //  Generate signal data.
   //-------------------------------------------------------------------------
   always @(posedge clk_e)
     if (reset)
       ce <= #DELAY 1'b0;
     else begin
        x  <= #DELAY $random;
        ce <= #DELAY 1'b1;
     end

   //-------------------------------------------------------------------------
   //  Not quite sure...
   wire p = x_ ^ d;

   always @(negedge clk_e)
     x_ <= #DELAY x;


   //-------------------------------------------------------------------------
   //  Generate fake jitter and offsets, for the input signal.
   //-------------------------------------------------------------------------
   signal_stagger
     #( .PHASE_JITTER(2),
        .PHASE_OFFSET(2),
        .CYCLE_JITTER(0)
        ) STAG
       (
        .clk(clk_x),
        .rst(reset),
        .ce (ce),
        .d  (x),
        .q  (d)
        );


   //-------------------------------------------------------------------------
   //
   //  DEVICE UNDER TEST (DUT).
   //
   //-------------------------------------------------------------------------
   signal_capture
     #( .RATIO    (12),
        .RBITS    (4),
        .RESET    (1),
        .DRIFT    (1),
        .CYCLE    (1),
        .NOISY    (1),
        .DELAY    (DELAY)
        ) SIGCAP
       (
        .clock_i  (clk_x),
        .reset_i  (reset),
        .align_i  (align),
        .cycle_i  (cycle),
        .drift_i  (drift),
        .ready_o  (ready),
        .locked_o (locked),
        .invalid_o(invalid),
        .retry_i  (retry),

        .signal_i(d),
        .signal_o(q)
        );

   
endmodule // signal_capture_tb
