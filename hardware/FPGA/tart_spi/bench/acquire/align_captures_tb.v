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

module align_captures_tb;

   parameter RATE = 12;
   parameter HALF = RATE >> 1;
   parameter BITS = 4;
   parameter SIGS = 8;
   parameter MSB  = SIGS-1;

   wire [MSB:0] daq, sig, cap, strobes, lockeds, invalids;
   wire         ready, locked, invalid;
   reg          clk12x = 1'b1, clk = 1'b1, rst = 1'b0, ce = 1'b0, ack = 1'b0;
   reg [MSB:0]  raw, acks = 0;

   always #2.50 clk12x <= ~clk12x;
   always #30.0 clk <= ~clk;


   initial begin : SIGNAL_TB
      $dumpfile ("align_tb.vcd");
      $dumpvars;
      
      #43 rst = 1'b1; #90 rst = 1'b0;

      #720; #720; #720;
      #720 $finish;
   end


   //-------------------------------------------------------------------------
   // Generate random data to be acquired.
   always @(posedge clk)
     raw <= #DELAY $random;

   always @(posedge clk12x)
     if (ready) $display("%8t: DATA = %08b (%02x)", $time, daq, daq);

   // Start acquisition after a reset.
   always @(posedge clk)
     ce <= #DELAY !rst ? 1 : 0 ;

   // Acknowledge that invalid data was received.
   always @(posedge clk)
     ack <= #DELAY !ack && !rst && ce && |{invalid, invalids};


   //-------------------------------------------------------------------------
   //  Align the staggered data.
   align_captures
     #( .NUM_SIGNALS(SIGS),
        .CLOCK_RATE (RATE),
        .CLEAR_GAP  (HALF + 1),
        .STROBE_GAP (HALF - 1),
        .BITS_COUNT (BITS)
        ) ALIGNS0
       (
        .clk(clk12x),
        .rst(rst),
        .ce(ce),
        
        .data_in(cap),
        .strobes(strobes),
        .lockeds(lockeds),
        .invalids(invalids),

        .data_out(daq),
        .ready(ready),
        .locked(locked),
        .invalid(invalid),
        .ack(ack)
        );

   signal_capture SIGCAP0 [MSB:0]
     ( .clk_i    (clk12x),
       .rst_i    (rst),
       .ce_i     (ce),
       .dat_i    (sig),
       .dat_o    (cap),
       .ready_o  (strobes),
       .locked_o (lockeds),
       .invalid_o(invalids),
       .ack_i    (acks)
       );

   // Offset and jitter the given signals.
   signal_stagger
     #( .PHASE_JITTER(1),
        .PHASE_OFFSET(2),
        .CYCLE_JITTER(0)
        ) STAGGER0 [MSB:0]
     (  .clk(clk12x),
        .rst(rst),
        .ce (ce),
        .d  (raw),
        .q  (sig)
        );


endmodule // align_captures_tb
