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

module signal_capture_tb;


   reg clk12x = 1'b1, clk = 1'b1, rst = 1'b0;
   reg ce = 1'b0, r = 1'b0, x = 1'b0, ack = 1'b0;
   wire d, q, ready, locked, invalid;

   wire p = r ^ d;

   always #2.5 clk12x <= ~clk12x;
   always #30 clk <= ~clk;


   initial begin : SIGNAL_TB
      $dumpfile ("../vcd/signal_tb.vcd");
      $dumpvars;
      
      #45 rst = 1; #90 rst = 0;

      #720;
      #720;
      #720;
      #720 $finish;
   end


   always @(posedge clk)
     x <= $random;

   always @(negedge clk)
     r <= x;

   always @(posedge clk)
     ce <= !rst ? 1 : 0 ;


   signal_capture SIGCAP0
     ( .clk_i(clk12x),
       .rst_i(rst),
       .ce_i(ce),
       .rdy_o(ready),
       .locked_o(locked),
       .invalid_o(invalid),
       .ack_i(ack),
       .dat_i(d),
       .dat_o(q)
       );

   signal_stagger
     #( .PHASE_JITTER(2),
        .PHASE_OFFSET(2),
        .CYCLE_JITTER(0)
        ) STAGGER0
       (
        .clk(clk12x),
        .rst(rst),
        .ce (ce),
        .d  (x),
        .q  (d)
        );

   
endmodule // signal_capture_tb
