`timescale 1ns/100ps
/*
 * Module      : bench/correlator/control_tb.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
 * 
 * NOTE:
 *  + OBSOLETE (Pat @ 08/11/2016);
 * 
 * TODO:
 * 
 */

module control_tb;

   reg clk_x = 1'b1, b_clk = 1'b1, reset = 1'b0;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION STIMULI.
   //
   //-------------------------------------------------------------------------
   always #2.5 clk_x <= ~clk_x;
   always #5.0 b_clk <= ~b_clk;


   initial begin : SIMULATE
      $dumpfile ("vcd/ctrl_tb.vcd");
      $dumpvars;

      //----------------------------------------------------------------------
      $display("%12t: Issuing reset.", $time);
      #33 reset = 1'b1; #40 reset = 1'b0;

      $display("%12t: Simulation finished.", $time);
   end


   //-------------------------------------------------------------------------
   //
   //  DEVICES UNDER TEST.
   //
   //-------------------------------------------------------------------------
   control
     #( .ACCUM(24),
        .TICKS(4),
        .DUPS (1),
        .SLOW (0),
        ) CTRL
       (
        .clk_x(clk_x),
        .rst  (reset),

        .bank_o(bank),
        .sw_o(clr)
        );


endmodule // control_tb
