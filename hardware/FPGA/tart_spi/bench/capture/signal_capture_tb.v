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
   wire n_clk = ~b_clk;

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
   wire error, valid, start;
   wire [3:0] phase;
   wire [3:0] delta;


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
     #( .PHASE_JITTER(1),
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



`ifdef __DO_NOT_USE_YODUT
   //-------------------------------------------------------------------------
   //
   //  OTHER DEVICE UNDER TEST (ODUT).
   //
   //-------------------------------------------------------------------------
   wire start, valid, error;
   wire [3:0] phase;
   wire [4:0] delta;

   signal_phase
     #( .RATIO  (12),
        .RBITS  (4),
        .RESET  (1),
        .DRIFT  (1),
        .CYCLE  (1),
        .NOISY  (1),
        .DELAY  (DELAY)
        ) PHASE
       (
        .clk_x_i(clk_x),
        .rst_x_i(reset),

        .clk_e_i(clk_e),
        .sig_e_i(d),

        .align_i(align),
        .cycle_i(cycle),
        .drift_i(drift),
        .start_o(start),
        .phase_o(phase),
        .delta_o(delta),
        .valid_o(valid),
        .error_o(error),
        .retry_i(retry)
        );



`else // !`ifdef __DO_NOT_USE_YODUT
   //-------------------------------------------------------------------------
   //
   //  YET OTHER DEVICE UNDER TEST (YODUT).
   //
   //-------------------------------------------------------------------------
   reg        dat_p, dat_n;
   reg        sig_p, sig_n;

   always @(posedge b_clk)
     dat_p <= #DELAY d;

   always @(posedge n_clk)
     dat_n <= #DELAY d;

   always @(posedge b_clk)
     {sig_n, sig_p} <= #DELAY {dat_n, d};


   signal_phase_DDR
     #( .RATIO  (6),
        .RBITS  (3),
        .TICKS  (2),
        .RESET  (1),
        .POLAR  (0),
        .NOISY  (1),
        .DELAY  (DELAY)
        ) PHASE
       (
        .clk_e_i(clk_e),
        .clk_s_i(b_clk),
        .clk_n_i(n_clk),
        .reset_i(reset),

        .enable_i(align),
        .invert_i(1'b0),
        .strobe_o(start),
        .sig_p_i (sig_p),
        .sig_n_i (sig_n),

        .phase_o (phase),
        .delta_o (delta),
        .locked_o(valid),
        .error_o (error),
        .retry_i (retry)
        );
`endif // !`ifdef __DO_NOT_USE_YODUT


   
endmodule // signal_capture_tb
