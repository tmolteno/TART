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

module align_captures_tb;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION SETTINGS.
   //
   //-------------------------------------------------------------------------
   parameter RATIO = 12;
   parameter RBITS = 4;
   parameter RMAX  = RATIO-1;
   parameter HALF  = RATIO>>1;

   parameter SIGS  = 8;
   parameter MSB   = SIGS-1;

   parameter DELAY = 3;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION SIGNALS.
   //
   //-------------------------------------------------------------------------
   wire [MSB:0] daq, sig, cap, strobes, lockeds, invalids;
   wire         ready, locked, invalid;
   reg          clk12x = 1'b1, clk = 1'b1, rst = 1'b0, ce = 1'b0, ack = 1'b0;
   reg [MSB:0]  raw, acks = {SIGS{1'b0}};


   //-------------------------------------------------------------------------
   //
   //  SIMULATION CLOCKS.
   //
   //-------------------------------------------------------------------------
   always #2.50 clk12x <= ~clk12x;
   always #30.0 clk    <= ~clk;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION STIMULI.
   //
   //-------------------------------------------------------------------------
   initial begin : ALIGN_TB
      $dumpfile ("../vcd/align_tb.vcd");
      $dumpvars;
      
      #43 rst = 1'b1; #90 rst = 1'b0;

      #720; #720; #720;
      #720 $finish;
   end


   //-------------------------------------------------------------------------
   //
   //  GENERATE RANDOM DATA TO BE ACQUIRED.
   //
   //-------------------------------------------------------------------------
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
   //
   //  DEVICES UNDER TEST.
   //
   //-------------------------------------------------------------------------

   //-------------------------------------------------------------------------
   //  Align the staggered data.
   //-------------------------------------------------------------------------
   align_captures
     #( .WIDTH(SIGS),
        .RATIO(RATIO),
        .RBITS(RBITS),
        .CLEAR(HALF+1),
        .VALID(HALF-1),
        .DELAY(DELAY)
        ) ALIGNS0
       (
        .clock_i (clk12x),
        .reset_i (rst),
        .enable_i(ce),
        
        .data_in (cap),
        .strobes (strobes),
        .lockeds (lockeds),
        .invalids(invalids),

        .data_out(daq),
        .ready   (ready),
        .locked  (locked),
        .invalid (invalid),
        .ack     (ack)
        );


   //-------------------------------------------------------------------------
   //  Instantiate multiple signal-capture blocks.
   //-------------------------------------------------------------------------
   signal_capture
     #( .RATIO(RATIO),
        .DELAY(DELAY)
        ) SIGCAP0 [MSB:0]
     (  .clk_i    (clk12x),
        .rst_i    (rst),
        .ce_i     (ce),
        .dat_i    (sig),
        .dat_o    (cap),
        .rdy_o    (strobes),
        .locked_o (lockeds),
        .invalid_o(invalids),
        .ack_i    (acks)
        );


   //-------------------------------------------------------------------------
   // Generate fake offsets and jitter, for the given signals.
   //-------------------------------------------------------------------------
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
