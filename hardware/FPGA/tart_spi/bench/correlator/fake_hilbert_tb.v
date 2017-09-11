`timescale 1ns/100ps
/*
 * Module      : bench/correlator/fake_hilbert_tb.v
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
 * Testbench for a fake Hilbert transform, computed for a supersampled signal
 * approximated using a bit-shift.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

module fake_hilbert_tb;

   parameter MSB = 0;

   wire [MSB:0] re, im;
   reg          clk = 0, rst = 0, en = 0, stb = 0;
   reg [MSB:0]  d;
   wire         valid;

   always #5 clk <= ~clk;


   initial begin : HILB_TB
      $dumpfile ("hilb_tb.vcd");
      $dumpvars;

      #23 rst <= 1; #20 rst <= 0; #20 en <= 1;

      // Test phasing of `en` vs `valid`:
      #200 en <= 0; #40 en <= 1;
      #200 en <= 0; #40 en <= 1;
      
      #200 $finish;
   end


   always @(posedge clk)
     if (!rst && stb)
       d <= #3 $random;
     else
       d <= #3 d;

   always @(posedge clk)
     if (rst)
       stb <= #3 0;
     else
       stb <= #3 ~stb;


   fake_hilbert #( .WIDTH(MSB+1) ) HILBIE[MSB:0]
     (  .clock_i (clk),
        .reset_i (rst),

        .enable_i(en),
        .strobe_i(stb),
        .signal_i(d),

        .locked_o(locked),
        .strobe_o(strobe),
        .framed_o(framed),
        .sig_re_o(re),
        .sig_im_o(im)
        );


endmodule // fake_hilbert_tb
