`timescale 1ns/100ps
/*
 * Module      : verilog/correlator/fake_hilbert.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
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
 * Approximate Hilbert transform, for a 1-bit signal.
 * 
 * NOTE:
 *  + this version is designed to work with the DDR (6x oversampled) captured
 *    source-signals;
 *  + for 4x oversampled signals, the 90 degree phase difference between
 *    samples is approximately equal to the complex component of the previous
 *    sample, and is very cheap to compute;
 * 
 * TODO:
 * 
 */

module fake_hilbert
  #( parameter WIDTH = 24,
     parameter MSB   = WIDTH-1,
     parameter TICKS = 12,
     parameter TBITS = 4,
     parameter TSB   = TBITS-1,
     parameter DELAY = 3)
   (
    //  Oversampling-domain clock & reset input signals.
    input          clock_i,
    input          reset_i,

    //  Oversampled input source and framing signals.
    input          enable_i,
    input          strobe_i,
    input [MSB:0]  signal_i,

    //  Oversampled output stream and framing signals.
    output         locked_o,
    output         strobe_o,
    output         framed_o,

    output [MSB:0] sig_re_o,
    output [MSB:0] sig_im_o
    );


   reg [MSB:0]     sig_imag = {WIDTH{1'b0}};
   reg             enable = 1'b0, starts = 1'b0;


   //-------------------------------------------------------------------------
   //  Output assignments.
   //-------------------------------------------------------------------------
   assign locked_o = enable;
   assign strobe_o = strobe_i;
   assign framed_o = starts;

   assign sig_re_o = signal_i;        // Real & imaginary signal-component outputs
   assign sig_im_o = sig_imag;


   //-------------------------------------------------------------------------
   //  Device-enable lags by one frame.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i)
       enable <= #DELAY 1'b0;
     else if (strobe_i)
       enable <= #DELAY enable_i;
     else
       enable <= #DELAY enable;

   //-------------------------------------------------------------------------
   //  Sample is starts after this signal strobes.
   always @(posedge clock_i)
     if (enable_i)
       starts <= #DELAY strobe_i;

   //-------------------------------------------------------------------------
   //  Sample is starts after this signal strobes.
   always @(posedge clock_i)
     if (enable_i && strobe_i)
       sig_imag <= #DELAY signal_i;


endmodule // fake_hilbert
