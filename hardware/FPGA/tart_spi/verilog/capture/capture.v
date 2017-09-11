`timescale 1ns/100ps
/*
 * Module      : verilog/capture/capture.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
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
 * Captures and aligns the input signals. The resulting signals are all over-
 * sampled by `RATIO`, and they all transition (if they transition) on the
 * same clock-edge (of the sampling clock).
 * 
 * OBSOLETE:
 *  + replaced by 'tart_capture.v';
 *  + this version uses too many resources, as it uses duplicated alignment
 *    logic is for each channel;
 *    
 * NOTE:
 * 
 * TODO:
 * 
 */

module capture
  #(//  Data bit-width parameters:
    parameter WIDTH = 24,
    parameter MSB   = WIDTH-1,

    //  Data-alignment settings:
    parameter RATIO = 12,       // oversampling ratio?
    parameter RBITS = 4,        // bit-width of clock-counter
    parameter RSB   = RBITS-1,
    parameter HALF  = RATIO>>1,
    parameter CLEAR = HALF+1,
    parameter VALID = HALF-1,

    //  Data-alignment options:
    parameter DELAY = 3)
   (
    input          clock_i,     // oversampling (by 'RATIO') clock
    input          reset_i,     // clears all stored timing info
    input          align_i,     // align the inputs while asserted

    output         ready_o,     // strobes for each new output-value
    output         valid_o,     // valid data is being emitted
    output         error_o,     // lost tracking of the signal
    input          clear_i,     // clear the error flag

    input [MSB:0]  acks_i,      // TODO
    input [MSB:0]  data_i,
    output [MSB:0] data_o
    );

   wire [MSB:0] data_w, strobes, lockeds, invalids;


   //-------------------------------------------------------------------------
   //  Align the (potentially) staggered data.
   //-------------------------------------------------------------------------
   align_captures
     #( .WIDTH(WIDTH),
        .RATIO(RATIO),
        .RBITS(RBITS),
        .CLEAR(HALF+1),
        .VALID(HALF-1),
        .DELAY(DELAY)
        ) ALIGNS0
       (
        .clock_i (clock_i),
        .reset_i (reset_i),
        .align_i (align_i),
        
        .data_in (data_w),
        .strobes (strobes),
        .lockeds (lockeds),
        .invalids(invalids),

        .data_out(data_o ),
        .ready   (ready_o),
        .locked  (valid_o),
        .invalid (error_o),
        .ack     (clear_i)
        );


   //-------------------------------------------------------------------------
   //  Instantiate multiple signal-capture blocks.
   //-------------------------------------------------------------------------
   signal_capture
     #( .RATIO(RATIO),
        .DELAY(DELAY)
        ) SIGCAP0 [MSB:0]
     (  .clock_i  (clock_i),
        .reset_i  (reset_i),
        .align_i  (align_i),
        .signal_i (data_i),
        .signal_o (data_w),
        .ready_o  (strobes),
        .phase_o  (),
        .locked_o (lockeds),
        .invalid_o(invalids),
        .retry_i  (acks_i)
        );


endmodule // capture
