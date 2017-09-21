`timescale 1ns/100ps
/*
 * Module      : verilog/tart_wishbone.v
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
 * Performs a "complex correlation step."
 * 
 * NOTE:
 *  + not a normal correlation, but computes real sine and cosine components;
 * 
 * TODO:
 * 
 */

module correlate_cos_sin
  #( parameter ACCUM = 24,
     parameter MSB   = ACCUM-1,
     parameter SUMHI = 0,       // is "ones-counting" needed?
     parameter DELAY = 3)
   (
    input          clk,
    input          en,

    input          hi,
    input          ar,
    input          br,
    input          bi,

    input [MSB:0]  dcos,
    input [MSB:0]  dsin,

    output reg     valid = 0,
    output [MSB:0] qcos,
    output [MSB:0] qsin
    );


`ifdef __USE_SWAPPED_MEANS
   wire        c0 = SUMHI && hi ? br == 1'b1 : ar == br;
   wire        c1 = SUMHI && hi ? ar == 1'b1 : ar == bi;
`else
   wire        c0 = SUMHI && hi ? ar == 1'b1 : ar == br;
   wire        c1 = SUMHI && hi ? br == 1'b1 : ar == bi;
`endif

   always @(posedge clk)
     valid <= #DELAY en;

`define __licarus
`ifdef  __licarus
// `ifdef __icarus
   reg [MSB:0]     r_cos = 0;
   reg [MSB:0]     r_sin = 0;

   wire [ACCUM:0]  w_cos = dcos + c0;
   wire [ACCUM:0]  w_sin = dsin + c1;

   assign qcos = r_cos;
   assign qsin = r_sin;

   always @(posedge clk)
     if (en) begin
        r_cos <= #DELAY w_cos[MSB:0];
        r_sin <= #DELAY w_sin[MSB:0];
//         r_sin <= #DELAY {ACCUM{1'b1}};
     end

`else // !`ifdef __icarus
   wire [MSB:0] w_cos = c0 ? dcos + 1 : dcos;
   wire [MSB:0] w_sin = c1 ? dsin + 1 : dsin;

   FDRE #( .INIT(0)) RCOS [MSB:0]
     ( .D(w_cos), .C(clk), .R(1'b0), .CE(en), .Q(qcos));

   FDRE #(.INIT(0)) RSIN [MSB:0]
     ( .D(w_sin), .C(clk), .R(1'b0), .CE(en), .Q(qsin));
`endif // !`ifdef __icarus


endmodule // correlate_cos_sin
