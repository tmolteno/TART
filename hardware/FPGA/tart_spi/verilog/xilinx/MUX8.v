`timescale 1ns/100ps
/*
 * Module      : verilog/xilinx/MUX8.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * An 8:1 multiplexor (MUX) that is built using explicit instantiation of
 * Xilinx primitives, so that it can be floorplanned easily.
 * 
 * NOTE:
 *  + can be simulated behaviourally, or using suitable modules for the Xilinx
 *    primitives (to ensure that both behaviours match);
 * 
 * TODO:
 *  + initialisation value is wrong?
 * 
 */

//----------------------------------------------------------------------------
//  Set this to simulate the Xilinx primitives.
// `define __SIMULATE_XILINX_PRIMITIVES

`ifdef __icarus
 `ifdef __SIMULATE_XILINX_PRIMITIVES
  `define __DONT_USE_BEHAVIOURAL
 `endif
`else
 `define __DONT_USE_BEHAVIOURAL
`endif

module MUX8
  #(parameter WIDTH = 24,      // bit-width of the MUX'd signal
    parameter MSB   = WIDTH-1, // MSB of the data inputs/outputs
    parameter NOISY = 0,       // display extra debug info?
    parameter DELAY = 3)       // simulated combinational delay (ns)
   (
    input [MSB:0]  a,
    input [MSB:0]  b,
    input [MSB:0]  c,
    input [MSB:0]  d,
    input [MSB:0]  e,
    input [MSB:0]  f,
    input [MSB:0]  g,
    input [MSB:0]  h,
    input [2:0]    s,
    output [MSB:0] x
    );

`ifndef __DONT_USE_BEHAVIOURAL
   reg [MSB:0]     lx, ux;

   assign x = s[2] ? ux : lx;

   always @*
     case (s[1:0])
       0: {ux, lx} = {e, a};
       1: {ux, lx} = {f, b};
       2: {ux, lx} = {g, c};
       3: {ux, lx} = {h, d};
     endcase // case (s[1:0])
`else // !`ifndef __DONT_USE_BEHAVIOURAL

   wire [MSB:0]    lx, ux;

   // FIXME: Initialisation value is wrong?
   LUT6_L
     #( .INIT(64'b1111111100000000111100001111000011001100110011001010101010101010)
        ) LUT0 [MSB:0]
     ( .I0(a),
       .I1(b),
       .I2(c),
       .I3(d),
       .I4(s[0]),
       .I5(s[1]),
       .LO(lx)
       );

   LUT6_L
     #( .INIT(64'b1111111100000000111100001111000011001100110011001010101010101010)
        ) LUT1 [MSB:0]
     ( .I0(e),
       .I1(f),
       .I2(g),
       .I3(h),
       .I4(s[0]),
       .I5(s[1]),
       .LO(ux)
       );

   MUXF7 MUXF7 [MSB:0]
     ( .I0(lx),
       .I1(ux),
       .S(s[2]),
       .O(x)
       );
`endif // !`ifndef __DONT_USE_BEHAVIOURAL


   //-------------------------------------------------------------------------
   //  Simulation-only output.
   //-------------------------------------------------------------------------
   initial begin
      if (NOISY)
        $display("Module : MUX8 (%m, Structural simulation mode)\n\t WIDTH\t= %4d", WIDTH);
   end


endmodule // MUX8
