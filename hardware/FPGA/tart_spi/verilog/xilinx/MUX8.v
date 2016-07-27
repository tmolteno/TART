`timescale 1ns/100ps
module MUX8
  #( parameter WIDTH = 24,
     parameter MSB   = WIDTH-1,
     parameter DELAY = 3)
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

`ifdef __icarus
   reg [MSB:0]     lx, ux;

   assign x = s[2] ? ux : lx;

   always @*
     case (s[1:0])
       0: {ux, lx} = {e, a};
       1: {ux, lx} = {f, b};
       2: {ux, lx} = {g, c};
       3: {ux, lx} = {h, d};
     endcase // case (s[1:0])

`else // !`ifdef __icarus
   wire [MSB:0]    lx, ux;

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
`endif // !`ifdef __icarus

endmodule // MUX8
