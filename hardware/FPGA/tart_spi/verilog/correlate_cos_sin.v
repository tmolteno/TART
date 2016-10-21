`timescale 1ns/100ps
/*
 *
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
   
   wire        c0 = SUMHI && hi ? br == 1'b1 : ar == br;
   wire        c1 = SUMHI && hi ? ar == 1'b1 : ar == bi;

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
