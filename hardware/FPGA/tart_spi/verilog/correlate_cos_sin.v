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
  #( parameter ACCUM = 32,
     parameter MSB   = ACCUM-1,
     parameter DELAY = 3)
   (
    input          clk,
    input          rst,
    input          en,

    input          ar,
    // input                  ai,
    input          br,
    input          bi,

    input [MSB:0]  dcos,
    input [MSB:0]  dsin,

    output reg     valid = 0,
    output reg     oc = 0, // cosine overflow
    output reg     os = 0, // sine overflow
    output [MSB:0] qcos,
    output [MSB:0] qsin
    );
   
   always @(posedge clk)
     if (rst) valid <= #DELAY 0;
     else     valid <= #DELAY en;

`define __licarus
`ifdef  __licarus
// `ifdef __icarus
   reg [MSB:0]     r_cos = 0;
   reg [MSB:0]     r_sin = 0;

   assign qcos = r_cos;
   assign qsin = r_sin;

   always @(posedge clk)
     if (rst) begin
        {oc, r_cos} <= #DELAY 0;
        {os, r_sin} <= #DELAY 0;
     end
     else if (en) begin
        {oc, r_cos} <= #DELAY ar == br ? dcos + 1 : dcos ;
        {os, r_sin} <= #DELAY ar == bi ? dsin + 1 : dsin ;
     end

`else // !`ifdef __icarus
   wire [MSB:0] w_cos = ar == br ? dcos + 1 : dcos;
   wire [MSB:0] w_sin = ar == bi ? dsin + 1 : dsin;

   FDRE #( .INIT(0)) RCOS [MSB:0]
     ( .D(w_cos), .C(clk), .R(rst), .CE(en), .Q(qcos));

   FDRE #(.INIT(0)) RSIN [MSB:0]
     ( .D(w_sin), .C(clk), .R(rst), .CE(en), .Q(qsin));
`endif // !`ifdef __icarus


endmodule // correlate_cos_sin
