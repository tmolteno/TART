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
     parameter DELAY = 3)
   (
    input                  clk,
    input                  rst,
    input                  en,

    input                  ar,
    // input                  ai,
    input                  br,
    input                  bi,

    input [ACCUM-1:0]      dcos,
    input [ACCUM-1:0]      dsin,

    output reg             valid = 0,
    output reg             oc = 0, // cosine overflow
    output reg             os = 0, // sine overflow
    output reg [ACCUM-1:0] qcos = 0,
    output reg [ACCUM-1:0] qsin = 0
    );

   always @(posedge clk)
     if (rst) valid <= #DELAY 0;
     else     valid <= #DELAY en;

   always @(posedge clk)
     if (rst) begin
        {oc, qcos} <= #DELAY 0;
        {os, qsin} <= #DELAY 0;
     end
     else if (en) begin
        {oc, qcos} <= #DELAY ar == br ? dcos + 1 : dcos ;
        {os, qsin} <= #DELAY ar == bi ? dsin + 1 : dsin ;
     end

endmodule // correlate_cos_sin
