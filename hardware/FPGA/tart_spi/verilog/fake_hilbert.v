`timescale 1ns/100ps
/*
 *
 * Approximate Hilbert transform, for a 1-bit signal.
 * 
 * NOTE:
 *  + for 4x oversampled signals, the 90 degree phase difference between
 *    samples is approximately equal to the complex component of the previous
 *    sample, and is very cheap to compute;
 * 
 * TODO:
 * 
 */

module fake_hilbert
  #( parameter WIDTH = 1,
     parameter MSB   = WIDTH-1,
     parameter TICKS = 12,
     parameter TBITS = 4,
     parameter TSB   = TBITS-1,
     parameter DELAY = 3)
   (
    input              clk,
    input              rst,
    input              en,
    input [MSB:0]      d,
    output reg         valid = 0,
    output reg         strobe = 0,

    (* KEEP = "TRUE", IOB = "FALSE" *)
    output reg [MSB:0] re = 0,
    (* KEEP = "TRUE", IOB = "FALSE" *)
    output reg [MSB:0] im = 0
    );

   reg                 go = 0;
   reg [TSB:0]         ticks = 0;
   wire                ticks_wrap = ticks == TICKS-1;
   wire [TSB:0]        ticks_next = ticks_wrap ? 0 : ticks+1;

   always @(posedge clk)
     if (rst)                  ticks <= #DELAY 0;
     else if (en || ticks > 0) ticks <= #DELAY ticks_next;

   always @(posedge clk)
     if (rst) go <= #DELAY 1'b0;
     else     go <= #DELAY ticks_wrap ? en : 0;

   always @(posedge clk)
     if (rst) valid <= #DELAY 1'b0;
     else     valid <= #DELAY ticks == 0 ? go : valid;

   always @(posedge clk)
     if (rst) strobe <= #DELAY 1'b0;
     else     strobe <= #DELAY go;

   always @(posedge clk)
     if (en && ticks == 0) {im, re} <= #DELAY {re, d};
     else                  {im, re} <= #DELAY {im, re};

   /*
   always @(posedge clk)
     if (go) {im, re} <= #DELAY {re, d};
     else    {im, re} <= #DELAY {im, re};

   always @(posedge clk)
     if (rst) {valid, go} <= #DELAY 0;
     else     {valid, go} <= #DELAY {go & en, en | go};

   always @(posedge clk)
     if (rst)     re <= #DELAY 0;
     else if (en) re <= #DELAY d;
     else         re <= #DELAY re;

   always @(posedge clk)
     if (rst)     im <= #DELAY 0;
     else if (go) im <= #DELAY re;
     else         im <= #DELAY im;
    */

endmodule // fake_hilbert
