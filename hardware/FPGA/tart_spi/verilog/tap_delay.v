`timescale 1ns/1ps
module tap_delay
  (
   input  clk_i,
   input  rst_i,
//    output vld_o,
   input  dat_i,
   output reg dat_o = 0
   );

   (* KEEP = "TRUE" *) wire [7:0] a;

   always @(posedge clk_i)
     if (rst_i) dat_o <= 0;
     else       dat_o <= a[7];

   LUT1 #(.INIT(2'b10)) lut0 ( .I0 (dat_i), .O(a[0]) );
   LUT1 #(.INIT(2'b10)) lut1 ( .I0 ( a[0]), .O(a[1]) );
   LUT1 #(.INIT(2'b10)) lut2 ( .I0 ( a[1]), .O(a[2]) );
   LUT1 #(.INIT(2'b10)) lut3 ( .I0 ( a[2]), .O(a[3]) );

   LUT1 #(.INIT(2'b10)) lut4 ( .I0 ( a[3]), .O(a[4]) );
   LUT1 #(.INIT(2'b10)) lut5 ( .I0 ( a[4]), .O(a[5]) );
   LUT1 #(.INIT(2'b10)) lut6 ( .I0 ( a[5]), .O(a[6]) );
   LUT1 #(.INIT(2'b10)) lut7 ( .I0 ( a[6]), .O(a[7]) );
                              
endmodule // tap_delay
