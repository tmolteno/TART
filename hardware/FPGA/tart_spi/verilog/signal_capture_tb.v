`timescale 1ns/1ps
module signal_capture_tb;


   reg clk12x = 1, clk = 1, rst = 0;
   reg ce = 0, d = 0;
   wire q, ready, locked, invalid;

   always #2.5 clk12x <= ~clk12x;
   always #30 clk <= ~clk;


   initial begin : SIGNAL_TB
      $dumpfile ("signal_tb.vcd");
      $dumpvars;
      
      #40 rst = 1; #60 rst = 0;

      #720;
      #720 $finish;
   end


   always @(posedge clk)
     d <= $random;

   always @(posedge clk)
     ce <= !rst;


   signal_capture SIGCAP0
     ( .clk(clk12x),
       .rst(rst),
       .ce(1'b1 && !rst),
       .d(d),
       .q(q),
       .ready(ready),
       .locked(locked),
       .invalid(invalid)
       );

   
endmodule // signal_capture_tb
