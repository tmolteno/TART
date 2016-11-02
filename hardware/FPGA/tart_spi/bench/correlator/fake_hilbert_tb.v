`timescale 1ns/100ps
module fake_hilbert_tb;

   parameter MSB = 0;

   wire [MSB:0] re, im;
   reg          clk = 0, rst = 0, en = 0;
   reg [MSB:0]  d;
   wire         valid;

   always #5 clk <= ~clk;


   initial begin : HILB_TB
      $dumpfile ("hilb_tb.vcd");
      $dumpvars;

      #20 rst <= 1; #20 rst <= 0; #20 en <= 1;

      // Test phasing of `en` vs `valid`:
      #200 en <= 0; #40 en <= 1;
      #200 en <= 0; #40 en <= 1;
      
      #200 $finish;
   end

   always @(posedge clk)
     if (!rst) d <= #3 $random;


   fake_hilbert #( .WIDTH(MSB+1) ) HILB0[MSB:0]
     (  .clk(clk),
        .rst(rst),
        .en(en),
        .d(d),
        .valid(valid),
        .strobe(strobe),
        .frame(valid),
        .re(re),
        .im(im)
        );

endmodule // fake_hilbert_tb
