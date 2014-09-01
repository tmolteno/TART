/*
 * This module synchronizes a clock (clk_in) with a serial data stream.
 *
 * by Tim Molteno. tim@elec.ac.nz
 *
 * TODO. Do some averaging of the 'delay' variable to remove sensitivity to noise.
 */
`timescale 1ns/1ns

module sync_data_to_clock(
      input fast_clk,
      input clk_in,
      input data_in,

      output reg clk_out,
      output reg data_out,
      output wire [5:0] avg_delay);

   reg [2:0] clk_in_r;  always @(posedge fast_clk) clk_in_r <= {clk_in_r[1:0], clk_in};
   wire clk_in_risingedge;  assign clk_in_risingedge  = (clk_in_r[2:1]==2'b01);  // now we can detect SCK rising edges
   wire clk_in_fallingedge; assign clk_in_fallingedge = (clk_in_r[2:1]==2'b10);  // and falling edges

   always @(posedge fast_clk)
      begin
         if      (clk_in_risingedge)  clk_out <= 1'b1;
         else if (clk_in_fallingedge) clk_out <= 1'b0;
      end

   reg [2:0] data_in_r;  always @(posedge fast_clk) data_in_r <= {data_in_r[1:0], data_in};
   assign data_in_risingedge = (data_in_r[2:1]==2'b01);  // now we can detect SCK rising edges

   reg [5:0] counter = 6'b0;
   reg [2:0] delay_ptr = 3'b0;
   reg [5:0] delay_reg [7:0];

   integer j;
   initial  begin for (j = 0; j<8; j=j+1) delay_reg[j] = 6'b0; end

   //wire [5:0] avg_delay;  // avg number of fast clock cycles between rising edge of data and rising edge of data.
   //assign avg_delay = (delay_reg[0] + delay_reg[1] + delay_reg[2] + delay_reg[3] + delay_reg[4] + delay_reg[5] + delay_reg[6] + delay_reg[7]) >> 3;
   assign avg_delay = delay_reg[0];

   reg [9:0] clk_period;
   reg [9:0] clk_period_cnt = 0;

   always @(posedge fast_clk)
   begin
      clk_period_cnt <= (clk_in_risingedge) ? 10'b0: clk_period_cnt + 1'b1;
      if (clk_in_risingedge) clk_period <= clk_period_cnt;
   end

   reg start = 0;
   always @(posedge fast_clk)
      begin
         if (data_in_risingedge)
            begin
               counter <= 6'b0;
               start <= 1;
            end
         else if (start && clk_in_risingedge)
            begin
               start <= 0;
               delay_ptr <= delay_ptr + 1'b1 ;
               delay_reg[delay_ptr] <= counter;
               $display("%d, %d, %d, %d, avg: %d", delay_reg[0], delay_reg[1], delay_reg[2], delay_reg[3], avg_delay);
            end
         else counter <= counter + 1'b1;
      end

   reg [9:0] out_counter_rise = 9'b0;
   reg buffer;

   reg sp;

   always @(posedge fast_clk)
      begin
         out_counter_rise <= (clk_in_risingedge) ? 9'b0: out_counter_rise + 1'b1;
         sp <= (out_counter_rise == (clk_period/2-avg_delay)) ? 1'b1 : 1'b0;
         if (out_counter_rise == (clk_period/2-avg_delay)) buffer <= data_in;
         if (clk_in_fallingedge) data_out <= buffer;
      end
endmodule


module sync_data_to_clock_tb();
   wire data_out;
   wire clk_out;
   wire [5:0] avg_delay;

   reg clk_in = 0;
   reg fast_clk = 0;
   reg data_in = 0;

   always #1.125 fast_clk = !fast_clk;
   always #30 clk_in = !clk_in;

   reg [9:0] i;

   initial
      begin
         $dumpfile("dll.lx2");
         $dumpvars(0,sync_data_to_clock_tb);
         $monitor("At time %t, data_out = %h, clk_in = %h", $time, data_out, clk_in);
         #25 data_in = 0;
         for (i = 1; i<100; i=i+1)
         #60 data_in = !data_in;
         # 200 $finish;
      end
   sync_data_to_clock dut (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in), .clk_out(clk_out), .data_out(data_out), .avg_delay(avg_delay));

endmodule