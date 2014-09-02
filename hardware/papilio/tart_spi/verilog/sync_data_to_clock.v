/*
 * This module synchronizes a clock (clk_in) with a serial data stream.
 *
 * by Tim Molteno. tim@elec.ac.nz
 *
 * TODO. Do some averaging of the 'delay' variable to remove sensitivity to noise.
 */
`timescale 1ns/1ns
module sync_antennas_to_clock(
      input fast_clk,
      input clk_in,
      input [23:0] data_in,
      output wire clk_out,
      output wire [23:0] data_out
      );
      sync_data_to_clock ant0 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[0]), .data_out(data_out[0]), .clk_out(clk_out));
      sync_data_to_clock ant1 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[1]), .data_out(data_out[1]));
      sync_data_to_clock ant2 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[2]), .data_out(data_out[2]));
      sync_data_to_clock ant3 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[3]), .data_out(data_out[3]));
      sync_data_to_clock ant4 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[4]), .data_out(data_out[4]));
      sync_data_to_clock ant5 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[5]), .data_out(data_out[5]));
      sync_data_to_clock ant6 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[6]), .data_out(data_out[6]));
      sync_data_to_clock ant7 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[7]), .data_out(data_out[7]));
      sync_data_to_clock ant8 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[8]), .data_out(data_out[8]));
      sync_data_to_clock ant9 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[9]), .data_out(data_out[9]));
      sync_data_to_clock ant10 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[10]), .data_out(data_out[10]));
      sync_data_to_clock ant11 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[11]), .data_out(data_out[11]));
      sync_data_to_clock ant12 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[12]), .data_out(data_out[12]));
      sync_data_to_clock ant13 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[13]), .data_out(data_out[13]));
      sync_data_to_clock ant14 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[14]), .data_out(data_out[14]));
      sync_data_to_clock ant15 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[15]), .data_out(data_out[15]));
      sync_data_to_clock ant16 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[16]), .data_out(data_out[16]));
      sync_data_to_clock ant17 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[17]), .data_out(data_out[17]));
      sync_data_to_clock ant18 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[18]), .data_out(data_out[18]));
      sync_data_to_clock ant19 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[19]), .data_out(data_out[19]));
      sync_data_to_clock ant20 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[20]), .data_out(data_out[20]));
      sync_data_to_clock ant21 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[21]), .data_out(data_out[21]));
      sync_data_to_clock ant22 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[22]), .data_out(data_out[22]));
      sync_data_to_clock ant23 (.fast_clk(fast_clk), .clk_in(clk_in), .data_in(data_in[23]), .data_out(data_out[23]));


endmodule


module sync_data_to_clock(
      input fast_clk,
      input clk_in,
      input data_in,
      output wire clk_out,
      output reg data_out,
      output wire [5:0] avg_delay);

   reg [2:0] clk_in_r;  always @(posedge fast_clk) clk_in_r <= {clk_in_r[1:0], clk_in};
   wire clk_in_risingedge;  assign clk_in_risingedge  = (clk_in_r[2:1]==2'b01);  // now we can detect SCK rising edges
   wire clk_in_fallingedge; assign clk_in_fallingedge = (clk_in_r[2:1]==2'b10);  // and falling edges

   reg clk_out_reg = 0;
   always @(posedge fast_clk)
      begin
         if      (clk_in_risingedge)  clk_out_reg <= 1'b1;
         else if (clk_in_fallingedge) clk_out_reg <= 1'b0;
      end
   assign clk_out = clk_out_reg;
   
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