`timescale 1ns/1ps
module spi_target_tb;

   wire [7:0] data_from_spi;
   reg        clk = 0, rst = 0, ack = 0;
   wire       cyc, stb, we;
   reg [7:0]  data_to_send;

   wire       oflow, uflow, debug_w;

   reg        SCK = 0, SSEL = 1, MOSI;
   wire       MISO;

   always #5 clk <= ~clk;

   initial begin : SPI_TARGET_TEST
      $dumpfile ("spi_target_tb.vcd");
      $dumpvars;

      #10 rst = 1; #20 rst = 0;

      #80 SSEL = 0;
      #240;

      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;

      #16;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;

      #16;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;
      #4 MOSI = $random; #4 SCK = 1; #8 SCK = 0;

      #400 SSEL = 1;
      #240 $finish;
   end // SPI_TARGET_TEST
   
   always @(posedge clk)
     if (rst) ack <= 0;
     else if (cyc && stb && !we && !ack)
       begin
          ack <= 1;
          data_to_send = $random;
          $display("%8t: Tx: 0x%02x", $time, data_to_send);
       end
     else if (cyc && stb && we && !ack)
       begin
          ack <= 1;
          $display("%8t: Rx: 0x%02x", $time, data_from_spi);
       end
     else
       ack <= 0;


   spi_target SPI_TARGET0
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_o(cyc),
       .stb_o(stb),
       .we_o (we),
       .ack_i(ack),
       .dat_i(data_to_send),
       .dat_o(data_from_spi),

       .overflow_o (oflow),
       .underflow_o(uflow),
       .debug_o    (debug_w),
       
       .SCK (SCK),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );
   
endmodule // spi_target_tb
