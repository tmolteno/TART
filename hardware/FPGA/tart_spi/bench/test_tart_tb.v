`timescale 1ns/1ps

module test_tart_tb();
   reg rst=0, clk32=0, sck=0, mosi, ssel=1;
   always #15.625 clk32 = !clk32; // 32 MHz Papilio
   reg [23:0] telescope_data = 0;
   reg  telescope_clk = 0; always #30 telescope_clk = !telescope_clk; // ~16 MHz tart clock
   wire led;

   tart tartwithspi(
                     .fpga_clk_32(clk32), .rst(rst), .led(led),                        // Papilio    32 MHz onboard clock
                     .rx_clk_16(telescope_clk), .antenna(telescope_data),              // TELESCOPE  16.368 MHz receiver master clock
                     .spi_sck(sck), .spi_mosi(mosi), .spi_miso(miso), .spi_ssel(ssel)  // SPI
                     );

   task halfsclk;
      begin
         #32.051;  	// T/2 =   32.05 ns || 15.6 MHz
         //#64.10;	// T/2 =   64.10 ns ||  7.8 MHz
      end
   endtask

   task fullsclk; begin halfsclk(); halfsclk(); end endtask

   task ssel_enable; begin ssel = 0; #200; end endtask
   task ssel_disable; begin #200; ssel = 1; #200; end endtask

   reg [7:0] slaveout = 8'bx;
   task spi_write;
      input[7:0] x;
      integer j;
      begin
         //$display("Begin SPI write=%x %b", x, x);
         mosi = 0;
         sck = 0;
         slaveout = 8'bx;
         // send data MSB first on the RISING edge of sck
         for (j = 7 ;  j >= 0 ;  j = j - 1)
            begin
               mosi = x[j];
               halfsclk; sck = !sck; // rising edge
               slaveout[j] = miso;
               halfsclk; sck = !sck; // falling edge
            end
      //$display("MOSI %b %h", x, x);
      $display("MISO(RPI) %t %b %h", $time, slaveout, slaveout);
      end
   endtask

   integer f;

   initial begin
   rst = 1; #3000; rst = 0; #3000;

   ssel_enable; spi_write(8'b0000_0000); fullsclk; spi_write(8'b0000_0000); ssel_disable; // READ STATUS
   ssel_enable; spi_write(8'b1000_0001); fullsclk; spi_write(8'b0000_0001); ssel_disable; // START AQUSITION
   #1_200_000 // wait for a while 60ns *20k = 1200k

// for (f=0; f < 20; f = f + 1)
//    begin
//         ssel_enable;
//         $display("3BYTE READ %t", $time);
//         spi_write(8'b0000_0010); fullsclk;
//         spi_write(8'b0000_0000); fullsclk; //MSB
//         spi_write(8'b0000_0000); fullsclk;
//         spi_write(8'b0000_0000); ssel_disable; //LSB
//         #200;
//    end

   for (f=0; f < 250; f = f + 1)
      begin
//         $display("MSB READ %t ps", $time);
//         ssel_enable; spi_write(8'b0_00000_10); fullsclk; spi_write(8'b0); ssel_disable; #200;//MSB
//         $display("  B READ %t", $time);
//         ssel_enable; spi_write(8'b0_00000_11); fullsclk; spi_write(8'b0); ssel_disable; #200;//  B
//         $display("LSB READ %t", $time);
         ssel_enable; spi_write(8'b0_00001_00); fullsclk; spi_write(8'b0); ssel_disable; #200;//LSB
      end

   #120 $finish();
   end
endmodule
