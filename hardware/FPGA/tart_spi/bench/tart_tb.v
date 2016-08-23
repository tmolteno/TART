`timescale 1ns/1ps

//  _____      _      ____    _____
// |_   _|    / \    |  _ \  |_   _|
//   | |     / _ \   | |_) |   | |
//   | |    / ___ \  |  _ <    | |
//   |_|   /_/   \_\ |_| \_\   |_|
//

`include "tartcfg.v"

module tart_tb;

   parameter DELAY = 3;

   reg [23:0] ax;
   reg        clk_e = 1;
   reg        sck   = 1;
   reg        ssel  = 0;
   wire       sck_w, mosi, miso;

   assign sck_w = ssel ? sck : 1'b0;


   //-------------------------------------------------------------------------
   //  External, system clocks.
   always #32 clk_e <= ~clk_e;
   always #5  sck   <= ~sck;


   //-------------------------------------------------------------------------
   //  Fake, pseduorandom antenna data.
   always @(posedge clk_e)
     ax <= #DELAY $random;


   //-------------------------------------------------------------------------
   //
   //  TART hardware device under test.
   //
   //-------------------------------------------------------------------------
   tart TART0
     (.SPI_SCK (sck_w),
      .SPI_SSEL(ssel),
      .SPI_MOSI(mosi),
      .SPI_MISO(miso),

      .rx_clk_16(clk_e),
      .antenna(ax),
      .led(led)
      );


endmodule // tart_tb
