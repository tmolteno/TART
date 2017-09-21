`timescale 1ns/100ps
/*
 * Module      : verilog/tart_wishbone_tb.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
 * 
 * 
 * This file is part of TART.
 * 
 * TART is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * TART is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser Public License along with
 * TART.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * 
 * Description:
 * Testbench for TART's Wishbone bus-arbiter, address-decoder, and signal-
 * router.
 * 
 * NOTE:
 *  + transfers are Wishbone SPEC B4 (by default);
 *  + when a bus only has a single master then the 'CYC' signals are not
 *    typically needed, and set 'CHECK = 0' to disable the 'CYC' logic (as
 *    'CYC' primarily signals to other masters that the bus is currently in
 *    use by another master);
 *  + bus wait-state requests ('WAT_O == 1') can't be pipelined, so these are
 *    upgraded to retry ('RTY_O == 1') responses.
 * 
 * TODO:
 * 
 */

`include "tartcfg.v"

module tart_wishbone_tb;

   parameter DELAY = 3;


   //-------------------------------------------------------------------------
   //  Domain-wide signals.
   reg clock = 1'b1, reset = 1'b0;

   //-------------------------------------------------------------------------
   //  SPI signals.
   wire [7:0] spi_drx;
   reg        spi_cyc = 1'b0, spi_stb = 1'b0, spi_we;
   wire       spi_ack, spi_wat, spi_rty, spi_err;
   reg [6:0]  spi_adr;
   reg [7:0]  spi_dtx;

   //-------------------------------------------------------------------------
   //  Capture signals.
   wire [7:0] cap_dtx;
   wire       cap_cyc, cap_stb, cap_we;
   reg        cap_ack, cap_wat, cap_rty, cap_err;
   wire [1:0] cap_adr;
   reg [7:0]  cap_drx;

   //  Acquisition signals.
   wire [7:0] acq_dtx;
   wire       acq_cyc, acq_stb, acq_we;
   reg        acq_ack, acq_wat, acq_rty, acq_err;
   wire [1:0] acq_adr;
   reg [7:0]  acq_drx;

   //  DSP signals.
   wire [7:0] dsp_dtx;
   wire       dsp_cyc, dsp_stb, dsp_we;
   reg        dsp_ack, dsp_wat, dsp_rty, dsp_err;
   wire [1:0] dsp_adr;
   reg [7:0]  dsp_drx;

   //  System signals.
   wire [7:0] sys_dtx;
   wire       sys_cyc, sys_stb, sys_we;
   reg        sys_ack, sys_wat, sys_rty, sys_err;
   wire [1:0] sys_adr;
   reg [7:0]  sys_drx;



   //-------------------------------------------------------------------------
   //
   //  SIMULATION STIMULUS.
   //
   //-------------------------------------------------------------------------
   always #5 clock <= ~clock;

   initial begin
      $dumpfile("vcd/arb_tb.vcd");
      $dumpvars;

      {cap_err, cap_rty, cap_wat, cap_ack} <= 4'b0000;
      {acq_err, acq_rty, acq_wat, acq_ack} <= 4'b0000;
      {dsp_err, dsp_rty, dsp_wat, dsp_ack} <= 4'b0000;
      {sys_err, sys_rty, sys_wat, sys_ack} <= 4'b0000;

      #13 reset = 1; #20 reset = 0;

      //----------------------------------------------------------------------
      // system unit:
      #20 spi_cyc = 1; spi_stb = 1; spi_we = 0; spi_adr = 7'h63;
      #10 spi_stb = 0; while (!spi_ack) #10;

      #20 spi_cyc = 1; spi_stb = 1; spi_we = 1; spi_adr = 7'h63; spi_dtx = $random;
      #10 spi_stb = 0; while (!spi_ack) #10;

      // DSP unit:
      #20 spi_cyc = 1; spi_stb = 1; spi_we = 0; spi_adr = 7'h41;
      #10 spi_stb = 0; while (!spi_ack) #10;

      #20 spi_cyc = 1; spi_stb = 1; spi_we = 1; spi_adr = 7'h41; spi_dtx = $random;
      #10 spi_stb = 0; while (!spi_ack) #10;

      // capture unit:
      #20 spi_cyc = 1; spi_stb = 1; spi_we = 0; spi_adr = 7'h00;
      #10 spi_stb = 0; while (!spi_ack) #10;

      #20 spi_cyc = 1; spi_stb = 1; spi_we = 1; spi_adr = 7'h00; spi_dtx = $random;
      #10 spi_stb = 0; while (!spi_ack) #10;

      // acquisition unit:
      #20 spi_cyc = 1; spi_stb = 1; spi_we = 0; spi_adr = 7'h22;
      #10 spi_stb = 0; while (!spi_ack) #10;

      #20 spi_cyc = 1; spi_stb = 1; spi_we = 1; spi_adr = 7'h22; spi_dtx = $random;
      #10 spi_stb = 0; while (!spi_ack) #10;

      //----------------------------------------------------------------------
      #80 $finish;
   end


   //-------------------------------------------------------------------------
   //  Auto-timeout.
   //-------------------------------------------------------------------------
   initial begin
      #50000 $display("%12t:\tTIMEOUT.", $time);
      $finish;
   end


   //-------------------------------------------------------------------------
   //  Display received data.
   //-------------------------------------------------------------------------
   always @(posedge clock)
     if (spi_ack && !spi_we) $display("%12t:\tDATA = %02x", $time, spi_drx);

   //-------------------------------------------------------------------------
   //  And also for each of the slaves.
   always @(posedge clock)
     if (cap_stb &&  cap_we) $display("%12t:\t CAP = %02x", $time, cap_dtx);

   always @(posedge clock)
     if (acq_stb &&  acq_we) $display("%12t:\t ACQ = %02x", $time, acq_dtx);

   always @(posedge clock)
     if (dsp_stb &&  dsp_we) $display("%12t:\t DSP = %02x", $time, dsp_dtx);

   always @(posedge clock)
     if (sys_stb &&  sys_we) $display("%12t:\t SYS = %02x", $time, sys_dtx);


   //-------------------------------------------------------------------------
   //  Simulate the master device's response.
   //-------------------------------------------------------------------------
   always @(posedge clock)
     if (spi_ack)
       {spi_cyc, spi_stb} <= #DELAY 2'b00;


   //-------------------------------------------------------------------------
   //  Simulate the slave devices.
   //-------------------------------------------------------------------------
   always @(posedge clock)
     if (cap_cyc && cap_stb) begin
        {cap_err, cap_rty, cap_wat, cap_ack} <= #DELAY 4'b0001;
        if (!cap_we)
          cap_drx <= #DELAY $random;
     end
     else
       cap_ack <= #DELAY 1'b0;

   always @(posedge clock)
     if (acq_cyc && acq_stb) begin
        {acq_err, acq_rty, acq_wat, acq_ack} <= #DELAY 4'b0001;
        if (!acq_we)
          acq_drx <= #DELAY $random;
     end
     else
       acq_ack <= #DELAY 1'b0;

   always @(posedge clock)
     if (dsp_cyc && dsp_stb) begin
        {dsp_err, dsp_rty, dsp_wat, dsp_ack} <= #DELAY 4'b0001;
        if (!dsp_we)
          dsp_drx <= #DELAY $random;
     end
     else
       dsp_ack <= #DELAY 1'b0;

   always @(posedge clock)
     if (sys_cyc && sys_stb) begin
        {sys_err, sys_rty, sys_wat, sys_ack} <= #DELAY 4'b0001;
        if (!sys_we)
          sys_drx <= #DELAY $random;
     end
     else
       sys_ack <= #DELAY 1'b0;



   //-------------------------------------------------------------------------
   //
   //  DEVICE UNDER TEST (DUT).
   //
   //-------------------------------------------------------------------------
   tart_wishbone
     #(  .XBITS(2),
         .ASYNC(1),
         .RESET(1),
         .CHECK(1),
         .PIPED(1),
         .DELAY(DELAY)
         ) ARB0
       ( .bus_clk_i(clock),
         .bus_rst_i(reset),

         //  SPI Wishbone master.
         .spi_cyc_i(spi_cyc),
         .spi_stb_i(spi_stb),
         .spi_we_i (spi_we),
         .spi_ack_o(spi_ack),
         .spi_wat_o(spi_wat),
         .spi_rty_o(spi_rty),
         .spi_err_o(spi_err),
         .spi_adr_i(spi_adr),
         .spi_dat_i(spi_dtx),
         .spi_dat_o(spi_drx),

         //  Capture-unit Wishbone slave.
         .cap_cyc_o(cap_cyc),
         .cap_stb_o(cap_stb),
         .cap_we_o (cap_we),
         .cap_ack_i(cap_ack),
         .cap_wat_i(cap_wat),
         .cap_rty_i(cap_rty),
         .cap_err_i(cap_err),
         .cap_adr_o(cap_adr),
         .cap_dat_i(cap_drx),
         .cap_dat_o(cap_dtx),

         //  Acquisition-unit Wishbone slave.
         .acq_cyc_o(acq_cyc),
         .acq_stb_o(acq_stb),
         .acq_we_o (acq_we),
         .acq_ack_i(acq_ack),
         .acq_wat_i(acq_wat),
         .acq_rty_i(acq_rty),
         .acq_err_i(acq_err),
         .acq_adr_o(acq_adr),
         .acq_dat_i(acq_drx),
         .acq_dat_o(acq_dtx),

         //  DSP-unit Wishbone slave.
         .dsp_cyc_o(dsp_cyc),
         .dsp_stb_o(dsp_stb),
         .dsp_we_o (dsp_we),
         .dsp_ack_i(dsp_ack),
         .dsp_wat_i(dsp_wat),
         .dsp_rty_i(dsp_rty),
         .dsp_err_i(dsp_err),
         .dsp_adr_o(dsp_adr),
         .dsp_dat_i(dsp_drx),
         .dsp_dat_o(dsp_dtx),

         //  System-control-unit Wishbone slave.
         .sys_cyc_o(sys_cyc),
         .sys_stb_o(sys_stb),
         .sys_we_o (sys_we),
         .sys_ack_i(sys_ack),
         .sys_wat_i(sys_wat),
         .sys_rty_i(sys_rty),
         .sys_err_i(sys_err),
         .sys_adr_o(sys_adr),
         .sys_dat_i(sys_drx),
         .sys_dat_o(sys_dtx)
         );


endmodule // tart_wishbone_tb
