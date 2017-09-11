`timescale 1ns/100ps
/*
 * Module      : bench/correlator/tart_correlator_tb.v
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
 * Testbench for the TART visibilities read-back functional unit.
 * 
 * NOTE:
 *  + OBSOLETE;
 * 
 * TODO:
 *  + bring up to using the latest version of the logic cores;
 * 
 */

module tart_visibilities_tb;

   parameter BLOCK = 32;        // Number of bits of a block
   parameter MSB   = BLOCK-1;
   parameter MRATE = 12;
   parameter DELAY = 3;
   parameter COUNT = 4; // (1 << 3) - 1;

   wire [MSB:0] dat_c, val_c, blocksize;
   wire [9:0]   adr_c;
   wire [7:0]   dat;
   reg          clk_x = 1, clk_b = 1, rst = 0, en = 0;
   reg          cyc = 0, stb = 0, we = 0, bst = 0;
   reg [11:0]   adr;
   reg [7:0]    val;
   reg          set = 0, get = 0, fin = 0, sw = 0;
   reg [2:0]    dev = 0;
   wire         ack;
   wire         cyc_c, stb_c, we_c, bst_c, ack_c;

   //-------------------------------------------------------------------------
   //  Setup correlator and bus clocks, respectively.
   always #5  clk_x <= ~clk_x;
   always #5  clk_b <= ~clk_b;
//    always #10 clk_b <= ~clk_b;


   //-------------------------------------------------------------------------
   //  Simulate two visibility calculations.
   integer      num = 0;
   integer      ptr = 0;
   initial begin : SIM_BLOCK
      $dumpfile ("vcd/correlator_tb.vcd");
      $dumpvars;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing reset:\n", $time);
      #35 rst <= 1; #40 rst <= 0;

      //----------------------------------------------------------------------
      $display("\n%8t: Setting up the block-size:", $time);
      #40 set <= 1; num <= 1; val <= COUNT; ptr <= 11'h701;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("%8t: Beginning data-correlation (bank 0):", $time);
      #40  en <= 1; strobe <= 1;
      while (!sw) #10;

      //----------------------------------------------------------------------
      $display("%8t: Switching banks (bank 1):", $time);
      while (sw) #10;
      $display("%8t: Reading back visibilities (bank 0):", $time);
      #80 get <= 1; dev <= 3'h0;
      while (!fin) #10;

      //----------------------------------------------------------------------
      while (!sw) #10;
      $display("\n%8t: Stopping data-correlation (bank 1):", $time);
      while (!wrap_cnt) #10;
      #10 en <= 0;

      //----------------------------------------------------------------------
      $display("%8t: Reading back visibilities (bank 1):", $time);
      #80 get <= 1; dev <= 3'h0;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("\n%8t: Reading back counts (bank 1):", $time);
      #80 get <= 1; dev <= 3'h6;
      while (!fin) #10;

      //----------------------------------------------------------------------
      #80 $display("\n%8t: Simulation finished:", $time);
      $finish;
   end

   initial begin : SIM_FAILED
      #12000 $display ("TIMEOUT!");
      $finish;
   end // SIM_FAILED


   //-------------------------------------------------------------------------
   //  Generate fake antenna data.
   reg [23:0] antenna;
   reg [3:0]  cnt = 0;
   reg        strobe = 0;
   wire [3:0] next_cnt = wrap_cnt ? 0 : cnt + 1 ;
   wire       wrap_cnt = cnt == MRATE-1;

   always @(posedge clk_x)
     if (rst) cnt <= #DELAY 0;
     else     cnt <= #DELAY en ? next_cnt : cnt;

   always @(posedge clk_x)
     if (rst) strobe <= #DELAY 0;
     else     strobe <= #DELAY en && wrap_cnt;

   initial antenna <= #DELAY $random;
   always @(posedge clk_x)
     if (!rst && en && wrap_cnt)
       antenna <= #DELAY $random;

   reg        sw_wait = 0;
   always @(posedge clk_b)
     if (rst) {sw, sw_wait} <= #DELAY 0;
     else if (!sw && !sw_wait && available) {sw, sw_wait} <= #DELAY 3;
     else if (sw && sw_wait && available) sw <= #DELAY 0;
     else if (sw_wait && !available) sw_wait <= #DELAY 0;

   //-------------------------------------------------------------------------
   //  Read back visibility data, from the correlators' registers.
   //-------------------------------------------------------------------------
   wire       bst_w = num > 2 && cyc;

   always @(posedge clk)
     if (rst) bst <= #DELAY 0;
     else     bst <= #DELAY bst_w || (set || get) && num > 1;

   always @(posedge clk_b)
     if (rst) bst <= #DELAY 0;
     else     bst <= #DELAY bst_w;

   always @(posedge clk_b)
     if (rst) begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 0;
     end
     else if (set) begin
        $display("%8t: write beginning (num = %1d)", $time, num);
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 7;
     end
     else if (get) begin
        $display("%8t: read beginning (num = %1d)", $time, num);
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 6;
     end
     else if (cyc) begin
        if (!stb && ack) $display("%8t: transfer ending", $time);
        {fin, get, set} <= #DELAY {!stb && ack, get, set};
        {cyc, stb, we } <= #DELAY {stb || !ack, bst, we && (stb || !ack)};
     end
     else begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 0;
     end

   wire [ASB:0] next_adr = bst ? adr + 1 : adr;

   always @(posedge clk)
     if (rst)             adr <= #DELAY 0;
     else if (set || get) adr <= #DELAY ptr;
     else if (cyc)        adr <= #DELAY next_adr;

   always @(posedge clk)
     if (cyc && stb) num <= #DELAY num - 1;

   //-------------------------------------------------------------------------
   // Display the data, and which correlator and register it is from.
   reg [10:0]  adr_r;
   reg [5:0]   ci;
   reg [4:0]   ri;

   always @(posedge clk_b)
     begin
        adr_r <= adr;
        {ci, ri} <= adr_r;
        if (cyc && stb && !we && ack)
          $display("%8t: Vis = %08x (c: %02x, r:%02x)", $time, dat, ci, ri);
     end


   //-------------------------------------------------------------------------
   //  Devices under test (DUT).
   //-------------------------------------------------------------------------
   tart_visibilities
     #(  .BLOCK (BLOCK),
         .COUNT (24),
         .MRATE (MRATE),
         .DELAY (DELAY)
         ) TART_VISIBILITIES0
       ( .clk_i(clk_b),
         .rst_i(rst),

         .cyc_i(cyc),
         .stb_i(stb),
         .we_i (we),
         .bst_i(bst),
         .ack_o(ack),
         .adr_i(adr),
         .byt_i(val),
         .byt_o(dat),

         .cyc_o(cyc_c),
         .stb_o(stb_c),
         .we_o (we_c ),
         .bst_o(bst_c),
         .ack_i(ack_c),
         .adr_o(adr_c),
         .dat_i(val_c),
         .dat_o(dat_c),

         .switching(switching),
         .blocksize(blocksize),
         .available(available)
         );

   wb_sram #( .WIDTH(32), .SBITS(10) ) SRAM0
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_i(cyc),
       .stb_i(stb),
       .we_i (we),
       .bst_i(bst),
       .ack_o(ack),
       .adr_i(adr),
       .dat_i(val),
       .dat_o(dat)
       );


endmodule // tart_visibilities_tb
