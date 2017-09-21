`timescale 1ns/100ps
/*
 * Module      : bench/correlator/correlator_block_tb.v
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
 * Testbench for a block of hardware correlators.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

module correlator_block_tb;

   parameter ACCUM = 32;
   parameter DELAY = 3;
   parameter BLOCK = 3;         // Number of bits of a block
   parameter COUNT = 1 << BLOCK;

   wire [31:0] dat;
   reg         clk_x = 1, clk_b = 1, rst = 0, sw = 0, en = 0;
   reg         cyc = 0, stb = 0;
   reg [6:0]   adr;
   reg         get = 0, fin = 0;
   wire        ack, oc, os;
   wire        we = 0;

   //-------------------------------------------------------------------------
   //  Setup correlator and bus clocks, respectively.
   always #5  clk_x <= ~clk_x;
   always #5  clk_b <= ~clk_b;
//    always #10 clk_b <= ~clk_b;


   //-------------------------------------------------------------------------
   //  Simulate two visibility calculations.
   initial begin : SIM_BLOCK
      $dumpfile ("correlator_tb.vcd");
      $dumpvars;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing reset:", $time);
      #35 rst <= 1; #40 rst <= 0;

      //----------------------------------------------------------------------
      $display("\n%8t: Beginning data-correlation (bank 0):", $time);
      #40  en <= 1;
      while (!sw) #10;

      //----------------------------------------------------------------------
      $display("%8t: Switching banks (bank 1):", $time);
      while (sw) #10;
      $display("%8t: Reading back visibilities (bank 0):", $time);
      #80 get <= 1;
      while (!fin) #10;

      //----------------------------------------------------------------------
      while (!sw) #10;
      $display("\n%8t: Stopping data-correlation (bank 1):", $time);
      while (!wrap_cnt) #10;
      #10 en <= 0;

      //----------------------------------------------------------------------
      $display("%8t: Reading back visibilities (bank 1):", $time);
      #80 get <= 1;
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
   reg [11:0] re, im;
   reg [3:0]  cnt = 0;
   wire [3:0] next_cnt = wrap_cnt ? 0 : cnt + 1 ;
   wire       wrap_cnt = cnt == 11;

   always @(posedge clk_x)
     if (rst) cnt <= #DELAY 0;
     else     cnt <= #DELAY en ? next_cnt : cnt;

   // TODO: Use `fake_hilbert`?
   initial {re, im} <= #DELAY $random;
   always @(posedge clk_x)
     if (!rst && en && wrap_cnt)
       {re, im} <= #DELAY $random;

   //-------------------------------------------------------------------------
   //  Fill a block with visibilities, and then switch banks.
   reg [BLOCK-1:0] blk = 0;
   wire [BLOCK-1:0] next_blk = wrap_blk ? 0 : blk + 1 ;
   wire             wrap_blk = blk == COUNT-1;

   always @(posedge clk_x)
     if (rst) begin
        sw  <= #DELAY 0;
        blk <= #DELAY 0;
     end
//      else if (en && wrap_cnt) begin
     else if (en) begin
        sw  <= #DELAY cnt == 6 && wrap_blk;
        blk <= #DELAY wrap_cnt ? next_blk : blk;
     end
     else begin
        sw  <= #DELAY 0;
        blk <= #DELAY blk;
     end

   //-------------------------------------------------------------------------
   //  Watch for overflows.
   reg old_oc = 0, old_os = 0;

   always @(posedge clk_x)
     if (rst) {old_oc, old_os} <= #DELAY 0;
     else     {old_oc, old_os} <= #DELAY {oc, os};

   always @(posedge clk_x) begin
      if (!old_oc && oc) $display("\n*** Cosine overflow ***\n", $time);
      if (!old_os && os) $display("\n*** Sine overflow ***\n", $time);
   end


   //-------------------------------------------------------------------------
   //  Read back visibility data, from the correlators' registers.
   //-------------------------------------------------------------------------
   wire bst_w = get || cyc && stb && adr < 7'h1b;
   reg  bst = 0;                // Use BurSTs (Bulk Seq. Transfers)

   always @(posedge clk_b)
     if (rst) bst <= #DELAY 0;
     else     bst <= #DELAY bst_w;

   always @(posedge clk_b)
     if (rst) begin
        {fin, get} <= #DELAY 0;
        {cyc, stb} <= #DELAY 0;
     end
     else if (get) begin
        {fin, get} <= #DELAY 0;
        {cyc, stb} <= #DELAY 7;
     end
     else if (cyc && stb && ack && !bst) begin
//      else if (cyc && stb && ack && adr == 7'h1b) begin
        {fin, get} <= #DELAY 2;
        {cyc, stb} <= #DELAY 0;
     end
     else begin
        {fin, get} <= #DELAY 0;
        {cyc, stb} <= #DELAY {cyc, stb};
     end

   // There are 12 registers per cos/sin component, per correlator.
   wire [6:0] next_adr = adr[3:0] == 11 ? adr + 5 : adr + 1 ;

   always @(posedge clk_b)
     if (rst || get || fin)        adr <= #DELAY 0;
     else if (cyc && stb && bst_w) adr <= #DELAY next_adr;

   //-------------------------------------------------------------------------
   // Display the data, and which correlator and register it is from.
   reg [1:0]  ci;
   reg [4:0]  ri;

   always @(posedge clk_b)
     begin
        {ci, ri} <= adr;
        if (cyc && stb && ack)
          $display("%8t: Vis = %08x (c: %1x, r:%02x)", $time, dat, ci, ri);
     end


   //-------------------------------------------------------------------------
   //  Device under test (DUT).
   //-------------------------------------------------------------------------
   correlator_block
     #(  .ACCUM (ACCUM),
//          .PAIRS0(PAIRS0),
//          .PAIRS1(PAIRS1),
//          .PAIRS2(PAIRS2),
//          .PAIRS3(PAIRS3),
         .DELAY (DELAY)
         ) CORRELATOR_BLOCK0
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_b),
         .cyc_i(cyc),
         .stb_i(stb),
         .we_i (we),
         .bst_i(bst),
         .ack_o(ack),
         .adr_i(adr[6:0]),
         .dat_i(32'bx),
         .dat_o(dat),

         .sw(sw),
         .en(en),
         .re(re[11:0]),
         .im(im[11:0]),

         .overflow_cos(oc),
         .overflow_sin(os)
         );


endmodule // correlator_block_tb
