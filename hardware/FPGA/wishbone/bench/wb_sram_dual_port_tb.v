`timescale 1ns/100ps
/*
 * Module      : wishbone/wb_sram_dual_port_tb.v
 * Copyright   : (C) Tim Molteno     2017
 *             : (C) Max Scheel      2017
 *             : (C) Patrick Suggate 2017
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
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
 */

module wb_sram_dual_port_tb;

   parameter WIDTH = 8;        // Number of bits of a block
   parameter MSB   = WIDTH-1;
   parameter SBITS = 12;
   parameter SIZE  = 1 << SBITS;
   parameter ASB   = SBITS-1;
   parameter DELAY = 3;

   wire [MSB:0] dat;
   reg [MSB:0]  val;
   reg [ASB:0]  adr;
   reg          clk = 1, rst = 0;
   reg          cyc = 0, stb = 0, we = 0, bst = 0;
   reg          set = 0, get = 0, fin = 0;
   wire         ack, bst_w;

   //-------------------------------------------------------------------------
   //  Setup bus clock.
   always #5  clk <= ~clk;


   //-------------------------------------------------------------------------
   //  Simulate SRAM accesses.
   integer      num = 0;
   integer      ptr = 0;
   initial begin : SIM_BLOCK
      $dumpfile ("dualsram_tb.vcd");
      $dumpvars;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing reset:\n", $time);
      #33 rst <= 1; #40 rst <= 0;

      //----------------------------------------------------------------------
      $display("\n%8t: Single write:", $time);
      #40 set <= 1; num <= 1; ptr <= $random;
      while (!fin) #10;

      $display("\n%8t: Single read:", $time);
      #10 get <= 1; num <= 1;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("\n%8t: Burst write:", $time);
      #40 set <= 1; num <= 16; ptr <= $random;
      while (!fin) #10;

      $display("\n%8t: Burst read:", $time);
      #40 get <= 1; num <= 8;
      while (!fin) #10;

      $display("\n%8t: Burst read:", $time);
      ptr <= ptr + 8;
      #10 get <= 1; num <= 8;
      while (!fin) #10;

      //----------------------------------------------------------------------
      #40 $display("\n%8t: Simulation finished:", $time);
      $finish;
   end

   initial begin : SIM_FAILED
      #1200 $display ("TIMEOUT!");
      $finish;
   end // SIM_FAILED


   //-------------------------------------------------------------------------
   //  Generate write data.
   always @(posedge clk)
     if (set || bst_w && we)
       val <= #DELAY $random;


   //-------------------------------------------------------------------------
   //  Generate WB-like transactions.
   //-------------------------------------------------------------------------
   assign bst_w = num > 1 && cyc;

   always @(posedge clk)
     if (rst) bst <= #DELAY 0;
     else     bst <= #DELAY bst_w || (set || get) && num > 1;

   always @(posedge clk)
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
   //  Device under test (DUT).
   //-------------------------------------------------------------------------
   wb_sram_dual_port #( .SBITS(SBITS) ) SRAM0
     ( .rst_i(rst),

       .a_clk_i(clk),
       .a_cyc_i(a_cyc),
       .a_stb_i(a_stb),
       .a_we_i (a_we),
       .a_bst_i(a_bst),
       .a_ack_o(a_ack),
       .a_adr_i(a_adr),
       .a_dat_i(a_val),
       .a_dat_o(a_dat),

       .b_clk_i(clk),
       .b_cyc_i(cyc),
       .b_stb_i(stb),
       .b_we_i (we),
       .b_bst_i(bst),
       .b_ack_o(ack),
       .b_adr_i(adr),
       .b_dat_i(val),
       .b_dat_o(dat)
       );


endmodule // wb_sram_dual_port_tb
