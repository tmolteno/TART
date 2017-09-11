`timescale 1ns/100ps
/*
 * Module      : wishbone/wb_prefetch_tb.v
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

module wb_prefetch_tb;

   parameter WIDTH = 32;        // Number of bits of a block
   parameter MSB   = WIDTH-1;
   parameter SBITS = 10;
   parameter SIZE  = 1 << SBITS;
   parameter ASB   = SBITS-1;
   parameter PSB   = SBITS+1;
   parameter DELAY = 3;
   parameter COUNT = 16;

   wire [MSB:0] dat;
   reg [MSB:0]  val;
   reg [ASB:0]  adr;
   reg          clk = 1, rst = 0;
   reg          cyc = 0, stb = 0, we = 0, bst = 0;
   reg          set = 0, get = 0, fin = 0, pre = 0;
   wire         ack, rdy, bst_w;

   //-------------------------------------------------------------------------
   //  Setup bus clock.
   always #5  clk <= ~clk;


   //-------------------------------------------------------------------------
   //  Simulate SRAM accesses.
   integer      num = 0;
   integer      ptr = 0;
   initial begin : SIM_BLOCK
      $dumpfile ("prefetch_tb.vcd");
      $dumpvars;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing reset:\n", $time);
      #33 rst <= 1; #40 rst <= 0;

      //----------------------------------------------------------------------
      $display("\n%8t: Burst writing to set SRAM contents:", $time);
      #40 set <= 1; num <= COUNT; ptr <= 0;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("\n%8t: Beginning prefetch:", $time);
      #40 pre <= 1;
      #10 pre <= 0;
      while (!rdy) #10;

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
     if (set || bst && we)
       val <= #DELAY $random;


   //-------------------------------------------------------------------------
   //  Generate WB-like transactions.
   //-------------------------------------------------------------------------
   assign bst_w = num > 2 && cyc;

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
   //  Signals from the prefetch unit, to the SRAM.
   wire         b_cyc, b_stb, b_we, b_bst, b_ack;
   wire [7:0]   b_val, b_dat;
   wire [PSB:0] b_adr;

   //  Signals from the prefetch unit, to the testbench.
   wire         c_cyc, c_stb, c_we, c_bst;
   wire [7:0]   c_dat;
   wire [PSB:0] c_adr;
   reg          c_ack = 0;
   reg [7:0]    c_val;

   //  Acknowledge and display the data arriving from the prefetch unit.
   always @(posedge clk)
     if (rst) c_ack <= #DELAY 0;
     else     c_ack <= #DELAY c_cyc && c_stb;

   always @(posedge clk)
     if (c_cyc && !c_we)
       $display("%8t: data received:\t%02x", $time, c_dat);


   //-------------------------------------------------------------------------
   //  Devices under test (DUT).
   //-------------------------------------------------------------------------
   wb_prefetch #( .WIDTH(8), .SBITS(SBITS+2), .COUNT(17),
                  .BSIZE(5), .BBITS(3) ) PREFETCH0
     ( .rst_i(rst),
       .clk_i(clk),

       .begin_i(pre),
       .ready_o(rdy),

       .a_cyc_o(b_cyc),         // prefetch interface (from the SRAM)
       .a_stb_o(b_stb),
       .a_we_o (b_we),
       .a_bst_o(b_bst),
       .a_ack_i(b_ack),
       .a_adr_o(b_adr),
       .a_dat_i(b_val),
       .a_dat_o(b_dat),

       .b_cyc_o(c_cyc),         // output interface (to the testbench)
       .b_stb_o(c_stb),
       .b_we_o (c_we),
       .b_bst_o(c_bst),
       .b_ack_i(c_ack),
       .b_adr_o(c_adr),
       .b_dat_i(c_val),
       .b_dat_o(c_dat)
       );

   wb_sram_dual_port #( .SBITS(SBITS) ) SRAM0
     ( .rst_i(rst),

       .a_clk_i(clk),           // this port is used to fill the SRAM with
       .a_cyc_i(cyc),           // random data, at startup
       .a_stb_i(stb),
       .a_we_i (we),
       .a_bst_i(bst),
       .a_ack_o(ack),
       .a_adr_i(adr),
       .a_dat_i(val),
       .a_dat_o(dat),

       .b_clk_i(clk),           // this port is driven by the prefetch unit
       .b_cyc_i(b_cyc),
       .b_stb_i(b_stb),
       .b_we_i (b_we),
       .b_bst_i(b_bst),
       .b_ack_o(b_ack),
       .b_adr_i(b_adr),
       .b_dat_i(b_dat),
       .b_dat_o(b_val)
       );


endmodule // wb_prefetch_tb
