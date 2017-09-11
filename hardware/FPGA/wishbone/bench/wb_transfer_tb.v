`timescale 1ns/100ps
/*
 * Module      : bench/wb_transfer_tb.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
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
 * Generates the control and address signals needed, to prefetch a block of
 * data, using Wishbone-like burst transfers.
 * 
 * NOTE:
 * 
 * TODO:
 *  + STUB -- only a copy & paste of another TB;
 * 
 * Changelog:
 *  + 23/11/2016  --  initial file;
 * 
 */

module wb_transfer_tb;

   parameter WIDTH = 32;        // Number of bits of a block
   parameter MSB   = WIDTH-1;
   parameter BYTES = WIDTH>>3;
   parameter BSB   = BYTES-1;
   parameter ABITS = 9;
   parameter ASB   = ABITS-1;
   parameter ESB   = ABITS+1;
   parameter SIZE  = 1 << ABITS;
   parameter DELAY = 3;

   parameter FETCH = 8;
   parameter FBITS = 3;
   parameter FSB   = FBITS-1;
   parameter STORE = 32;
   parameter SBITS = 5;
   parameter SSB   = SBITS-1;


   reg          clk = 1'b1, rst = 1'b0;
   reg          fetch = 1'b0, store = 1'b0;
   wire         ended, spent;

   wire         f_cyc, f_stb, f_we, f_ack, f_wat, f_rty, f_err;
   wire [FSB:0] f_adr;
   wire [MSB:0] f_drx, f_dtx;

   wire         s_cyc, s_stb, s_we, s_ack, s_wat, s_rty, s_err;
   wire [SSB:0] s_adr;
   wire [7:0]   s_drx;
   reg [7:0]    s_dtx;

   wire         sram_a_ce, sram_a_we;
   wire [ASB:0] sram_a_adr;
   wire [BSB:0] sram_a_bes;
   wire [MSB:0] wb_a_to_sram, sram_to_wb_a;

   wire         sram_b_ce, sram_b_we;
   wire [ESB:0] sram_b_adr;
   wire [7:0]   wb_b_to_sram, sram_to_wb_b;


   //-------------------------------------------------------------------------
   //  Setup bus clock.
   always #5  clk <= ~clk;


   //-------------------------------------------------------------------------
   //  
   //  SIMULATION STIMULI.
   // 
   //-------------------------------------------------------------------------
   //  Simulate SRAM accesses.
   integer      num = 0;
   integer      ptr = 0;
   initial begin : SIM_BLOCK
      $dumpfile("vcd/fetch_tb.vcd");
      $dumpvars;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing reset:\n", $time);
      #33 rst <= 1'b1; #40 rst <= 1'b0;

      //----------------------------------------------------------------------
      $display("\n%8t: Burst writing to set SRAM contents:", $time);
      #40 store <= 1'b1; #10 store <= 1'b0;
      while (!spent) #10;

      //----------------------------------------------------------------------
      $display("\n%8t: Beginning fetch:", $time);
      #40 fetch <= 1'b1; #10 fetch <= 1'b0;
      while (!ended) #10;

      //----------------------------------------------------------------------
      #40 $display("\n%8t: Simulation finished:", $time);
      $finish;
   end

   initial begin : SIM_FAILED
      #1200 $display ("TIMEOUT!");
      $finish;
   end // SIM_FAILED



   //-------------------------------------------------------------------------
   //  
   //  GENERATE TESTING DATA.
   // 
   //-------------------------------------------------------------------------
   always @(posedge clk)
     if (store || s_we && s_stb)
       s_dtx <= #DELAY $random;



   //-------------------------------------------------------------------------
   //  
   //  DISPLAY RESULTS.
   // 
   //-------------------------------------------------------------------------
   always @(posedge clk)
     if (f_cyc && f_ack)
       $display("%12t: data fetched\t= %08x", $time, f_drx);



   //-------------------------------------------------------------------------
   //  
   //  Devices under test (DUT).
   // 
   //-------------------------------------------------------------------------

   //-------------------------------------------------------------------------
   //    Wishbone block fetch.
   //-------------------------------------------------------------------------
   wb_fetch
     #( .FETCH(FETCH),
        .FBITS(FBITS),
        .DELAY(DELAY)
        ) FETCH0
     ( .rst_i(rst),
       .clk_i(clk),

       .fetch_i(fetch),
       .ready_o(ended),

       .cyc_o(f_cyc),         // fetch interface (from the SRAM)
       .stb_o(f_stb),
       .we_o (f_we),
       .ack_i(f_ack),
       .wat_i(f_wat),
       .rty_i(f_rty),
       .err_i(f_err),
       .adr_o(f_adr)
       );


   //-------------------------------------------------------------------------
   //    Wishbone block store.
   //-------------------------------------------------------------------------
   wb_store
     #( .STORE(STORE),
        .SBITS(SBITS),
        .DELAY(DELAY)
        ) STORE0
     ( .rst_i(rst),
       .clk_i(clk),

       .store_i(store),
       .ready_o(spent),

       .cyc_o(s_cyc),         // store interface (to the SRAM)
       .stb_o(s_stb),
       .we_o (s_we),
       .ack_i(s_ack),
       .wat_i(s_wat),
       .rty_i(s_rty),
       .err_i(s_err),
       .adr_o(s_adr)
       );


   //-------------------------------------------------------------------------
   //  Wishbone interface between the fetch unit and the SRAM.
   //-------------------------------------------------------------------------
   wb_sram_port
     #( .ABITS(ABITS),
        .USEBE(1),
        .WIDTH(WIDTH),
        .PIPED(1),
        .TICKS(1),
        .DELAY(DELAY)
        ) SRAM0
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_i(f_cyc),       // this port is used to fill the SRAM with
       .stb_i(f_stb),       // random data, at startup
       .we_i (f_we),
       .ack_o(f_ack),
       .wat_o(f_wat),
       .rty_o(f_rty),
       .err_o(f_err),
       .adr_i({{ABITS-FBITS{1'b0}}, f_adr}),
       .sel_i({BYTES{1'b1}}),
       .dat_i(f_dtx),
       .dat_o(f_drx),

       .sram_ce_o (sram_a_ce),
       .sram_we_o (sram_a_we),
       .sram_adr_o(sram_a_adr),
       .sram_bes_o(sram_a_bes),
       .sram_dat_i(sram_to_wb_a),
       .sram_dat_o(wb_a_to_sram)
       );


   //-------------------------------------------------------------------------
   //  Wishbone interface between the store unit and the SRAM.
   //-------------------------------------------------------------------------
   wb_sram_port
     #( .ABITS(ABITS+2),
        .USEBE(0),
        .WIDTH(8),
        .PIPED(1),
        .TICKS(1),
        .DELAY(DELAY)
        ) SRAM1
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_i(s_cyc),       // this port is used to fill the SRAM with
       .stb_i(s_stb),       // random data, at startup
       .we_i (s_we),
       .ack_o(s_ack),
       .wat_o(s_wat),
       .rty_o(s_rty),
       .err_o(s_err),
       .adr_i({{ABITS-SBITS+2{1'b0}}, s_adr}),
       .sel_i(1'b1),
       .dat_i(s_dtx),
       .dat_o(s_drx),

       .sram_ce_o (sram_b_ce),
       .sram_we_o (sram_b_we),
       .sram_adr_o(sram_b_adr),
       .sram_bes_o(),
       .sram_dat_i(sram_to_wb_b),
       .sram_dat_o(wb_b_to_sram)
       );


   //-------------------------------------------------------------------------
   //  Xilinx, TDP (True Dual Port) block SRAM.
   //-------------------------------------------------------------------------
   RAMB16X32X8_TDP
     #(.DELAY(3)) XSRAM
       (.CLKA (clk),
        .ENA  (sram_a_ce),
        .WEA  (sram_a_bes),
        .ADDRA(sram_a_adr),
        .DIA  (wb_a_to_sram),
        .DOA  (sram_to_wb_a),

        .CLKB (clk),
        .ENB  (sram_b_ce),
        .WEB  (sram_b_we),
        .ADDRB(sram_b_adr),
        .DIB  (wb_b_to_sram),
        .DOB  (sram_to_wb_b)
        );


endmodule // wb_transfer_tb
