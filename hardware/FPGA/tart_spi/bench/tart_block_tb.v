`timescale 1ns/100ps
/*
 * Module      : bench/tart_block_tb.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
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
 * 
 * Description:
 * The purpose of this testbench is to test that the correlator computation is
 * correct, and to ensure that the data is read back faithfully.
 * 
 * NOTE:
 *  + tests just one block (which contains four correlators);
 * 
 * TODO:
 * 
 */

`include "tartcfg.v"

//----------------------------------------------------------------------------
//  There are two correlator-block modules, one that uses DSP48 accumulators,
//  and the other uses standard carry-chain accumulators.
`define __TEST_DSP_BLOCK
// `undef  __TEST_DSP_BLOCK

//----------------------------------------------------------------------------
//  Use a more compact print-out of the visibilities, or use a column of them?
// `define __USE_COLUMN_DISPLAY
`undef  __USE_COLUMN_DISPLAY

`define __USE_DECIMAL_DISPLAY

module tart_block_tb;

   //-------------------------------------------------------------------------
   //
   //  SIMULATION SETTINGS.
   //
   //-------------------------------------------------------------------------
   //  Antenna + accumulator settings:
   parameter AXNUM = `NUM_ANTENNA; // Number of antenna inputs
   parameter NSB   = AXNUM-1;
   parameter ACCUM = `ACCUM_BITS;  // Bit-width of the accumulators
   parameter BLOCK = ACCUM;        // Block samples-counter bits
   parameter MSB   = BLOCK-1;      // Accumulator MSB
   parameter TRATE = `TMUX_RATE;   // Time-multiplexing rate
   parameter TBITS = `TMUX_BITS;   // TMUX bits
   parameter TSB   = TBITS-1;      // MSB of TBITS

   //  Read-back settings:
   parameter RSIZE = TRATE*2;      // Number of sine & cosine values that are
   parameter RBITS = TBITS+1;      // stored within a correlator, and the
   parameter RSB   = RBITS-1;      // number of counter-bits needed

   //  Settings for the visibilities data banks:
   parameter BREAD = NREAD << 2;
   parameter XBITS = `BANK_BITS; // Bank-counter bit-width; of the b
   parameter XSB   = XBITS-1;    // MSB of the bank-counter
   parameter BANKS = 1 << XBITS; // Number of visibilities banks

   //  Internal correlator data-bus settings:
   parameter CBITS = XBITS+FBITS; // Correlator-bus address bit-width
   parameter CSB   = CBITS-1;     // MSB of correlator-bus address

   //  Correlator-block Wishbone-like bus setttings:
   parameter ABITS = 2+XBITS+RBITS; // Address bit-width
   parameter ASB   = ABITS-1;       // Address MSB

   //  Read-back settings:
   parameter FBITS = `READ_BITS; // Fetch-counter bit-width
   parameter FSB   = FBITS-1;    // MSB of fetch-counter
   parameter NREAD = 96;
   parameter COUNT = 6;
   parameter BSIZE = (1 << COUNT) - 1;
   parameter READS = 2;         // Banks of visibilities to read

   //  Additional simulation settings:
   parameter RNG   = `RANDOM_DATA; // Use random antenna data?
   parameter DELAY = `DELAY;       // Simulated combinational delay


   //-------------------------------------------------------------------------
   //
   //  SIMULATION SIGNALS.
   //
   //-------------------------------------------------------------------------
   //  Fake antenna signals:
   wire [NSB:0] antenna;
   reg          enable = 1'b0;
   reg [NSB:0]  data [0:255];

   //  Global control signals:
   reg          clk_x = 1'b1, b_clk = 1'b1, rst = 1'b0;
   wire         b_rst = rst;         

   //  Fake Hilbert-transform signals:
   wire [NSB:0] re, im;
   wire         valid, strobe, frame;

   //  Correlator signals:
   reg [MSB:0]  blocksize;
   wire [XSB:0] bank;
   wire         swap_x, switch;

   //  Bus signals:
   wire         b_cyc, b_stb, b_we, b_bst, b_ack, b_wat, b_err;
   wire [RSB:0] b_adr;
   wire [MSB:0] b_dat, b_vis, b_viz;

   //  Read-back signals:
   reg          fetch = 1'b0, read = 1'b0;
   wire         done;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION STIMULI.
   //
   //-------------------------------------------------------------------------
   //  Setup correlator and bus clocks, respectively.
   always #`CLK_X  clk_x <= ~clk_x;
   always #`CLK_B  b_clk <= ~b_clk;


   //-------------------------------------------------------------------------
   //  Simulate two visibility calculations.
   integer      num = 0;
   integer      ptr = 0;
   initial begin : SIM_BLOCK
      if (COUNT < 7) begin
         $dumpfile ("vcd/block_tb.vcd");
         $dumpvars;
      end

      //----------------------------------------------------------------------
      $display("\n%12t: TART correlator settings:", $time);
      $display(  "%12t:  Simulated correlator settings:", $time);
      $display(  "%12t:   Accumulator bit-width:   \t\t%4d", $time, ACCUM);
      $display(  "%12t:   Correlator bus data-width:       \t%4d", $time, ACCUM);
      $display(  "%12t:   Correlator bus address-width:    \t%4d", $time, ABITS);
      $display(  "%12t:   Number of correlator banks:      \t%4d", $time, BANKS);
      $display(  "%12t:   Number of counter bits:          \t%4d", $time, COUNT);
      $display(  "%12t:   Max count value (samples/bank):  \t%4d", $time, BSIZE);
      $display(  "%12t:   Data prefetch block-size (words):\t%4d", $time, RSIZE);

      //----------------------------------------------------------------------
      $display("\n%12t: Generating fake antenna data:", $time);
      if (RNG)
        $display("%12t:  (Data is random values)", $time);
      else
        $display("%12t:  (Data is just increasing counter values)", $time);
      for (ptr = 0; ptr < 256; ptr = ptr+1) begin
         if (RNG) data[ptr] <= $random;
         else     data[ptr] <= ptr;
      end

      //----------------------------------------------------------------------
      #20 $display("\n%12t: Issuing RESET.\n", $time);
      #13 rst <= 1; #40 rst <= 0;

      //----------------------------------------------------------------------
      $display("\n%12t: Setting block-size & beginning correlation (bank 0)", $time);
      #40 blocksize <= BSIZE; enable <= 1'b1;

      //----------------------------------------------------------------------
      while (!switch) #10; while (switch) #10;
      $display("%12t: Switching banks (bank 1)", $time);

      //----------------------------------------------------------------------
      #10 $display("\n%12t: Fetching back visibilities (bank 0)", $time);
      #10 fetch <= 1'b1; #10 while (fetch) #10;

      //----------------------------------------------------------------------
      while (!switch) #10; while (switch) #10;
      $display("\n%12t: Stopping data-correlation (bank 1)", $time);
      #10 enable <= 1'b0;

      //----------------------------------------------------------------------
      #10 $display("\n%12t: Fetching back visibilities (bank 1)", $time);
      #10 fetch <= 1'b1; #10 while (fetch) #10;

      //----------------------------------------------------------------------
      #80 $display("\n%12t: Simulation finished:", $time);
      $finish;
   end


   //-------------------------------------------------------------------------
   //
   //  SIMULATION FAKE-DATA.
   //
   //-------------------------------------------------------------------------
   //  Generate fake antenna data, from the fake DRAM contents.
   reg [TSB:0]  cnt = 0;
   wire [TSB:0] next_cnt = wrap_cnt ? 0 : cnt + 1 ;
   wire         wrap_cnt = cnt == TRATE-1;
   integer      rd_adr = 0;

`ifdef CONST_DATA
   assign antenna = `CONST_WORD;
`else
   assign antenna = RNG ? mfsr_reg : data[rd_adr[7:0]];
`endif

   always @(posedge clk_x)
     if (rst) cnt <= #DELAY 0;
     else     cnt <= #DELAY enable ? next_cnt : cnt;

   always @(posedge clk_x)
     if (rst) rd_adr <= #DELAY 0;
     else     rd_adr <= #DELAY enable && wrap_cnt ? rd_adr + 1 : rd_adr;

   //-------------------------------------------------------------------------
   //  Generate random antenna data using a MFSR.
   //-------------------------------------------------------------------------
   wire [31:0]     mfsr_new;
   reg [31:0]      mfsr_reg = RNG;

   always @(posedge clk_x)
     if (rst) mfsr_reg <= #DELAY RNG;
     else     mfsr_reg <= #DELAY enable && wrap_cnt ? mfsr_new : mfsr_reg;

   //-------------------------------------------------------------------------
   //  One of Roy's MFSR's (which is similar to a LFSR, but fewer gates).
   mfsr32 MFSR32 (.count_i(mfsr_reg), .count_o(mfsr_new));


   //-------------------------------------------------------------------------
   //
   //  SIMULATION TERMINATION.
   //
   //-------------------------------------------------------------------------
   //  Exit if the simulation appears to have stalled.
   parameter LIMIT = 1000 + (1 << COUNT) * 320;

   initial begin : SIM_FAILED
      $display("%12t: Simulation TIMEOUT limit:\t%12d", $time, LIMIT);
      #LIMIT $display ("\nTIMEOUT!\n");
      $finish;
   end // SIM_FAILED


   //-------------------------------------------------------------------------
   //
   //  SIMULATION RESULTS.
   //
   //-------------------------------------------------------------------------
   wire [FBITS:0] f_nxt = f_adr + 1;
   reg [MSB:0]    fetched [0:NREAD-1];
   reg [FSB:0]    f_adr = {FBITS{1'b0}};

   //  Display bank-switch events.
   always @(posedge b_clk) begin
      if (switch)
        $display("%12t: New block available.", $time);
   end

`ifdef  __USE_COLUMN_DISPLAY
   //-------------------------------------------------------------------------
   //  Display the data, and which correlator and register it is from.
   always @(posedge b_clk) begin
      if (b_cyc && !b_we && b_ack)
        $display("%12t: Vis = %08x (d: %8d)", $time, b_viz, b_viz);
   end
`else

   //-------------------------------------------------------------------------
   //  Assemble bytes into words, and stored them until the transfer has been
   //  completed.
   always @(posedge b_clk)
     if (rst || !fetch) begin
        f_adr <= #DELAY {FBITS{1'b0}};
     end
     else if (b_cyc && !b_we && b_ack) begin
        f_adr <= #DELAY f_nxt;
        fetched[f_adr] <= #DELAY b_viz;
     end

   //-------------------------------------------------------------------------
   //  Format and display the retrieved visibilities.
   always @(negedge fetch)
     if (f_adr > 0) begin
        $display("\n%12t: Fetched visibilities (num = %d):", $time, f_adr);
        for (ptr = 0; ptr < NREAD; ptr = ptr + TRATE) begin
           $write("\t");
           for (num = 0; num < TRATE; num = num + 1)
 `ifdef __USE_DECIMAL_DISPLAY
             $write("%7d ", fetched[ptr + num]);
 `else
             $write("%06x ", fetched[ptr + num]);
 `endif
           $write("\n");
        end
     end
`endif // !`ifdef __USE_COLUMN_DISPLAY


   //-------------------------------------------------------------------------
   //     
   //     VISIBILITIES-DATA CONTROL AND READ-BACK.
   //     
   //-------------------------------------------------------------------------
   wire [XBITS:0] n_blk = c_blk + 1;
   wire [2:0]     n_cor = c_cor + 1;
   reg [XSB:0]    c_blk = {XBITS{1'b0}};
   reg [1:0]      c_cor = 2'b00;
   wire [ASB:0]   c_adr = {c_blk, b_adr[RSB:1], c_cor, b_adr[0]};

   assign b_viz = fetch ? b_vis : {ACCUM{1'bz}};

   always @(posedge b_clk)
     if (b_rst)
       {fetch, read, c_blk, c_cor} <= #DELAY {XBITS+4{1'b0}};
     else if (fetch && done && c_cor == 2'b11) // end
       {fetch, c_blk} <= #DELAY {1'b0, n_blk[XSB:0]};
     else if (fetch && done) // read next correlator
       {read, c_cor} <= #DELAY {1'b1, n_cor[1:0]};
     else if (fetch && !b_cyc && !read) // begin
       {read, c_cor} <= #DELAY 3'b100;
     else
       read <= #DELAY 1'b0;


   //-------------------------------------------------------------------------
   //  Hilbert transform to recover imaginaries.
   //-------------------------------------------------------------------------
   fake_hilbert #( .WIDTH(AXNUM) ) HILB0
     (  .clk(clk_x),
        .rst(rst),
        .en(enable),
        .d(antenna),
        .valid(valid),
        .strobe(strobe), // `antenna` data is valid
        .frame(frame),   // last cycle for `antenna` data to be valid
        .re(re),
        .im(im)
        );


   //-------------------------------------------------------------------------
   //  TART bank-switching unit.
   //-------------------------------------------------------------------------
   tart_bank_switch #( .COUNT(BLOCK) ) SW0
     ( .clk_x(clk_x),
       .clk_i(b_clk),
       .rst_i(rst),
       .ce_i (valid),
       .frame_i(frame),
       .bcount_i(blocksize),
       .swap_x(swap_x),
       .swap_o(switch)
       );


   //-------------------------------------------------------------------------
   //  A single block of 4 correlators, and implemented using the Xilinx DSP48
   //  primitives.
   //-------------------------------------------------------------------------
   //  The configuration file includes the parameters that determine which
   //  antannae connect to each of the correlators.
`include "../include/tart_pairs.v"
`ifdef  __TEST_DSP_BLOCK
   assign b_ack = dsp_ack;
   assign b_vis = dsp_vis;
   assign bank  = dsp_bank;
`else
   assign b_ack = sdp_ack;
   assign b_vis = sdp_vis;
   assign bank  = sdp_bank;
`endif

   wire           dsp_ack;
   wire [MSB:0]   dsp_vis;
   wire [XSB:0]   dsp_bank;

   correlator_block_DSP
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS00_00),
         .PAIRS1(PAIRS00_01),
         .PAIRS2(PAIRS00_02),
         .PAIRS3(PAIRS00_03),
         .DELAY (DELAY)
         ) DSPB
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(b_clk),
         .cyc_i(b_cyc),
         .stb_i(b_stb),
         .we_i (1'b0),
         .bst_i(b_bst),
         .ack_o(dsp_ack),
         .adr_i(c_adr),
         .dat_i(b_dat),
         .dat_o(dsp_vis),

         .sw_i(swap_x),
         .en_i(valid),
         .re_i(re),
         .im_i(im),

         .bank_o(dsp_bank)
         );

   wire           sdp_ack;
   wire [MSB:0]   sdp_vis;
   wire [XSB:0]   sdp_bank;

   correlator_block_SDP
     #(  .ACCUM (BLOCK),
         .PAIRS0(PAIRS00_00),
         .PAIRS1(PAIRS00_01),
         .PAIRS2(PAIRS00_02),
         .PAIRS3(PAIRS00_03),
         .DELAY (DELAY)
         ) SDPB
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(b_clk),
         .cyc_i(b_cyc),
         .stb_i(b_stb),
         .we_i (1'b0),
         .bst_i(b_bst),
         .ack_o(sdp_ack),
         .adr_i(c_adr),
         .dat_i(b_dat),
         .dat_o(sdp_vis),

         .sw_i(swap_x),
         .en_i(valid),
         .re_i(re),
         .im_i(im),

         .bank_o(sdp_bank)
         );


   //-------------------------------------------------------------------------
   //  Address-generation unit, for each block.
   //-------------------------------------------------------------------------
   assign b_we  = 1'b0;
   assign b_wat = 1'b0;
   assign b_err = 1'b0;

   wb_get_block
     #(.BSIZE(RSIZE), .BBITS(RBITS), .DELAY(DELAY)
       ) FETCH0
       ( .clk_i(b_clk),
         .rst_i(b_rst),
         .cyc_o(b_cyc),
         .stb_o(b_stb),
         .we_o (b_we),
         .bst_o(b_bst),
         .ack_i(b_ack),
         .wat_i(b_wat),
         .err_i(b_err),
         .adr_o(b_adr),

         .read_i(read),
         .done_o(done)
         );


endmodule // tart_block_tb
