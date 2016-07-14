`timescale 1ns/100ps
/*
 * Module      : verilog/tart_aquire.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Time-multiplexed correlator block.
 * 
 * This version operates the Xilinx SRAM's in Simple Dual-Port mode, to make
 * better use of the available SRAM resources.
 * 
 * NOTE:
 *  + typically several of these would be attached to a common set of antenna
 *    and a system bus;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as multi-
 *    port SRAM's are used;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM resources;
 *  + ignores write-attempts, but still generates acknowledges;
 * 
 * Changelog:
 *  + 04/07/2016  --  initial file (refactored from `correlator`);
 * 
 */

`include "tartcfg.v"

`define __NERDY

module correlator_DSP
  #(parameter ACCUM = `ACCUM_BITS,            // Re/Im accumulator bit-widths
    parameter MSB   = ACCUM - 1,
    parameter WIDTH = ACCUM + ACCUM, // Combined Re & Im components
    parameter WSB   = WIDTH - 1,
    parameter BADDR = 9,        // Buffer address bit-width
    parameter ASB   = BADDR - 1,
    parameter BSIZE = 1 << BADDR,
    parameter MRATE = 12,       // Time-multiplexing rate
    parameter MBITS = 4,
    parameter MSIZE = (2<<MBITS)-1,
    parameter TSB   = MBITS - 1,
    // Pairs of antennas to correlate:
    parameter PAIRS = 120'hb1a191817161b0a090807060,
    parameter DELAY = 3)
   (
    input           clk_x, // correlator clock
    input           rst,

    // Wishbone-like bus interface for reading visibilities.
    input           clk_i, // bus clock
    input           cyc_i,
    input           stb_i,
    input           we_i, // writes are ignored
    input           bst_i, // burst-mode transfer?
    output reg      ack_o = 0,
    input [BADDR:0] adr_i,
    input [MSB:0]   dat_i,
//     output reg [MSB:0] dat_o,
    output [MSB:0]  dat_o,

    // Real and imaginary components from the antennas.
    input           sw, // switch banks
    input           en, // data is valid
    input [23:0]    re,
    input [23:0]    im,

    output reg      overflow_cos = 0,
    output reg      overflow_sin = 0
    );


   //-------------------------------------------------------------------------
   //  Xilinx Distributed & Block RAM's for the accumulators & visibilities.
   //-------------------------------------------------------------------------
   // For Xilinx FPGA's, this should be two `RAM32M's, and operating in SDP
   // mode.
   wire [MSB:0]        dcos, dsin, qcos, qsin;
`ifndef __NERDY
   reg [MSB:0]         cosram[0:MSIZE];
   reg [MSB:0]         sinram[0:MSIZE];

   assign dcos = cosram[x_rd_adr];
   assign dsin = sinram[x_rd_adr];
`endif

   // Xilinx Block RAM for the buffered, visibilities data.
   reg [WSB:0]         visram[0:BSIZE-1];
   reg [ASB-MBITS:0]   block = 0;
   reg                 swap = 0, clear = 0;
   reg [TSB:0]         x_rd_adr = 0, x_wt_adr = 0, x_wr_adr = 0;
   wire                wrap_x_rd_adr = x_rd_adr == MRATE - 1;
   wire [TSB:0]        next_x_rd_adr = wrap_x_rd_adr ? 0 : x_rd_adr + 1 ;
   wire                oc, os;
   reg                 go = 0, valid = 0;


   //-------------------------------------------------------------------------
   //  Control signals.
   //-------------------------------------------------------------------------
   //  Add a cycle of latency to wait for the RAM read.
   //  Add another cycle of latency, to wait for the DSP addition.
   always @(posedge clk_x)
     {valid, go} <= #DELAY {go, en};


   //-------------------------------------------------------------------------
   //  Wishbone-like bus interface logic.
   //-------------------------------------------------------------------------
   reg [WSB:0]         d_reg = 0;
   reg                 m_reg = 0;

   assign dat_o = m_reg ? d_reg[WSB:ACCUM] : d_reg[MSB:0];

   always @(posedge clk_i)
     if (cyc_i && stb_i) begin
        m_reg <= #DELAY adr_i[0];
        d_reg <= #DELAY visram[adr_i[BADDR:1]];
     end

   //  Acknowledge any request, even if ignored.
   always @(posedge clk_i)
     if (rst) ack_o <= #DELAY 0;
     else     ack_o <= #DELAY cyc_i && stb_i;


   //-------------------------------------------------------------------------
   //  Correlator memories.
   //-------------------------------------------------------------------------
   //  Pipelined correlator requires cycles for:
   //    { read, MAC, write } .
   always @(posedge clk_x)
     if (rst) begin
        x_rd_adr <= #DELAY 0;
        x_wt_adr <= #DELAY 0;
        x_wr_adr <= #DELAY 0;
     end
     else begin
        x_rd_adr <= #DELAY en    ? next_x_rd_adr : x_rd_adr;
        x_wt_adr <= #DELAY go    ? x_rd_adr      : x_wt_adr;
        x_wr_adr <= #DELAY valid ? x_wt_adr      : x_wr_adr;
     end

   //  Banks are switched at the next address-wrap event.
   always @(posedge clk_x)
     if (rst) begin
        swap  <= #DELAY 0;
        block <= #DELAY 0;
     end
     else if (wrap_x_rd_adr && (sw || swap)) begin // swap banks
        swap  <= #DELAY 0;
        block <= #DELAY block + 1;
     end
     else if (sw && !swap) begin // swap banks @next wrap
        swap  <= #DELAY 1;
     end

   //  Clear a bank when correlators are enabled, or during the first set of
   //  writes after a bank-switch.
   always @(posedge clk_x)
     if (rst || !en)
        clear <= #DELAY 1;
     else if (wrap_x_rd_adr && (sw || swap))
        clear <= #DELAY 1;
     else if (wrap_x_rd_adr && clear) // finished restarting counters
        clear <= #DELAY 0;

`ifndef __NERDY
   //  Read and write RAM contents for the correlator.
   //  TODO: Verify that this uses RAM32M's in SDP mode.
   always @(posedge clk_x)
     if (valid) begin
        cosram[x_wr_adr] <= #DELAY qcos;
        sinram[x_wr_adr] <= #DELAY qsin;
     end
`endif

   //  Write the current sums to the BRAM, so when a block-switch occurs, the
   //  buffer already contains the desired visibilities.
   always @(posedge clk_x)
     if (valid)
       visram[{block, x_wr_adr}] <= #DELAY {qsin, qcos};


   //-------------------------------------------------------------------------
   //  Select pairs of antenna to correlate.
   //-------------------------------------------------------------------------
   //  TODO: Can more of this be parameterised?
   parameter PAIRS00 = (PAIRS >>   0) & 10'h3ff;
   parameter PAIRS01 = (PAIRS >>  10) & 10'h3ff;
   parameter PAIRS02 = (PAIRS >>  20) & 10'h3ff;
   parameter PAIRS03 = (PAIRS >>  30) & 10'h3ff;
   parameter PAIRS04 = (PAIRS >>  40) & 10'h3ff;
   parameter PAIRS05 = (PAIRS >>  50) & 10'h3ff;
   parameter PAIRS06 = (PAIRS >>  60) & 10'h3ff;
   parameter PAIRS07 = (PAIRS >>  70) & 10'h3ff;
   parameter PAIRS08 = (PAIRS >>  80) & 10'h3ff;
   parameter PAIRS09 = (PAIRS >>  90) & 10'h3ff;
   parameter PAIRS0A = (PAIRS >> 100) & 10'h3ff;
   parameter PAIRS0B = (PAIRS >> 110) & 10'h3ff;

`ifdef __icarus
   // NOTE: Icarus Verilog doesn't seem to support curly-braces for setting
   //   the wire values;
   wire [9:0]   pairs[0:11];

   assign pairs[00] = PAIRS00;
   assign pairs[01] = PAIRS01;
   assign pairs[02] = PAIRS02;
   assign pairs[03] = PAIRS03;
   assign pairs[04] = PAIRS04;
   assign pairs[05] = PAIRS05;
   assign pairs[06] = PAIRS06;
   assign pairs[07] = PAIRS07;
   assign pairs[08] = PAIRS08;
   assign pairs[09] = PAIRS09;
   assign pairs[10] = PAIRS0A;
   assign pairs[11] = PAIRS0B;
`else
   wire [9:0]   pairs[0:11] = {PAIRS00, PAIRS01, PAIRS02, PAIRS03,
                               PAIRS04, PAIRS05, PAIRS06, PAIRS07,
                               PAIRS08, PAIRS09, PAIRS0A, PAIRS0B};
`endif

   wire [9:0]   pairs_index = pairs[x_rd_adr];
   wire [4:0]   a_index = pairs_index[4:0];
   wire [4:0]   b_index = pairs_index[9:5];

   /*
   wire [119:0] pairs_wide = PAIRS;
   wire [9:0]   pairs[0:11];

   assign pairs[00] = pairs_wide[  9:  0];
   assign pairs[01] = pairs_wide[ 19: 10];
   assign pairs[02] = pairs_wide[ 29: 20];
   assign pairs[03] = pairs_wide[ 39: 30];
   assign pairs[04] = pairs_wide[ 49: 40];
   assign pairs[05] = pairs_wide[ 59: 50];
   assign pairs[06] = pairs_wide[ 69: 60];
   assign pairs[07] = pairs_wide[ 79: 70];
   assign pairs[08] = pairs_wide[ 89: 80];
   assign pairs[09] = pairs_wide[ 99: 90];
   assign pairs[10] = pairs_wide[109:100];
   assign pairs[11] = pairs_wide[119:110];
    */

   wire         ar = re[a_index];
   wire         br = re[b_index];
   wire         bi = im[b_index];


   //-------------------------------------------------------------------------
   //  Time-multiplexed correlator.
   //-------------------------------------------------------------------------
   correlate_cos_sin_DSP
     #(  .ACCUM(ACCUM), .DELAY(DELAY) ) CORR_COS_SIN0
       ( .clk(clk_x),
         .rst(rst),
         .clr(clear),

         // Antenna enables and inputs:
         .en(en),
         .vld(go),
         .ar(ar),
         .br(br),
         .bi(bi),

         // Accumulator inputs and outputs:
         .dcos(dcos),
         .dsin(dsin),
         .qcos(qcos),
         .qsin(qsin),

         // Overflow flags:
         .oc(oc),
         .os(os)
         );

`ifdef __NERDY
   //-------------------------------------------------------------------------
   //  RAM32M's implemented the nerdy way.
   //-------------------------------------------------------------------------
   RAM32X6_SDP
     #( .INITA(64'h0),
        .INITB(64'h0),
        .INITC(64'h0),
        .INITD(64'h0),
        .DELAY(3)
        ) RAM32X6_SDP_COS0 [3:0]
       (.WCLK(clk_x),
        .WE(valid),
        .WADDR({1'b0, x_wr_adr}),
        .DI(qcos),
        .RADDR({1'b0, x_rd_adr}),
        .DO(dcos)
        );

   RAM32X6_SDP
     #( .INITA(64'h0),
        .INITB(64'h0),
        .INITC(64'h0),
        .INITD(64'h0),
        .DELAY(3)
        ) RAM32X6_SDP_SIN0 [3:0]
       (.WCLK(clk_x),
        .WE(valid),
        .WADDR({1'b0, x_wr_adr}),
        .DI(qsin),
        .RADDR({1'b0, x_rd_adr}),
        .DO(dsin)
        );
`endif //  `ifdef __NERDY


endmodule // correlator_DSP
