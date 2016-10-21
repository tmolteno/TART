`timescale 1ns/100ps
/*
 * Module      : verilog/bus/wb_prefetch.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
 * 
 * Has a master Wishbone bus, for prefetching, and a slave Wishbone bus, for
 * retrieving the prefetched data (and both buses are Wishbone SPEC B4). Data
 * is buffered locally using Xilinx block SRAM's.
 * 
 * The data-prefetches are broken up into multiple block-transfers, of the
 * parameterised sizes, so that data blocks can be sequentially-fetched from
 * multiple, similar devices.
 * 
 * NOTE:
 *  + currently synchronous, and both bus interfaces share the same clock;
 *  + it would be unwise to change the input `count_i` value while this module
 *    is active;
 *  + built to Wishbone B4 SPEC Pipelined BURST READS & WRITES;
 * 
 * Changelog:
 *  + 21/10/2016  --  initial file;
 * 
 * TODO:
 *  + modularise all Wishbone functionality, and then refactor this module;
 *  + parameterise the bit-widths of the both the Wishbone interfaces?
 * 
 */

module wb_prefetch_pipelined
  #(parameter WIDTH = 32,       // word bit-width (32-bit is max)
    parameter MSB   = WIDTH-1,  // word MSB
    parameter BYTES = WIDTH>>3, // Byte-select bits
    parameter SSB   = BYTES-1,  // MSB of the byte-selects
    parameter ABITS = CBITS+BBITS, // address bit-width
    parameter ASB   = ABITS-1,     // address MSB
    parameter COUNT = 24,       // <block count>
    parameter CBITS = 5,        // block-counter bits
    parameter CSB   = CBITS-1,  // block-counter MSB
    parameter BSIZE = 24,       // <word count>
    parameter BBITS = 5,        // word-counter bits
    parameter BSB   = BBITS-1,  // word-counter MSB
    parameter DELAY = 3)
   (
    input          rst_i,
    input          clk_i,

    //  Prefetcher control & status signals:
    input          begin_i,
    output reg     ready_o = 0,

    //  The "prefetch" master Wishbone-like bus interface:
    output         a_cyc_o,
    output         a_stb_o,
    output         a_we_o,
    input          a_ack_i,
    input          a_wat_i,
    input          a_err_i,
    output [ASB:0] a_adr_o,
    output [SSB:0] a_sel_o,
    input [MSB:0]  a_dat_i,
    output [MSB:0] a_dat_o,

    //  The "transmit" slave Wishbone-like bus interface:
    input          b_cyc_i,
    input          b_stb_i,
    input          b_we_i,
    output         b_ack_o,
    output         b_wat_o,
    output         b_rty_o,
    output         b_err_o,
    input [ASB:0]  b_adr_i,
    input [SSB:0]  b_sel_i,
    input [MSB:0]  b_dat_i,
    output [MSB:0] b_dat_o
    );

   reg [CSB:0]         count = 0;
   reg [CBITS:0]       count_nxt;
   reg                 count_end = 0;
   reg                 read = 0;
   wire                done;


   assign a_sel_o = {BYTES{1'b1}};


   initial begin : PREFETCH_BLOCK
      $display("\nModule : wb_prefetch\n\tWIDTH\t= %4d\n\tCOUNT\t= %4d\n\tSBITS\t= %4d\n\tBSIZE\t= %4d\n\tBBITS\t= %4d\n\tBSTEP\t= %4d\n\tUBITS\t= %4d\n", WIDTH, COUNT, SBITS, BSIZE, BBITS, BSTEP, UBITS);
   end // PREFETCH_BLOCK


   //-------------------------------------------------------------------------
   //  Prefetcher control-signals.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i || begin_i) count <= #DELAY {SBITS{1'b0}};
     else if (done)        count <= #DELAY count_nxt;

   //  Strobe the `ready_o` signal when a prefetch has been completed.
   always @(posedge clk_i)
     if (rst_i || begin_i) ready_o <= #DELAY 0;
     else                  ready_o <= #DELAY count_end && done;

   //  Pipeline these signals, since the block-prefetches take several clock-
   //  cycles to complete.
   always @(posedge clk_i) begin
      count_nxt <= #DELAY count + BSIZE;
      count_end <= #DELAY !begin_i && count_nxt >= COUNT;
   end


   //-------------------------------------------------------------------------
   //  Port A master Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   reg [ASB:0]         a_adr = {ABITS{1'b0}};
   wire [ABITS:0]      a_adr_nxt = a_adr + 1;
   wire [BSB:0]        a_blk;

   assign a_adr_o = {a_adr, a_blk};
   assign a_dat_o = {WIDTH{1'bx}};

   //-------------------------------------------------------------------------
   //  Keep prefetching blocks, until at least `COUNT` words have been
   //  transferred.
   always @(posedge clk_i)
     if (rst_i)
       {a_adr, read} <= #DELAY {(UBITS+1){1'b0}};
     else if (begin_i)
       {a_adr, read} <= #DELAY {{UBITS{1'b0}}, 1'b1};
     else if (done && count_nxt < COUNT)
       {a_adr, read} <= #DELAY {a_adr_nxt[USB:0], 1'b1};
     else
       {a_adr, read} <= #DELAY {a_adr, 1'b0};


   //-------------------------------------------------------------------------
   //  Port B master Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   //  Port B state and bus-control signals.
   reg [BSB:0]         b_blk = 0;
   wire [BBITS:0]      b_nxt = b_blk + 1;
   wire                b_run = a_cyc_o && a_ack_i && !b_cyc_o;
   wire                b_cyc = b_stb_o || !b_ack_i;
   wire                b_stb;

   assign b_we_o = b_cyc_o;

   always @(posedge clk_i)
     if (rst_i)
       {b_cyc_o, b_stb_o} <= #DELAY 2'b00;
     else if (b_run)
       {b_cyc_o, b_stb_o} <= #DELAY 2'b11;
     else if (b_cyc_o)
       {b_cyc_o, b_stb_o} <= #DELAY {b_cyc, b_stb};
     else
       {b_cyc_o, b_stb_o} <= #DELAY 2'b00;

   //-------------------------------------------------------------------------
   //  Address generation.
   wire [SBITS:0]      b_adr_next = b_adr_o + 1;

   always @(posedge clk_i)
     if (rst_i || begin_i)
       b_adr_o <= #DELAY {SBITS{1'b0}};
     else if (b_cyc_o && (!b_wat_i || !b_cyc))
       b_adr_o <= #DELAY b_adr_next[ASB:0];

   //  Transfer incoming data to the output bus.
   always @(posedge clk_i)
     if (a_ack_i) b_dat_o <= #DELAY a_dat_i;

   //-------------------------------------------------------------------------
   //  Block-counter, for computing burst transfers.
   always @(posedge clk_i)
     if (rst_i || b_run) b_blk <= #DELAY {BBITS{1'b0}};
     else                b_blk <= #DELAY b_bst_o ? b_nxt[BSB:0] : b_blk;


   //-------------------------------------------------------------------------
   //  Address-generation unit, for each block.
   //-------------------------------------------------------------------------
   wb4_get_block
     #(.BSIZE(BSIZE), .BBITS(BBITS), .DELAY(DELAY)
       ) FETCH0
       ( .clk_i(clk_i),
         .rst_i(rst_i),
         .cyc_o(a_cyc_o),
         .stb_o(a_stb_o),
         .we_o (a_we_o),
         .ack_i(a_ack_i),
         .wat_i(a_wat_i),
         .rty_i(a_rty_i),
         .err_i(a_err_i),
         .adr_o(a_blk),

         .read_i(read),
         .done_o(done)
         );


   //-------------------------------------------------------------------------
   //  Wishbone to SRAM interface for storing the prefetched visibilities.
   //-------------------------------------------------------------------------
   wb_sram_interface #( .WIDTH(WIDTH), .ABITS(9) ) WB0
     ( .clk_i(clk_i),
       .rst_i(rst_i),
       .cyc_i(a_cyc_o),
       .stb_i(a_ack_i),
       .we_i (a_we_o),
       .ack_o(),
       .wat_o(),
       .rty_o(),
       .err_o(),
       .adr_i(s_adr),
       .sel_i(4'b1111),
       .dat_i(a_dat_i),
       .dat_o(a_dat_o),

       .sram_ce_o (sram_a_ce),
       .sram_we_o (),
       .sram_bes_o(sram_a_bes),
       .sram_adr_o(sram_a_adr),
       .sram_dat_i({WIDTH{1'b0}}),
       .sram_dat_o(wb_to_sram)
       );


   //-------------------------------------------------------------------------
   //  Wishbone to SRAM interface for retrieving the prefetched visibilities.
   //-------------------------------------------------------------------------
   wb_sram_interface #( .WIDTH(8), .ABITS(11) ) WB1
     ( .clk_i(clk_i),
       .rst_i(rst_i),
       .cyc_i(b_cyc_i),
       .stb_i(b_stb_i),
       .we_i (b_we_i),
       .ack_o(b_ack_o),
       .wat_o(b_wat_o),
       .rty_o(b_rty_o),
       .err_o(b_err_o),
       .adr_i(s_adr),
       .sel_i(4'b1111),
       .dat_i(a_dat_i),
       .dat_o(a_dat_o),

       .sram_ce_o (sram_b_ce),
       .sram_we_o (sram_b_we),
       .sram_bes_o(),
       .sram_adr_o(sram_b_adr),
       .sram_dat_i({WIDTH{1'b0}}),
       .sram_dat_o(wb_to_sram)
       );


   //-------------------------------------------------------------------------
   //  SRAM for the prefetched visibilities.
   //-------------------------------------------------------------------------
   RAMB16X32X8_TDP
     #(.DELAY(3)) SRAM0
       (.CLKA (clk_i),
        .ENA  (sram_a_ce),
        .WEA  (sram_a_bes),
        .ADDRA(sram_a_adr),
        .DIA  (wb_to_sram),
        .DOA  (),

        .CLKB (clk_i),
        .ENB  (sram_b_ce),
        .WEB  (sram_b_we),
        .ADDRB(sram_b_adr),
        .DIB  ({WIDTH{1'b0}}),
        .DOB  (sram_to_wb)
        );


`ifdef __icarus
   //-------------------------------------------------------------------------
   //  Count the number of prefetched words.
   //-------------------------------------------------------------------------
   integer             rxd = 0;

   always @(posedge clk_i)
     if (rst_i || begin_i) rxd <= #DELAY 0;
     else if (a_cyc_o && a_ack_i) rxd <= #DELAY rxd+1;
`endif //  `ifdef __icarus


endmodule // wb_prefetch_pipelined
