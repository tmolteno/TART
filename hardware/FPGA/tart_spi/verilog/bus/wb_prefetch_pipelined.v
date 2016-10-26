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
  #(// Wishbone bus parameters:
    parameter WIDTH = 32,       // word bit-width (32-bit is max)
    parameter MSB   = WIDTH-1,  // word MSB
    parameter BYTES = WIDTH>>3, // Byte-select bits
    parameter SSB   = BYTES-1,  // MSB of the byte-selects
    parameter ABITS = CBITS+BBITS, // address bit-width
    parameter ASB   = ABITS-1,     // address MSB
    parameter ESB   = ASB+2,       // address MSB for byte-wide access

    //  Prefetcher, block-size parameters:
    parameter COUNT = 24,       // blocks per complete prefetch
    parameter CBITS = 5,        // block-counter bits
    parameter CSB   = CBITS-1,  // block-counter MSB
    parameter CMAX  = COUNT-1,  // maximum (block-)counter value
    parameter BSIZE = 24,       // words/block
    parameter BBITS = 5,        // word-counter bits
    parameter BSB   = BBITS-1,  // word-counter MSB
    parameter BMAX  = BSIZE-1,  // maximum (word-)counter value

    parameter DELAY = 3)
   (
    input          clk_i,
    input          rst_i,

    //  Prefetcher control & status signals:
    input          begin_i,
    output reg     ready_o = 1'b0,

    //  The "prefetch" master Wishbone-like bus interface:
    output         a_cyc_o,
    output         a_stb_o,
    output         a_we_o,
    input          a_ack_i,
    input          a_wat_i,
    input          a_rty_i,
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
    input [ESB:0]  b_adr_i,
    input [7:0]    b_dat_i,
    output [7:0]   b_dat_o
    );


   //-------------------------------------------------------------------------
   //  Prefetcher signal definitions.
   //-------------------------------------------------------------------------
   //  Block-fetcher (lower-address) control signals.
   reg                 read = 1'b0;
   wire                done;
   wire [BSB:0]        lower;

   //  Block (upper-address) counter.
   reg [CSB:0]         block = {CBITS{1'b0}};
   reg [CSB:0]         block_nxt;
   reg                 block_end = 1'b0;
   wire [CBITS:0]      block_inc = block + 1;

   //  Local address counter.
   reg [ASB:0]         count = {ABITS{1'b0}};
   wire [ABITS:0]      count_inc = count + 1;

   //  SRAM signals.
   wire                sram_a_ce, sram_b_ce, sram_b_we;
   wire [3:0]          sram_a_bes;
   wire [9:0]          sram_a_adr;
   wire [11:0]         sram_b_adr;
   wire [31:0]         wb_to_sram;
   wire [7:0]          sram_to_wb;


   //-------------------------------------------------------------------------
   //  Additional Wishbone master interface signals.
   //-------------------------------------------------------------------------
   assign a_adr_o = {block, lower};
   assign a_sel_o = {BYTES{1'b1}};
   assign a_dat_o = dat_q[MSB:0];


   //-------------------------------------------------------------------------
   //  Block address.
   //-------------------------------------------------------------------------
   //  Increment the blocker after each block has been prefetched.
   always @(posedge clk_i)
     if (begin_i)
       block <= #DELAY {CBITS{1'b0}};
     else if (done)
       block <= #DELAY block_nxt;

   //  Pipeline these signals, since the block-prefetches take several clock-
   //  cycles to complete.
   always @(posedge clk_i)
     begin
        block_nxt <= #DELAY block_inc[CSB:0];
        block_end <= #DELAY !begin_i && block_nxt == COUNT;
     end


   //-------------------------------------------------------------------------
   //  Strobe the `ready_o` signal when a prefetch has been completed.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i || begin_i)
       ready_o <= #DELAY 1'b0;
     else
       ready_o <= #DELAY block_end && done;


   //-------------------------------------------------------------------------
   //  Local SRAM address/counter.
   //-------------------------------------------------------------------------
   //  Increment the counter after each word has been transferred over the
   //  Wishbone master interface.
   always @(posedge clk_i)
     if (begin_i)
       count <= #DELAY {ABITS{1'b0}};
     else if (a_cyc_o && a_ack_i)
       count <= #DELAY count_inc[ASB:0];


   //-------------------------------------------------------------------------
   //  Address-generation unit, for each block.
   //-------------------------------------------------------------------------
   wire read_w = begin_i || done && !block_end;

   always @(posedge clk_i)
     if (begin_i || done && !block_end)
       read <= #DELAY 1'b1;
     else
       read <= #DELAY 1'b0;


   //-------------------------------------------------------------------------
   //  Wishbone pipelined BURST READS functional unit.
   //-------------------------------------------------------------------------
   wb_fetch
     #(  .FETCH(BSIZE), .FBITS(BBITS), .DELAY(DELAY)
         ) FETCH0
       ( .clk_i(clk_i),
         .rst_i(rst_i),

`ifdef  __USE_ASYNC_FETCH
         .fetch_i(read_w),
`else
         .fetch_i(read),
`endif
         .ready_o(done),

         .cyc_o(a_cyc_o),
         .stb_o(a_stb_o),
         .we_o (a_we_o),
         .ack_i(a_ack_i),
         .wat_i(a_wat_i),
         .rty_i(a_rty_i),
         .err_i(a_err_i),
         .adr_o(lower)
         );


   //-------------------------------------------------------------------------
   //  Wishbone to SRAM interface for storing the prefetched data.
   //-------------------------------------------------------------------------
   //  NOTE: The maximum supported bit-width is 32-bits, to port A of the
   //    SRAM, and only storing upto 1024 words.
   wire [31:0] dat_w, dat_s, dat_q;

   //  Swizzle the nibbles.
   assign dat_w = {{32-WIDTH{1'b0}}, a_dat_i};
   assign dat_s = {dat_w[31:28], dat_w[23:20], dat_w[15:12], dat_w[7:4],
                   dat_w[27:24], dat_w[19:16], dat_w[11:8] , dat_w[3:0]};

   wb_sram_interface
     #(  .WIDTH(32),
         .ABITS(10)
         ) PORTA
       ( .clk_i(clk_i),
         .rst_i(rst_i),
         .cyc_i(a_cyc_o),
         .stb_i(a_ack_i),
         .we_i (a_cyc_o),      // TODO: allowed to change, before ACK?
         .ack_o(),
         .wat_o(),
         .rty_o(),
         .err_o(),
         .adr_i(count),
         .sel_i(4'b1111),
         .dat_i(dat_s),
         .dat_o(dat_q),

         .sram_ce_o (sram_a_ce),
         .sram_we_o (),
         .sram_bes_o(sram_a_bes),
         .sram_adr_o(sram_a_adr),
         .sram_dat_i(32'hx),
         .sram_dat_o(wb_to_sram)
         );


   //-------------------------------------------------------------------------
   //  Wishbone to SRAM interface for retrieving the prefetched data.
   //-------------------------------------------------------------------------
   //  NOTE: This is an 8-bit interface to port B of the SRAM.
   wb_sram_interface
     #(  .WIDTH(8),
         .ABITS(12)
         ) PORTB
       ( .clk_i(clk_i),
         .rst_i(rst_i),
         .cyc_i(b_cyc_i),
         .stb_i(b_stb_i),
         .we_i (b_we_i),
         .ack_o(b_ack_o),
         .wat_o(b_wat_o),
         .rty_o(b_rty_o),
         .err_o(b_err_o),
         .adr_i(b_adr_i),
         .sel_i(1'b1),
         .dat_i(b_dat_i),
         .dat_o(b_dat_o),

         .sram_ce_o (sram_b_ce),
         .sram_we_o (sram_b_we),
         .sram_bes_o(),
         .sram_adr_o(sram_b_adr),
         .sram_dat_i(sram_to_wb),
         .sram_dat_o()
         );


   //-------------------------------------------------------------------------
   //  SRAM's for the prefetched data.
   //-------------------------------------------------------------------------
   //  Stores the lower nibbles of the visibilities.
   RAMB16X16X4_TDP
     #( .DELAY(3)) SRAM0
       (.CLKA (clk_i),
        .ENA  (sram_a_ce),
        .WEA  (sram_a_bes[1:0]),
        .ADDRA(sram_a_adr),
        .DIA  (wb_to_sram[15:0]),
        .DOA  (),

        .CLKB (clk_i),
        .ENB  (sram_b_ce),
        .WEB  (sram_b_we),
        .ADDRB(sram_b_adr),
        .DIB  (16'hx),
        .DOB  (sram_to_wb[3:0])
        );

   //  Stores the upper nibbles of the visibilities.
   RAMB16X16X4_TDP
     #( .DELAY(3)) SRAM1
       (.CLKA (clk_i),
        .ENA  (sram_a_ce),
        .WEA  (sram_a_bes[3:2]),
        .ADDRA(sram_a_adr),
        .DIA  (wb_to_sram[31:16]),
        .DOA  (),

        .CLKB (clk_i),
        .ENB  (sram_b_ce),
        .WEB  (sram_b_we),
        .ADDRB(sram_b_adr),
        .DIB  (16'hx),
        .DOB  (sram_to_wb[7:4])
        );


`ifdef __icarus
   //-------------------------------------------------------------------------
   //  Debug information.
   //-------------------------------------------------------------------------
   initial begin : PREFETCH_BLOCK
      $display("\nModule : wb_prefetch_pipelined\n\tWIDTH\t= %4d\n\tBYTES\t= %4d\n\tABITS\t= %4d\n\tCOUNT\t= %4d\n\tCBITS\t= %4d\n\tBSIZE\t= %4d\n\tBBITS\t= %4d\n", WIDTH, BYTES, ABITS, COUNT, CBITS, BSIZE, BBITS);
   end // PREFETCH_BLOCK


   //-------------------------------------------------------------------------
   //  Count the number of prefetched words.
   //-------------------------------------------------------------------------
   integer             rxd = 0;

   always @(posedge clk_i)
     if (rst_i || begin_i) rxd <= #DELAY 0;
     else if (a_cyc_o && a_ack_i) rxd <= #DELAY rxd+1;
`endif //  `ifdef __icarus


endmodule // wb_prefetch_pipelined
