`timescale 1ns/100ps
/*
 * Module      : verilog/bus/wb_stream.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Manages fetching visibilities data from the correlators -- transferring
 * visibilities from the correlators to a SRAM after each bank-swap.
 * 
 * NOTE:
 *  + 8-bit bus to the SPI interface, and 32-bit bus to the correlators;
 *  + the correlator bank-address (typically the upper 4-bits of `adr_i`) is
 *    passed straight through;
 *  + this is because the bank-address is controlled by `tart_acquire`;
 * 
 * Changelog:
 *  + 16/06/2016  --  initial file;
 * 
 * TODO:
 *  + redirection ROM? Or, better to do the reordering in software?
 * 
 */

`include "tartcfg.v"

module tart_visibilities
  #(parameter BLOCK = 24,
    parameter MSB   = BLOCK-1,
    parameter COUNT = 576,     // TODO: correlators and averages
    parameter ABITS = 14,
    parameter ASB   = ABITS-1,
    parameter CBITS = 10,       // visibilities address bit-width
    parameter CSB   = CBITS-1,
    parameter ESB   = CSB+2,    // MSB of address of byte-wide bus
    parameter TRATE = 12,       // time-multiplexing rate
    parameter TBITS = 4,
    parameter MSKIP = (1 << TBITS) - TRATE + 1,
    parameter RSB   = TBITS-1,
    parameter XBITS = ABITS-CBITS, // number of banks of visibilities
    parameter XSB   = XBITS-1,
    parameter DELAY = 3)
   (
    input              clk_i, // bus clock & reset
    input              rst_i,

    // Wishbone-like (slave) bus interface that connects to `tart_spi`:
    input              cyc_i,
    input              stb_i,
    input              we_i,
`ifndef __WB_SPEC_B4
    input              bst_i, // Bulk Sequential Transfer?
`endif
    output             ack_o,
    output             wat_o,
    output             rty_o,
    output             err_o,
    input [ASB+2:0]    adr_i, // Upper 4-bits are typically the bank#
    input [7:0]        byt_i, // NOTE: non-standard names, because their bit-
    output [7:0]       byt_o, //   widths aren't parameterised

    // Wishbone-like (master) bus interface that connects to the correlators:
    output             cyc_o,
    output             stb_o,
    output             we_o,
`ifndef __WB_SPEC_B4
    output             bst_o, // Bulk Sequential Transfer?
`endif
    input              ack_i,
    input              wat_i,
    input              rty_i,
    input              err_i,
    output [ASB:0]     adr_o,
    input [MSB:0]      dat_i,
    output [MSB:0]     dat_o,

    // Status flags for the correlators and visibilities.
    input              streamed,  // signals that a bank has been sent
    input              overwrite, // overwrite when buffer is full?
    input              switching, // inicates that banks have switched
    output             available, // asserted when a window is accessed
    output reg [MSB:0] checksum = 0,

    // Correlator clock-domain signals.
    input              clk_x,
    input              switch_x,
    output reg         overflow_x = 1'b0
    );

   //-------------------------------------------------------------------------
   //  The time-multiplexing ratio determines how many values are stored
   //  within each correlator. E.g., for `TRATE = 12`, there are 12 real and
   //  12 complex values within each of the correlator's SRAM's.
   parameter BSIZE = TRATE*2;
   parameter BBITS = TBITS+1;
   parameter BSB   = BBITS-1;


`ifndef __USE_FANCY_PREFETCH
   //-------------------------------------------------------------------------
   //  Dual-port SRAM's for all visibilities from the same "window."
   //-------------------------------------------------------------------------
   reg [7:0]           sram0 [0:COUNT-1];
   reg [7:0]           sram1 [0:COUNT-1];
   reg [7:0]           sram2 [0:COUNT-1];
   reg [7:0]           sram3 [0:COUNT-1];
`endif

   //-------------------------------------------------------------------------
   //  The incoming address, `adr_i`, has fields:
   //    { BANK[3:0], CORRELATOR[4:0], TIME[3:0], SIN/COS, BYTE[1:0] }
   wire [XSB:0]        bnk_w = adr_i[ASB+2:CBITS+2];
   wire [CSB:0]        adr_w;

   assign adr_o = {bnk_w, adr_w};


   //-------------------------------------------------------------------------
   //  Compute checksums, to be used to verify off-chip transfers.
   //-------------------------------------------------------------------------
   //  TODO: Just use XOR?
   always @(posedge clk_i)
//      if (switching)
     if (rst_i)
       checksum <= #DELAY {BLOCK{1'b0}};
     else if (cyc_o && ack_i)
//        checksum <= #DELAY checksum + dat_i;
       checksum <= #DELAY checksum ^ dat_i;


   //-------------------------------------------------------------------------
   //  State-machine that tracks the contents of the prefetch buffer.
   //-------------------------------------------------------------------------
   reg                 empty = 1'b1, start = 1'b0;
   wire                prefetch_empty, prefetch_full;

   //  Prefetch buffer is empty on reset, or once all of its data has been
   //  streamed out.
   always @(posedge clk_i)
     if (rst_i || streamed)
       empty <= #DELAY 1'b1;
     else if (start)
       empty <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (rst_i || start)
       start <= #DELAY 1'b0;
     else if (empty && !prefetch_empty)
       start <= #DELAY 1'b1;

`ifdef __USE_OVERFLOW_DETECTION
   //-------------------------------------------------------------------------
   //  If the bank-full FIFO overflows, then assert the overflow flag.
   always @(posedge clk_x)
     if (rst_i)
       overflow_x <= #DELAY 1'b0;
     else if (prefetch_full && switch_x)
       overflow_x <= #DELAY 1'b1;
     else
       overflow_x <= #DELAY overflow_x;
`endif //  `ifdef __USE_OVERFLOW_DETECTION

   //-------------------------------------------------------------------------
   //  Use an asynchronous FIFO's control-logic to synchronise data-ready
   //  information across clock-domains.
   afifo_gray #( .WIDTH(0), .ABITS(XBITS) ) PC0
     ( .rd_clk_i (clk_i),
       .rd_en_i  (start),
       .rd_data_o(),
       
       .wr_clk_i (clk_x),
       .wr_en_i  (switch_x),
       .wr_data_i('bx),

       .rst_i    (rst_i),
       .rempty_o (prefetch_empty),
       .wfull_o  (prefetch_full)
       );


`ifdef  __USE_FANCY_PREFETCH
   //-------------------------------------------------------------------------
   //  Prefetch the visibilities from each of the correlators, and then store
   //  them in two local SRAM's, to be read back by the TART SPI interface.
   //-------------------------------------------------------------------------
   wire [3:0]          sel_o;   // TODO:

   wb_prefetch_pipelined
     #(  .WIDTH(BLOCK),         // bit-width of correlator data
         .BYTES(4),             // the backend uses 2x 16-bit SRAM's
         .ABITS(CBITS),         // address bit-width for prefetching
         .COUNT(24),            // number of correlators
         .CBITS(5),             // correlator address bit-width
         .BSIZE(BSIZE),         // words stored per correlator
         .BBITS(BBITS),         // word address bit-width
         .DELAY(DELAY)
         ) BUF0
       ( .clk_i  (clk_i),
         .rst_i  (rst_i),

         .begin_i(start),
         .ready_o(available),

         .a_cyc_o(cyc_o),  // this port is filled by the prefetch unit
         .a_stb_o(stb_o),  // whenever the correlators perform a bank-
         .a_we_o (we_o),   // switch
         .a_ack_i(ack_i),
         .a_wat_i(wat_i),
         .a_rty_i(1'b0),        // TODO:
         .a_err_i(1'b0),
         .a_adr_o(adr_w),
         .a_sel_o(sel_o),
         .a_dat_i(dat_i),
         .a_dat_o(dat_o),

         .b_cyc_i(cyc_i),  // this port is driven by the TART SPI unit
         .b_stb_i(stb_i),
         .b_we_i (we_i),
         .b_ack_o(ack_o),
         .b_wat_o(wat_o),
         .b_rty_o(rty_o),
         .b_err_o(err_o),
         .b_adr_i(adr_i[ESB:0]),
         .b_dat_i(byt_i),
         .b_dat_o(byt_o)
         );
`else // !`ifdef __USE_FANCY_PREFETCH


   //-------------------------------------------------------------------------
   //  Visibilities data is prefetched from SRAM's that are local to the
   //  correlators, and buffered for fast access by the SPI interface.
   //-------------------------------------------------------------------------
   wire [MSB:0] p_val, p_dat;
   wire [CSB:0] p_adr;
   wire         p_cyc, p_stb, p_we, p_bst, p_ack, p_wat;

   //  Swap the LSB with the real/complex bank-select signal, so that real +
   //  complex pairs are read out together.
   assign wat_o = 1'b0;

   //  Prefetches data from the various correlators after each bank-switch,
   //  and then sends it on to a block SRAM.
`ifdef __WB_PREFETCH_CLASSIC
   wb_prefetch_classic #( .WIDTH(BLOCK), .SBITS(CBITS), .COUNT(COUNT),
`else
   wb_prefetch #( .WIDTH(BLOCK), .SBITS(CBITS), .COUNT(COUNT),
`endif
                  .BSIZE(BSIZE), .BBITS(BBITS) ) PREFETCH0
     ( .rst_i(rst_i),
       .clk_i(clk_i),

       .begin_i(start),
       .ready_o(available),

       .a_cyc_o(cyc_o),         // prefetch interface (from the correlators)
       .a_stb_o(stb_o),
       .a_we_o (we_o),
       .a_bst_o(bst_o),
       .a_ack_i(ack_i),
       .a_wat_i(wat_i),
       .a_err_i(err_i),
       .a_adr_o(adr_w),
       .a_dat_i(dat_i),
       .a_dat_o(dat_o),

       .b_cyc_o(p_cyc),         // interface between prefetcher & SRAM
       .b_stb_o(p_stb),
       .b_we_o (p_we),
       .b_bst_o(p_bst),
       .b_ack_i(p_ack),
       .b_wat_i(p_wat),
       .b_adr_o(p_adr),
       .b_dat_i(p_val),
       .b_dat_o(p_dat)
       );

   //  Buffers the visibility data until ready via the TART SPI interface.
   wire [31:0]  w_dat_o;
   assign p_val = w_dat_o[MSB:0];

   wb_sram_dual_port #( .SBITS(CBITS) ) SRAM0
     ( .rst_i(rst_i),

       .a_clk_i(clk_i),    // this port is filled by the prefetch unit
       .a_cyc_i(p_cyc),    // whenever the correlators perform a bank-
       .a_stb_i(p_stb),    // switch
       .a_we_i (p_we),
       .a_bst_i(p_bst),
       .a_ack_o(p_ack),
       .a_wat_o(p_wat),
       .a_adr_i(p_adr),
       .a_dat_i({{32-BLOCK{1'b0}}, p_dat}),
       .a_dat_o(w_dat_o),

       .b_clk_i(clk_i),    // this port is driven by the TART SPI unit
       .b_cyc_i(cyc_i),
       .b_stb_i(stb_i),
       .b_we_i (we_i),
       .b_bst_i(bst_i),
       .b_ack_o(ack_o),
       .b_wat_o(),
       .b_adr_i(adr_i[ESB:0]),
       .b_dat_i(byt_i),
       .b_dat_o(byt_o)
       );
`endif // !`ifdef __USE_FANCY_PREFETCH


   //-------------------------------------------------------------------------
   //  Display debug/configuration information.
   //-------------------------------------------------------------------------
   initial begin : VIS_PARAMS
      $display("\nModule : tart_visibilities\n\tBLOCK\t= %4d\n\tCOUNT\t= %4d\n\tABITS\t= %4d\n\tCBITS\t= %4d\n\tTRATE\t= %4d\n\tTBITS\t= %4d\n\tMSKIP\t= %4d\n", BLOCK, COUNT, ABITS, CBITS, TRATE, TBITS, MSKIP);
   end // VIS_PARAMS


endmodule // tart_visibilities
