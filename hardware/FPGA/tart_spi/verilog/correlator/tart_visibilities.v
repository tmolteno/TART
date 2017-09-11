`timescale 1ns/100ps
/*
 * Module      : verilog/bus/tart_visibilities.v
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
 *  + 29/10/2016  --  rebuilt to Wishbone SPEC B4, and with external SRAM's;
 * 
 * TODO:
 *  + finish the checksums functionality;
 * 
 */

`include "tartcfg.v"

module tart_visibilities
  #(//  Visibilities-prefetch parameters:
    parameter COUNT = 24,       // blocks per prefetch
    parameter CBITS = 5,        // block-counter bit-width
    parameter BSIZE = 24,       // words/block
    parameter BBITS = 5,        // word-counter bits

    //  Address and data bit-widths:
    parameter WIDTH = 32,
    parameter MSB   = WIDTH-1,
    parameter BYTES = WIDTH>>3,
    parameter SSB   = BYTES-1,
    parameter ABITS = CBITS+BBITS,
    parameter ASB   = ABITS-1,
    parameter XBITS = 4,        // number of banks of visibilities

    //  Wishbone parameters:
    parameter PIPED = 1,
    parameter CHECK = 1,
    parameter RESET = 0,

    //  Simulation-only parameters:
    parameter NOISY = 0,       // display extra debug info?
    parameter DELAY = 3)       // simulated combinational delay (ns)
   (
    input          clk_i, // bus clock & reset
    input          rst_i,

    //  Wishbone (master, SPEC B4) interface for prefetching visibilities:
    output         cyc_o,
    output         stb_o,
    output         we_o,
    input          ack_i,
    input          wat_i,
    input          rty_i,
    input          err_i,
    output [ASB:0] adr_o,
    output [SSB:0] sel_o,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    //  SRAM signals that are used for storing the prefetched visibilities:
    output         sram_ce_o,
    output         sram_we_o,
    output [ASB:0] sram_ad_o,
    output [SSB:0] sram_be_o,
    input [MSB:0]  sram_do_i,
    output [MSB:0] sram_di_o,

    // Status flags for the correlators and visibilities.
    input          streamed_i, // signals that a bank has been sent
    output         newblock_o, // strobed when prefetch completes
    output [MSB:0] checksum_o,

    // Correlator clock-domain signals.
    input          clk_x,
    input          switch_x,
    output reg     overflow_x = 1'b0
    );


   reg [MSB:0]     checksum = {WIDTH{1'b0}}; // TODO
   reg             empty = 1'b1, start = 1'b0;
   wire            cyc_w, ready, prefetch_empty, prefetch_full;


   //-------------------------------------------------------------------------
   //  Map to status outputs.
   assign checksum_o = checksum;
   assign newblock_o = ready;

   assign cyc_w      = CHECK ? cyc_o : 1'b1;


   //-------------------------------------------------------------------------
   //  Compute checksums, to be used to verify off-chip transfers.
   //-------------------------------------------------------------------------
   //  TODO: Just use XOR?
   always @(posedge clk_i)
//      if (switching)
     if (rst_i)
       checksum <= #DELAY {WIDTH{1'b0}};
     else if (cyc_w && ack_i)
//        checksum <= #DELAY checksum + dat_i;
       checksum <= #DELAY checksum ^ dat_i;


   //-------------------------------------------------------------------------
   //  State-machine that tracks the contents of the prefetch buffer.
   //-------------------------------------------------------------------------
   //  Prefetch buffer is empty on reset, or once all of its data has been
   //  streamed out.
   always @(posedge clk_i)
     if (rst_i || streamed_i)
       empty <= #DELAY 1'b1;
     else if (start)
       empty <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (rst_i && RESET || start)
       start <= #DELAY 1'b0;
     else if (empty && !prefetch_empty)
       start <= #DELAY 1'b1;


`ifdef __USE_OVERFLOW_DETECTION
   //-------------------------------------------------------------------------
   //  If the bank-count FIFO overflows, then assert the overflow flag.
   //-------------------------------------------------------------------------
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
   //-------------------------------------------------------------------------
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


   //-------------------------------------------------------------------------
   //  Visibilities data is prefetched from SRAM's that are local to the
   //  correlators, and buffered for fast access by the SPI interface (and
   //  chunking into bytes).
   //-------------------------------------------------------------------------
   wb_sram_prefetch
     #(  .WIDTH(WIDTH),         // bit-width of correlator data
         .BYTES(BYTES),         // the backend uses 2x 16-bit SRAM's
         .USEBE(1),             // generate individual byte-enables
         .ABITS(ABITS),         // address bit-width for prefetching
         .COUNT(COUNT),         // number of correlators
         .CBITS(CBITS),         // correlator address bit-width
         .BSIZE(BSIZE),         // words stored per correlator
         .BBITS(BBITS),         // word address bit-width
         .CHECK(CHECK),         // use extra-checking of WB signals (0/1)?
         .DELAY(DELAY)
         ) BUF0
       ( .clk_i  (clk_i),
         .rst_i  (rst_i),

         .begin_i(start),
         .ready_o(ready),

         //  Fetches the visibilities from the correlators.
         .cyc_o(cyc_o),
         .stb_o(stb_o),
         .we_o (we_o),
         .ack_i(ack_i),
         .wat_i(wat_i),
         .rty_i(rty_i),
         .err_i(err_i),
         .adr_o(adr_o),
         .sel_o(sel_o),
         .dat_i(dat_i),
         .dat_o(dat_o),

         //  Stores the visibilities in SRAM's.
         .sram_ce_o(sram_ce_o),
         .sram_we_o(sram_we_o),
         .sram_be_o(sram_be_o),
         .sram_ad_o(sram_ad_o),
         .sram_do_i(sram_do_i),
         .sram_di_o(sram_di_o)
         );


   //-------------------------------------------------------------------------
   //  Display debug/configuration information.
   //-------------------------------------------------------------------------
   initial begin
      if (NOISY)
        $display("\nModule : tart_visibilities (%m)\n\tWIDTH\t= %4d\n\tBYTES\t= %4d\n\tABITS\t= %4d\n\tCOUNT\t= %4d\n\tBSIZE\t= %4d\n", WIDTH, BYTES, ABITS, COUNT, BSIZE);
   end


endmodule // tart_visibilities
