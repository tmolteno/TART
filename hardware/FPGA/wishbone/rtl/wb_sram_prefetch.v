`timescale 1ns/100ps
/*
 * Module      : rtl/wb_sram_prefetch.v
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
 *  + built to support Wishbone B4 SPEC Pipelined BURST READS & WRITES;
 * 
 * Changelog:
 *  + 29/10/2016  --  initial file;
 * 
 * TODO:
 * 
 */

/*
 Instantiation template:
 
    wb_sram_prefetch
     #(  .WIDTH(DATA), // Data and address bit-widths
         .ABITS(ADDR),
         .TICKS(1),    // Read latency of the attached SRAM
         .READ (1),    // Support SRAM reads (0/1)?
         .WRITE(1),    // Support SRAM writes (0/1)?
         .USEBE(1),    // Use individual byte-enables (0/1)?
         .BYTES(1),    // Number of byte-enables
         .PIPED(1),    // Pipelined Wishbone (SPEC B4) transfers (0/1)?
         .ASYNC(1),    // Combinational vs. synchronous (0/1/2)?
         .CHECK(1),    // Sanity-checking of inputs (0/1)?
         .DELAY(3)     // 3ns simulation combinational delay
         ) SRAM_PORT
       ( .clk_i(clk),
         .rst_i(rst),

         .begin_i(start), // Begins a prefetch
         .ready_o(ready), // Strobes once complete
 
         .cyc_i(cyc),
         .stb_i(stb),
         .we_i (we),
         .ack_o(ack),
         .wat_o(wat),
         .rty_o(rty),
         .err_o(err),
         .adr_i(adr),
         .sel_i(sel),
         .dat_i(din),
         .dat_o(dout),

         .sram_ce_o(sram_ce),
         .sram_we_o(sram_we),
         .sram_be_o(sram_be),
         .sram_ad_o(sram_ad),
         .sram_do_i(sram_do),
         .sram_di_o(sram_di)
         );
*/

module wb_sram_prefetch
  #(// Wishbone bus parameters:
    parameter WIDTH = 32,       // word bit-width (32-bit is max)
    parameter MSB   = WIDTH-1,  // word MSB
    parameter USEBE = 1,        // generate (write) byte-enables (0/1)?
    parameter BYTES = WIDTH>>3, // Byte-select bits
    parameter SSB   = BYTES-1,  // MSB of the byte-selects
    parameter ABITS = CBITS+BBITS, // address bit-width
    parameter ASB   = ABITS-1,     // address MSB

    //  Prefetch-blocks parameters:
    parameter COUNT = 24,       // blocks per complete prefetch
    parameter CBITS = 5,        // block-counter bits
    parameter CSB   = CBITS-1,  // block-counter MSB
    parameter CMAX  = COUNT-1,  // maximum (block-)counter value

    //  Block-size parameters:
    parameter BSIZE = 24,       // words/block
    parameter BBITS = 5,        // word-counter bits
    parameter BSB   = BBITS-1,  // word-counter MSB
    parameter BMAX  = BSIZE-1,  // maximum (word-)counter value

    //  Wishbone mode/settings parameters:
    parameter PIPED = 1,        // SRAM supports pipelined transfers (0/1)?
    parameter CHECK = 1,        // sanity-check when sharing a bus (0/1)?

    //  Simulation-only parameters:
    parameter NOISY = 0,        // display extra debug info?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input          clk_i,
    input          rst_i,

    //  Prefetcher control & status signals:
    input          begin_i,
    output reg     ready_o = 1'b0,

    //  The prefetching, Wishbone (master, SPEC B4) bus interface:
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

    //  Interface to the buffering SRAM:
    output         sram_ce_o,
    output         sram_we_o,
    output [ASB:0] sram_ad_o,
    output [SSB:0] sram_be_o,
    input [MSB:0]  sram_do_i,
    output [MSB:0] sram_di_o
    );


   //-------------------------------------------------------------------------
   //  Prefetcher signal definitions.
   //-------------------------------------------------------------------------
   //  Block-fetcher (lower-address) control signals.
   reg             read = 1'b0;
   wire            done;
   wire [BSB:0]    lower;

   //  Block (upper-address) counter.
   reg [CSB:0]     upper = {CBITS{1'b0}};
   reg [CSB:0]     upper_nxt;
   reg             upper_end = 1'b0;
   wire [CBITS:0]  upper_inc = upper + 1;

   //  Local address counter.
   //  NOTE: This is also the SRAM write-address; therefore, it lags the above
   //    counters/addresses.
   reg [ASB:0]     count = {ABITS{1'b0}};
   wire [ABITS:0]  count_inc = count + 1;

   //  Fetch signals.
   wire            f_cyc, f_stb, f_we, f_ack;
   wire [ASB:0]    f_adr;
   wire [SSB:0]    f_sel;
   wire [MSB:0]    f_dat;

   //  Internal Wishbone signals.
   wire            cyc_w, stb_w, ack_w;


   //-------------------------------------------------------------------------
   //  Additional Wishbone interface signals.
   //-------------------------------------------------------------------------
   //  External (master) Wishbone interface signals.
   assign adr_o = {upper, lower};
   assign sel_o = {BYTES{1'b1}};

   //  Internal (fetch) Wishbone interface signals.
   assign f_cyc = cyc_o;
   assign f_stb = ack_i;
   assign f_we  = cyc_o;
   assign f_adr = count;
   assign f_sel = {BYTES{1'b1}};
   assign f_dat = dat_i;

   //  Determine when a command has been received.
   assign cyc_w = CHECK ? cyc_o : 1'b1;
   assign stb_w = cyc_w && stb_o;
   assign ack_w = PIPED ? cyc_w && ack_i : stb_w && ack_i;


   //-------------------------------------------------------------------------
   //  Block address.
   //-------------------------------------------------------------------------
   //  Increment the block-counter after each block has been prefetched.
   always @(posedge clk_i)
     if (begin_i)
       upper <= #DELAY {CBITS{1'b0}};
     else if (done)
       upper <= #DELAY upper_nxt;

   //  Pipeline these signals, since the block-prefetches take several clock-
   //  cycles to complete.
   always @(posedge clk_i)
     begin
        upper_nxt <= #DELAY upper_inc[CSB:0];
        upper_end <= #DELAY !begin_i && upper_nxt == COUNT;
     end


   //-------------------------------------------------------------------------
   //  Strobe the `ready_o` signal when a prefetch has been completed.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i || begin_i)
       ready_o <= #DELAY 1'b0;
     else
       ready_o <= #DELAY upper_end && done;


   //-------------------------------------------------------------------------
   //  Local SRAM address/counter.
   //-------------------------------------------------------------------------
   //  Increment the counter after each word has been transferred over the
   //  Wishbone master interface.
   always @(posedge clk_i)
     if (begin_i)
       count <= #DELAY {ABITS{1'b0}};
     else if (ack_w)
       count <= #DELAY count_inc[ASB:0];


   //-------------------------------------------------------------------------
   //  Address-generation unit, for each block.
   //-------------------------------------------------------------------------
`ifdef  __USE_ASYNC_FETCH
   wire read_s = read_w;
`else
   wire read_s = read;
`endif
   wire read_w = begin_i || done && !upper_end;

   always @(posedge clk_i)
     read <= #DELAY read_w;


   //-------------------------------------------------------------------------
   //  Wishbone pipelined BURST READS functional unit.
   //-------------------------------------------------------------------------
   wb_fetch
     #(  .FETCH(BSIZE),
         .FBITS(BBITS),
         .DELAY(DELAY)
         ) FETCH0
       ( .clk_i(clk_i),
         .rst_i(rst_i),

         .fetch_i(read_s),
         .ready_o(done),

         .cyc_o(cyc_o),         // Drives the external Wishbone interface
         .stb_o(stb_o),
         .we_o (we_o),
         .ack_i(ack_i),
         .wat_i(wat_i),
         .rty_i(rty_i),
         .err_i(err_i),
         .adr_o(lower)
         );


   //-------------------------------------------------------------------------
   //  Wishbone to SRAM interface for storing the prefetched data.
   //-------------------------------------------------------------------------
   wb_sram_port
     #(  .WIDTH(WIDTH),
         .ABITS(ABITS),
         .TICKS(1),             // ignored, as write-only
         .READ (0),
         .WRITE(1),
         .USEBE(USEBE),
         .BYTES(BYTES),
         .PIPED(1),
         .ASYNC(0),             // synchronous ACK
         .CHECK(0),
         .DELAY(DELAY)
         ) SRAMWB
       ( .clk_i(clk_i),
         .rst_i(rst_i),
         .cyc_i(f_cyc),
         .stb_i(f_stb),
         .we_i (f_we),
         .ack_o(f_ack),
         .wat_o(),
         .rty_o(),
         .err_o(),
         .adr_i(f_adr),
         .sel_i(f_sel),
         .dat_i(f_dat),
         .dat_o(dat_o),

         .sram_ce_o(sram_ce_o),
         .sram_we_o(sram_we_o),
         .sram_be_o(sram_be_o),
         .sram_ad_o(sram_ad_o),
         .sram_do_i(sram_do_i),
         .sram_di_o(sram_di_o)
         );


`ifdef __icarus
   //-------------------------------------------------------------------------
   //  Debug information.
   //-------------------------------------------------------------------------
   initial begin
      if (NOISY)
        $display("\nModule : wb_sram_prefetch (%m)\n\tWIDTH\t= %4d\n\tBYTES\t= %4d\n\tABITS\t= %4d\n\tCOUNT\t= %4d\n\tCBITS\t= %4d\n\tBSIZE\t= %4d\n\tBBITS\t= %4d\n", WIDTH, BYTES, ABITS, COUNT, CBITS, BSIZE, BBITS);
   end


   //-------------------------------------------------------------------------
   //  Count the number of prefetched words.
   //-------------------------------------------------------------------------
   integer             rxd = 0;

   always @(posedge clk_i)
     if (rst_i || begin_i)
       rxd <= #DELAY 0;
     else if (ack_w)
       rxd <= #DELAY rxd+1;
`endif //  `ifdef __icarus


endmodule // wb_sram_prefetch
