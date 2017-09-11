`timescale 1ns/100ps
/*
 * Module      : rtl/wb_sram_stream.v
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
 * Simple data-streaming circuit, connecting to a SRAM via a Wishbone (SPEC
 * B4) interconnect.
 * The purpose of the module is to stream out data (from a block device; e.g.,
 * a SRAM), with the sequential addresses automatically generated per access.
 * 
 * NOTE:
 *  + supports classic, pipelined, and burst transfers;
 * 
 * Changelog:
 *  + 29/10/2016  --  initial file;
 * 
 * TODO:
 * 
 */

/*
 Instantiation template:
 
    wb_sram_stream
     #(  .WIDTH(DATA), // Data bit-width
         .WORDS(WNUM), // Number of words to stream
         .WBITS(BITS), // Word-counter bit-width
         .RESET(1),    // Resettable address-counter?
         .START(0),    // Start address
         .LAST (LAST), // Last address-counter value
         .STEP (1),    // Address step-size
         .TICKS(1),    // Read latency of the attached SRAM
         .READ (1),    // Support streaming read (0/1)?
         .WRITE(0),    // Support streaming writes as well (0/1)?
         .USEBE(0),    // Use individual byte-enables (0/1)?
         .BYTES(1),    // Number of byte-enables
         .PIPED(1),    // Pipelined Wishbone (SPEC B4) transfers (0/1)?
         .ASYNC(1),    // Combinational vs. synchronous (0/1/2)?
         .CHECK(1),    // Sanity-checking of inputs (0/1)?
         .DELAY(3)     // 3ns simulation combinational delay
         ) SRAM_PORT
       ( .clk_i(clk),
         .rst_i(rst),

         .cyc_i(cyc),
         .stb_i(stb),
         .we_i (we),
         .ack_o(ack),
         .wat_o(wat),
         .rty_o(rty),
         .err_o(err),
         .sel_i(sel),
         .dat_i(din),
         .dat_o(dout),

         .sram_ce_o(sram_ce),
         .sram_we_o(sram_we),
         .sram_be_o(sram_be),
         .sram_ad_o(sram_ad),
         .sram_do_i(sram_do),
         .sram_di_o(sram_di),
 
         .wrapped_o(wrapped)
         );
*/

module wb_sram_stream
  #(//  Parameters for the bus bit-widths:
    parameter WIDTH = 8,
    parameter MSB   = WIDTH-1,
    parameter WBITS = 10,
    parameter WORDS = 1 << WBITS,
    parameter ASB   = WBITS-1,

    //  Counter parameters:
    parameter RESET = 1,        // Resettable address counter?
    parameter START = {WIDTH{1'b0}},
    parameter LAST  = START+WORDS-1,
    parameter STEP  = 1,

    //  SRAM-feature parameters:
    parameter TICKS = 1,        // SRAM read latency (in cycles)
    parameter READ  = 1,        // Support (and count) reads (0/1)?
    parameter WRITE = 0,        // Support (and count) writes (0/1)?
    parameter USEBE = 0,        // Use individual byte-enables (0/1)?
    parameter BYTES = WIDTH>>3, // Number of byte-enables
    parameter SSB   = BYTES-1,  // MSB of the byte-enables

    //  Wishbone bus mode parameters:
    parameter ASYNC = 1,        // combinational control signals (0/1/2)?
    parameter PIPED = 1,        // pipelined (SPEC B4) transfers (0/1)?
    parameter CHECK = 0,        // TODO: extra sanity-checking (0/1)?

    //  Simulation-only parameters:
    parameter NOISY = 0,       // display extra debug info?
    parameter DELAY = 3)       // simulated combinational delay (ns)
   (input          clk_i,
    input          rst_i,

    //  Streaming/serial, Wishbone (SPEC B4, slave) interface:
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output         ack_o,
    output         wat_o,
    output         rty_o,
    output         err_o,
    input [SSB:0]  sel_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    //  Prefetching, SRAM interface:
    output         sram_ce_o,
    output         sram_we_o,
    output [ASB:0] sram_ad_o,
    output [SSB:0] sram_be_o,
    input [MSB:0]  sram_do_i,
    output [MSB:0] sram_di_o,

    output         wrapped_o
    );


   reg [ASB:0]     adr = START;
   wire            cyc_w, stb_w, ack_w, inc_w, wrap_adr;
   wire [WBITS:0]  next_adr;
   reg             wrap = 1'b0, wrapped = 1'b0;


   //-------------------------------------------------------------------------
   //  Signal whenever the address-counter wraps.
   assign wrapped_o = wrap && !wrapped;

   //-------------------------------------------------------------------------
   //  Generate the signals that handle the addresses.
   assign cyc_w    = CHECK ? cyc_i : 1'b1;
   assign stb_w    = cyc_w && stb_i;
   assign ack_w    = PIPED ? stb_w && !wat_o : ack_o;
   assign inc_w    = cyc_w && ack_w;

   assign wrap_adr = adr == LAST;
   assign next_adr = wrap_adr ? START : adr + STEP;


   //-------------------------------------------------------------------------
   //  Address generation.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i && RESET)
       adr <= #DELAY START;
     else if (inc_w && !wrap_adr)
       adr <= #DELAY next_adr[ASB:0];
     else
       adr <= #DELAY adr;

   //  Strobe the `wrap` signal after each complete transfer.
   always @(posedge clk_i)
     if (rst_i && (CHECK || RESET))
       wrap <= #DELAY 1'b0;
     else if (inc_w)
       wrap <= #DELAY wrap_adr;
     else
       wrap <= #DELAY wrap;

   //  Assert output `wrapped` after the bus transaction completes.
   always @(posedge clk_i)
     if (rst_i && (CHECK || RESET))
       wrapped <= #DELAY 1'b0;
     else
       wrapped <= #DELAY wrap;


   //-------------------------------------------------------------------------
   //  Wishbone to SRAM interface.
   //-------------------------------------------------------------------------
   wb_sram_port
     #(  .WIDTH(WIDTH), // Data and address bit-widths
         .ABITS(WBITS),
         .USEBE(USEBE), // Use individual byte-enables (0/1)?
         .BYTES(BYTES),
         .TICKS(TICKS), // SRAM latency
         .READ (READ ), // Streaming reads?
         .WRITE(WRITE), // Streaming writes?
         .PIPED(PIPED), // Pipelined Wishbone (SPEC B4) transfers (0/1)?
         .ASYNC(0),     // Combinational vs. synchronous (0/1/2)?
         .CHECK(CHECK), // Sanity-checking of inputs (0/1)?
         .DELAY(DELAY)
         ) SRAMWB
       ( .clk_i(clk_i),
         .rst_i(rst_i),

         .cyc_i(cyc_i),
         .stb_i(stb_i),
         .we_i (WRITE ? we_i : 1'b0),
         .ack_o(ack_o),
         .wat_o(wat_o),
         .rty_o(rty_o),
         .err_o(err_o),
         .adr_i(adr),
         .sel_i(USEBE ? sel_i : {BYTES{1'b0}}),
         .dat_i(WRITE ? dat_i : {WIDTH{1'b0}}),
         .dat_o(dat_o),

         .sram_ce_o(sram_ce_o),
         .sram_we_o(sram_we_o),
         .sram_be_o(sram_be_o),
         .sram_ad_o(sram_ad_o),
         .sram_do_i(sram_do_i),
         .sram_di_o(sram_di_o)
         );


   //-------------------------------------------------------------------------
   //  Additional debug/configuration output.
   //-------------------------------------------------------------------------
   initial begin
      if (NOISY)
        $display("\nModule : wb_sram_stream (%m)\n\tWIDTH\t= %4d\n\tWORDS\t= %4d\n\tWBITS\t= %4d\n\tSTART\t= %4d\n\tSTEP\t= %4d\n\tLAST\t= %4d\n", WIDTH, WORDS, WBITS, START, STEP, LAST);
   end


endmodule // wb_sram_stream
