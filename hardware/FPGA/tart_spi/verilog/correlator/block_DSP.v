`timescale 1ns/100ps
/*
 * Module      : verilog/correlator/block_DSP.v
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
 * Time-multiplexed block of correlator-blocks, and this version uses the
 * Xilinx DSP48A1 primitives (of Spartan 6's).
 * 
 * NOTE:
 *  + typically several of these would be attached to a common set of antenna
 *    and a system bus;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + the lower 3-bits of the SRAM read address select the SIN/COS# component
 *    (LSB), and the correlator to read (bits [2:1]);
 *  + SRAM read latency is 2 cycles, because the 8:1 MUX outputs are
 *    registered;
 * 
 * Changelog:
 *  + 28/10/2016  --  initial file (rebuilt from `correlator_block_DSP.v`);
 * 
 * TODO:
 * 
 */

module block_DSP
  #(//  Data and visibilities parameters:
    parameter ACCUM = 24,      // Accumulator bit-widths
    parameter MSB   = ACCUM-1,
    parameter IBITS = 24,      // Number of data sources
    parameter ISB   = IBITS-1,

    //  Pairs of antennas to correlate, for each block:
    parameter PAIRS0 = 120'h0,
    parameter PAIRS1 = 120'h0,
    parameter PAIRS2 = 120'h0,
    parameter PAIRS3 = 120'h0,

    //  Various additional data bit-widths:
    parameter WIDTH = ACCUM*2, // Combined Re & Im components
    parameter WSB   = WIDTH-1,
    parameter QBITS = WIDTH*4, // Total data-bus bit-width of SRAM
    parameter QSB   = QBITS-1,

    //  Time-multiplexing parameters:
    parameter TRATE = 12,      // Time-multiplexing rate
    parameter TBITS = 4,       // Required bit-width for TRATE counter
    parameter TSB   = TBITS-1,

    //  Visibilities banks parameter:
    parameter XBITS = 4,       // Bank address address bit-width
    parameter XSB   = XBITS-1,
    parameter NSRAM = ACCUM>>2, // #<block SRAM> for read-back

    //  SRAM address bits:
    parameter ABITS = TBITS+XBITS, // Internal address bits
    parameter ASB   = ABITS-1,     // MSB of internal address
    parameter JBITS = ABITS+3,     // External I/O address bits
    parameter JSB   = JBITS-1,     // MSB of external address

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_x, // correlator clock

    //  Signals from the block-control module:
    input [TSB:0]  x_rd_adr_i,
    input [TSB:0]  x_wr_adr_i,

    input [TSB:0]  y_rd_adr_i,
    input [TSB:0]  y_wr_adr_i,

    input [XSB:0]  bank_adr_i,

    // Real and imaginary components from the antennas:
    input          sw_i, // switch banks
    input          en_i, // data is valid
    input [ISB:0]  re_i, // real component of imput
    input [ISB:0]  im_i, // imaginary component of input

    //  SRAM interface for reading back the visibilities:
    input          sram_ck_i,
    input          sram_ce_i,
    input [JSB:0]  sram_ad_i,
    output [MSB:0] sram_do_o
    );


   wire [7:0]      waddr = {bank_adr_i, x_wr_adr_i};
   wire            write;
   wire [WSB:0]    vis0, vis1, vis2, vis3;
   wire [QSB:0]    vis;

   //  SRAM address and data signals.
   reg [2:0]       sram_sel;
   wire [ASB:0]    sram_ad_w;
   wire [QSB:0]    sram_do_w;
   wire [MSB:0]    sram_do;
   reg [MSB:0]     sram_dat;
   wire [MSB:0]    sram_d0, sram_d2, sram_d4, sram_d6;
   wire [MSB:0]    sram_d1, sram_d3, sram_d5, sram_d7;


   //-------------------------------------------------------------------------
   //  Output data assignment.
   assign sram_do_o = sram_dat;

   //  Assign the internal addresses, and device-selects.
   assign sram_ad_w = sram_ad_i[JSB:3];

   //  Data-signal assignments.
   assign {sram_d7, sram_d6, sram_d5, sram_d4,
           sram_d3, sram_d2, sram_d1, sram_d0} = sram_do_w;

   //  Visibilities assignments.
   assign vis = {vis3, vis2, vis1, vis0};


   //-------------------------------------------------------------------------
   //  Capture MUX'd data.
   always @(posedge sram_ck_i) begin
      sram_sel <= #DELAY sram_ad_i[2:0];
      sram_dat <= #DELAY sram_do;
   end


   //-------------------------------------------------------------------------
   //  Correlator instances.
   //-------------------------------------------------------------------------
   correlator_DSP
     #(  .ACCUM(ACCUM),
         .SUMHI(0),
         .TBITS(TBITS),
         .PAIRS(PAIRS0),
         .DELAY(DELAY)
         ) CORN0
       ( .clk_x(clk_x),
         .sw(sw_i),
         .en(en_i),
         .re(re_i),
         .im(im_i),
         .rd(x_rd_adr_i),
         .wr(x_wr_adr_i),

         .vld(write),
         .vis(vis0)
         );

   correlator_DSP
     #(  .ACCUM(ACCUM),
         .SUMHI(0),
         .TBITS(TBITS),
         .PAIRS(PAIRS1),
         .DELAY(DELAY)
         ) CORN1
       ( .clk_x(clk_x),
         .sw(sw_i),
         .en(en_i),
         .re(re_i),
         .im(im_i),
         .rd(x_rd_adr_i),
         .wr(x_wr_adr_i),

         .vld(),
         .vis(vis1)
         );

   correlator_DSP
     #(  .ACCUM(ACCUM),
         .SUMHI(0),
         .TBITS(TBITS),
         .PAIRS(PAIRS2),
         .DELAY(DELAY)
         ) CORN2
       ( .clk_x(clk_x),
         .sw(sw_i),
         .en(en_i),
         .re(re_i),
         .im(im_i),
         .rd(y_rd_adr_i),
         .wr(y_wr_adr_i),

         .vld(),
         .vis(vis2)
         );

   correlator_DSP
     #(  .ACCUM(ACCUM),
         .SUMHI(1),             // also count #ones
         .TBITS(TBITS),
         .PAIRS(PAIRS3),
         .DELAY(DELAY)
         ) CORN3
       ( .clk_x(clk_x),
         .sw(sw_i),
         .en(en_i),
         .re(re_i),
         .im(im_i),
         .rd(y_rd_adr_i),
         .wr(y_wr_adr_i),

         .vld(),
         .vis(vis3)
         );


   //-------------------------------------------------------------------------
   //  Explicitly instantiate an 8:1 MUX for the output-data, so that it can
   //  be floor-planned.
   //-------------------------------------------------------------------------
   MUX8 #( .WIDTH(ACCUM) ) DMUX
     ( .a(sram_d0),
       .b(sram_d1),
       .c(sram_d2),
       .d(sram_d3),
       .e(sram_d4),
       .f(sram_d5),
       .g(sram_d6),
       .h(sram_d7),
       .s(sram_sel),
       .x(sram_do)
       );


   //-------------------------------------------------------------------------
   //  Explicit instantiation, because explicitly placing them is needed to
   //  consistently meet timing (and XST sometimes gets the primitive wrong).
   //-------------------------------------------------------------------------
   RAMB8X32_SDP #(.DELAY(DELAY)) VISRAM [NSRAM-1:0]
     ( //  Write port.
       .WCLK (clk_x),
       .WE   (write),
       .WADDR(waddr),
       .DI   (vis),

       //  Read port.
       .RCLK (sram_ck_i),
       .CE   (sram_ce_i),
       .RADDR(sram_ad_w),
       .DO   (sram_do_w)
       );


endmodule // block_DSP
