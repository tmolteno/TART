`timescale 1ns/100ps
/*
 * Module      : verilog/correlator/block_SDP.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
 * 
 * Time-multiplexed block of correlator-blocks.
 * 
 * NOTE:
 *  + typically several of these would be attached to a common set of antenna
 *    and a system bus;
 *  + this module doesn't include the required control circuitry;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM, and carry-
 *    chain resources;
 * 
 * Changelog:
 *  + 28/10/2016  --  initial file (rebuilt from `correlator_block_SDP.v`);
 * 
 * TODO:
 * 
 */

module block_SDP
  #(//  Data and visibilities parparameters:
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
    parameter ABITS = TBITS+XBITS, // External I/O address bits
    parameter ASB   = ABITS-1,

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_x, // correlator clock

    //  Signals from the block-control module:
    input [XSB:0]  bank_i,

    input [TSB:0]  x_rd_adr_i,
    input [TSB:0]  x_wr_adr_i,

    input [TSB:0]  y_rd_adr_i,
    input [TSB:0]  y_wr_adr_i,

    // Real and imaginary components from the antennas:
    input          sw_i, // switch banks
    input          en_i, // data is valid
    input [ISB:0]  re_i, // real component of imput
    input [ISB:0]  im_i, // imaginary component of input

    //  SRAM interface for reading back the visibilities:
    input          sram_clk_i,
    input          sram_ce_i,
    input [ASB:0]  sram_adr_i,
    output [QSB:0] sram_dat_o
    );


   wire            write;
   wire [WSB:0]    vis0, vis1, vis2, vis3;
   wire [QSB:0]    vis;


   assign vis = {vis3, vis2, vis1, vis0};


   //-------------------------------------------------------------------------
   //  Correlator instances.
   //-------------------------------------------------------------------------
   correlator_SDP
     #(  .ACCUM(ACCUM),
         .SUMHI(0),
         .TBITS(TBITS),
         .PAIRS(PAIRS0),
         .DELAY(DELAY)
         ) CORRELATOR0
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

   correlator_SDP
     #(  .ACCUM(ACCUM),
         .SUMHI(0),
         .TBITS(TBITS),
         .PAIRS(PAIRS1),
         .DELAY(DELAY)
         ) CORRELATOR1
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

   correlator_SDP
     #(  .ACCUM(ACCUM),
         .SUMHI(0),
         .TBITS(TBITS),
         .PAIRS(PAIRS2),
         .DELAY(DELAY)
         ) CORRELATOR2
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

   correlator_SDP
     #(  .ACCUM(ACCUM),
         .SUMHI(1),             // also count #ones
         .TBITS(TBITS),
         .PAIRS(PAIRS3),
         .DELAY(DELAY)
         ) CORRELATOR3
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
   //  Explicit instantiation, because explicitly placing them is needed to
   //  consistently meet timing (and XST sometimes gets the primitive wrong).
   //-------------------------------------------------------------------------
   RAMB8X32_SDP #(.DELAY(DELAY)) VISRAM [NSRAM-1:0]
     ( //  Write port.
       .WCLK (clk_x),
       .WE   (write),
       .WADDR({bank_i, x_wr_adr_i}),
       .DI   (vis),

       //  Read port.
       .RCLK (sram_clk_i),
       .CE   (sram_ce_i),
       .RADDR(sram_adr_i),
       .DO   (sram_dat_o)
       );


endmodule // block_SDP
