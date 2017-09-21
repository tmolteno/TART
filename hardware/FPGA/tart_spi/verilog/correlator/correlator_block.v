`timescale 1ns/100ps
/*
 * Module      : verilog/correlator/correlator_block.v
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
 *  + the correlators based upon the DSP48A1 primitive require TICKS == 4,
 *    whereas the SDP version requires TICKS == 3;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as multi-
 *    port SRAM's are used;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM resources;
 * 
 * Changelog:
 *  + 01/11/2016  --  initial file (rebuilt from `block_DSP.v`);
 * 
 * TODO:
 *  + does all unneeded logic get properly pruned?
 * 
 */

module correlator_block
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
    parameter QBITS = WIDTH*4, // Total data-bus bit-width of SRAM(s)
    parameter QSB   = QBITS-1,

    //  Time-multiplexing parameters:
    parameter TRATE = 12,      // Time-multiplexing rate
    parameter TBITS = 4,       // Required bit-width for TRATE counter
    parameter TSB   = TBITS-1, // MSB of time-multiplexed registers

    //  Visibilities banks parameters:
    parameter XBITS = 4,       // Bank address address bit-width
    parameter XSB   = XBITS-1, // MSB of bank-address

    //  Correlator parameters:
    parameter NSRAM = ACCUM>>2, // #<block SRAM> for read-back
    parameter CARRY = 0,        // DSP48A2 or carry-chain adders (0/1)?
    parameter TICKS = 4-CARRY,  // RMW cycle latency (3/4)
    parameter SLOW  = 0,        // Slow clocks (0/1)?
    parameter DUPS  = CARRY,    // Duplicate high-fanout registers (0/1)?

    //  SRAM address bits:
    parameter ABITS = TBITS+XBITS, // Internal address bits
    parameter ASB   = ABITS-1,     // MSB of internal address
    parameter JBITS = ABITS+3,     // External I/O address bits
    parameter JSB   = JBITS-1,     // MSB of external address

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_x, // correlator clock
    input          rst_x,
    input          clk_i, // SRAM/bus clock

    // Real and imaginary components from the antennas:
    input          sw_i, // switch banks
    input          en_i, // data is valid
    input [ISB:0]  re_i, // real component of imput
    input [ISB:0]  im_i, // imaginary component of input

    output [XSB:0] bank_o,      // output the active correlator bank

    //  SRAM interface for reading back the visibilities:
    input          sram_ce_i,
    input [JSB:0]  sram_ad_i,
    output [MSB:0] sram_do_o    // NOTE: 2 cycles of read latency
    );


   wire [TSB:0]    x_rd_adr, x_wr_adr, y_rd_adr, y_wr_adr;
   wire [XSB:0]    bank_adr;
   wire            sw, en;
   wire [ISB:0]    re, im;
   wire [MSB:0]    sram_do_d, sram_do_s;


   //-------------------------------------------------------------------------
   //  Assign outputs depending on the given parameters.
   //-------------------------------------------------------------------------
   assign sram_do_o = CARRY ? sram_do_s : sram_do_d;
   assign bank_o = bank_adr;


   //-------------------------------------------------------------------------
   //  Local synchronous reset for the correlator controllers.
   //-------------------------------------------------------------------------
   (* NOMERGE *) reg rst_ctrl = 1'b1;

   always @(posedge clk_x)
     rst_ctrl <= #DELAY rst_x;


   //-------------------------------------------------------------------------
   //  Correlator (block) control unit.
   //-------------------------------------------------------------------------
   control
     #( .IBITS(IBITS),
        .TICKS(TICKS),
        .SLOW (SLOW),
        .DUPS (DUPS),
        .TRATE(TRATE),
        .TBITS(TBITS),
        .DELAY(DELAY)
        ) CTRL
       (.clk_x     (clk_x), // correlator clock
        .rst       (rst_ctrl),
        .sw_i      (sw_i), // switch banks
        .en_i      (en_i), // data is valid
        .re_i      (re_i), // real component of input
        .im_i      (im_i), // imaginary component
        .x_rd_adr_o(x_rd_adr),
        .x_wr_adr_o(x_wr_adr),
        .y_rd_adr_o(y_rd_adr),
        .y_wr_adr_o(y_wr_adr),
        .bank_o    (bank_adr), // active visibilities bank
        .swap_o    (sw), // bank-swap in progress
        .en_o      (en),
        .real_o    (re),
        .imag_o    (im)
        );


   //-------------------------------------------------------------------------
   //  Block of four DSP48A2-based correlators.
   //-------------------------------------------------------------------------
   block_DSP
     #( .ACCUM(ACCUM),          // Accumulator bit-widths
        .IBITS(IBITS),          // Number of data sources
        .PAIRS0(PAIRS0),
        .PAIRS1(PAIRS1),
        .PAIRS2(PAIRS2),
        .PAIRS3(PAIRS3),
        .WIDTH(WIDTH),         // Combined Re & Im components
        .QBITS(QBITS),         // Total data-bus bit-width of SRAM
        .TRATE(TRATE),         // Time-multiplexing rate
        .TBITS(TBITS),         // Required bit-width for TRATE counter
        .XBITS(XBITS),         // Bank address address bit-width
        .NSRAM(NSRAM),         // #<block SRAM> for read-back
        .ABITS(ABITS),         // External I/O address bits
        .DELAY(DELAY)
        ) BDSP
       (
        .clk_x(CARRY ? 1'b0 : clk_x),

        .x_rd_adr_i(CARRY ? {TBITS{1'b0}} : x_rd_adr),
        .x_wr_adr_i(CARRY ? {TBITS{1'b0}} : x_wr_adr),
        .y_rd_adr_i(CARRY ? {TBITS{1'b0}} : y_rd_adr),
        .y_wr_adr_i(CARRY ? {TBITS{1'b0}} : y_wr_adr),
        .bank_adr_i(CARRY ? {XBITS{1'b0}} : bank_adr),

        .sw_i(CARRY ? 1'b0 : sw), // switch banks
        .en_i(CARRY ? 1'b0 : en), // data is valid
        .re_i(CARRY ? {IBITS{1'b0}} : re), // real component of imput
        .im_i(CARRY ? {IBITS{1'b0}} : im), // imaginary component of input

        .sram_ck_i(CARRY ? 1'b0 : clk_i),
        .sram_ce_i(CARRY ? 1'b0 : sram_ce_i),
        .sram_ad_i(CARRY ? {JBITS{1'b0}} : sram_ad_i),
        .sram_do_o(sram_do_d)
        );


   //-------------------------------------------------------------------------
   //  Block of four carry-chain-based correlators.
   //-------------------------------------------------------------------------
   block_SDP
     #( .ACCUM(ACCUM),          // Accumulator bit-widths
        .IBITS(IBITS),          // Number of data sources
        .PAIRS0(PAIRS0),
        .PAIRS1(PAIRS1),
        .PAIRS2(PAIRS2),
        .PAIRS3(PAIRS3),
        .WIDTH(WIDTH),         // Combined Re & Im components
        .QBITS(QBITS),         // Total data-bus bit-width of SRAM
        .TRATE(TRATE),         // Time-multiplexing rate
        .TBITS(TBITS),         // Required bit-width for TRATE counter
        .XBITS(XBITS),         // Bank address address bit-width
        .NSRAM(NSRAM),         // #<block SRAM> for read-back
        .ABITS(ABITS),         // External I/O address bits
        .DELAY(DELAY)
        ) BSDP
       (
        .clk_x(CARRY ? clk_x : 1'b0),

        .x_rd_adr_i(CARRY ? x_rd_adr : {TBITS{1'b0}}),
        .x_wr_adr_i(CARRY ? x_wr_adr : {TBITS{1'b0}}),
        .y_rd_adr_i(CARRY ? y_rd_adr : {TBITS{1'b0}}),
        .y_wr_adr_i(CARRY ? y_wr_adr : {TBITS{1'b0}}),
        .bank_adr_i(CARRY ? bank_adr : {XBITS{1'b0}}),

        .sw_i(CARRY ? sw : 1'b0), // switch banks
        .en_i(CARRY ? en : 1'b0), // data is valid
        .re_i(CARRY ? re : {IBITS{1'b0}}), // real component of imput
        .im_i(CARRY ? im : {IBITS{1'b0}}), // imaginary component of input

        .sram_ck_i(CARRY ? clk_i : 1'b0),
        .sram_ce_i(CARRY ? sram_ce_i : 1'b0),
        .sram_ad_i(CARRY ? sram_ad_i : {JBITS{1'b0}}),
        .sram_do_o(sram_do_s)
        );


endmodule // correlator_block
