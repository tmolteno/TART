`timescale 1ns/100ps
/*
 * Module      : verilog/correlator/control.v
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
 *  + 28/10/2016  --  initial file (rebuilt from `correlator_block_DSP.v`);
 * 
 * TODO:
 * 
 */

module control
  #(//  Data-inputs parameters:
    parameter IBITS = 24,       // Number of data inputs
    parameter ISB   = IBITS-1,

    // Latency & performance parameters:
    parameter TICKS = 4,        // Correlator latency (3/4)
    parameter SLOW  = 0,        // Slow clocks (0/1)?
    parameter DUPS  = 0,        // Duplicate high-fanout registers (0/1)?

    //  Time-multiplexing parameters:
    parameter TRATE = 12,       // Time-multiplexing rate
    parameter TBITS = 4,
    parameter TSB   = TBITS-1,

    //  Parameters for the number of banks of visibilities:
    parameter XBITS = 4,
    parameter XSB   = XBITS-1,

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_x, // correlator clock
    input          rst,

    // Real and imaginary components from the antennas:
    input          sw_i, // switch banks
    input          en_i, // data is valid
    input [ISB:0]  re_i, // real component of input
    input [ISB:0]  im_i, // imaginary component

    //  Correlator-block control signals:
    output [TSB:0] x_rd_adr_o,
    output [TSB:0] x_wr_adr_o,

    output [TSB:0] y_rd_adr_o,
    output [TSB:0] y_wr_adr_o,

    output         swap_o, // bank-swap in progress
    output         en_o,   // data is valid, so enable correlators
    output [XSB:0] bank_o, // active visibilities bank
    output [ISB:0] real_o,
    output [ISB:0] imag_o
    );


   //  Bank-switching signals & registers.
   reg             swap = 1'b0, clear = 1'b1, clear_r = 1'b1;
   reg [XSB:0]     bank = {XBITS{1'b0}};
   wire [XBITS:0]  next_bank;

   //  Data-source signals & registers.
   wire            sw, en; // TODO: need `NOMERGE`?
   wire [ISB:0]    re, im;
   reg             sw_r = 1'b0, en_r = 1'b0; // TODO: need `NOMERGE`?
   reg [ISB:0]     re_r, im_r;

   //  Correlator SRAM address signals.
   wire            wrap_x_rd_adr, wrap_x_wr_adr;
   wire            w_swp, w_inc;
   (* NOMERGE = "TRUE" *) wire [TSB:0] x_rd_adr; // ensure separate address
   (* NOMERGE = "TRUE" *) wire [TSB:0] x_wr_adr; // counters for each block
   (* NOMERGE = "TRUE" *) wire [TSB:0] y_rd_adr;
   (* NOMERGE = "TRUE" *) wire [TSB:0] y_wr_adr;


   //-------------------------------------------------------------------------
   //  Map correlator data- & control- signals to the outputs.
   //-------------------------------------------------------------------------
   assign en_o   = en;
   assign swap_o = TICKS == 3 ? clear : clear_r;
   assign bank_o = bank;
   assign real_o = re;
   assign imag_o = im;

   //-------------------------------------------------------------------------
   //  These signals are the (Xilinx, distributed) SRAM addresses for the
   //  Read -> Accumulate -> Write cycles of the correlators.
   //-------------------------------------------------------------------------
   assign x_rd_adr_o = x_rd_adr;
   assign x_wr_adr_o = x_wr_adr;

   //  Optionally duplicate high-fanout signals.
   assign y_rd_adr_o = DUPS ? y_rd_adr : x_rd_adr;
   assign y_wr_adr_o = DUPS ? y_wr_adr : x_wr_adr;

   assign w_swp = wrap_x_rd_adr && (sw || swap);
   assign w_inc = wrap_x_wr_adr && clear;

   //-------------------------------------------------------------------------
   //  Internal control & data signals, from the data-source (antennae, via
   //  the Hilbert transform).
   //-------------------------------------------------------------------------
   assign sw        = SLOW ? sw_i : sw_r;
   assign en        = SLOW ? en_i : en_r;
   assign re        = SLOW ? re_i : re_r;
   assign im        = SLOW ? im_i : im_r;

   assign next_bank = bank + 1;


   //-------------------------------------------------------------------------
   //  Banks are switched at the next address-wrap event.
   //-------------------------------------------------------------------------
   //  Stores that a bank-swap is pending.
   always @(posedge clk_x)
     if (rst || w_swp)
       swap <= #DELAY 1'b0;
     else if (sw && !swap) // swap banks @next wrap
       swap <= #DELAY 1'b1;
   
   //  Clear a bank when correlators are enabled, or during the first set of
   //  writes after a bank-switch.
   always @(posedge clk_x)
     if (rst || !en)
        clear <= #DELAY 1'b1;
     else if (w_swp)
        clear <= #DELAY 1'b1;
     else if (wrap_x_rd_adr && clear) // finished restarting counters
        clear <= #DELAY 1'b0;

   //  Increment the bank-counter two cycles later, so that the correct data
   //  is stored within the SRAM's.
   always @(posedge clk_x)
     if (rst)
       bank <= #DELAY {XBITS{1'b0}};
     else if (w_inc)
       bank <= #DELAY next_bank[XSB:0];


   //-------------------------------------------------------------------------
   //  Optionally register high-fanout signals.
   //-------------------------------------------------------------------------
   //  Locally register some incoming, high-fanout (and path-length) signals.
   always @(posedge clk_x)
     {sw_r, en_r, im_r, re_r} <= #DELAY {sw_i, en_i, im_i, re_i};

   //  Add an extra cycle of latency to clearing the SRAM's, if using an
   //  additional cycle of pipelining.
   always @(posedge clk_x)
     clear_r <= #DELAY clear;


   //-------------------------------------------------------------------------
   //  Correlator memory pointers.
   //-------------------------------------------------------------------------
   rmw_address_unit
     #(  .ABITS(TBITS),
         .UPPER(TRATE-1),
         .TICKS(TICKS),
         .DELAY(DELAY)
         ) RMWX
       ( .clk_i(clk_x),
         .rst_i(rst),
         .ce_i(en),
         .rd_adr_o(x_rd_adr),
         .rd_wrap_o(wrap_x_rd_adr),
         .wr_adr_o(x_wr_adr),
         .wr_wrap_o(wrap_x_wr_adr)
         );

   //  Due to the high-fanout, and long path-lengths, (optionally) duplicate
   //  the distributed SRAM address registers.
   rmw_address_unit
     #(  .ABITS(TBITS),
         .UPPER(TRATE-1),
         .TICKS(TICKS),
         .DELAY(DELAY)
         ) RMWY
       ( .clk_i(DUPS ? clk_x : 1'b0),
         .rst_i(DUPS ? rst   : 1'b0),
         .ce_i (DUPS ? en    : 1'b0),
         .rd_adr_o(y_rd_adr),
         .rd_wrap_o(),
         .wr_adr_o(y_wr_adr),
         .wr_wrap_o()
         );


endmodule // control
