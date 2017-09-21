`timescale 1ns/100ps
/*
 * Module      : rtl/wb_transfer.v
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
 * Generates the Wishbone, master, control signals needed to perform single
 * READ & WRITE bus transactions.
 * 
 * Modes:
 *  + ASYNC == 0  -- adds two cycles of latency, by registering CYC & STB, and
 *                   then registering the result, before driving `done`;
 *  + ASYNC == 1  -- adds a cycle of latency, by registering CYC & STB;
 *  + ASYNC == 2  -- fully combinational;
 *  + PIPED == 0  -- asserts STB until ACK (Wishbone Classic);
 *  + PIPED == 1  -- asserts STB just once per transfer (SPEC B4);
 *  + CHECK == 0  -- doesn't perform sanity-checking, nor performs filter of
 *                   spurious bus signals, like extra ACK's, etc.; and
 *  + CHECK == 1  -- sanity checking plus ignores bus chatter outside of bus
 *                   cycles.
 *  + FRAME == 1  -- the 'frame_i' signal asserts for the duration of the
 *                   transfer, and just 'write_i' is used to determine the
 *                   direction of the transfer;
 *  + BURST == 1  -- requires 'FRAME == 1', and for each cycle that 'read_i'
 *                   or 'write_i' is asserted represents a command that will
 *                   have a corresponding 'ACK', and with 'frame_i' framing
 *                   the transaction;
 * 
 * NOTE:
 *  + supports CLASSIC, Pipelined CLASSIC, and SPEC B4 SINGLE READ/WRITE
 *    (PIPELINED) transfers;
 *  + masters that support PIPELINED transfers assert 'STB_O' once/transfer,
 *    whereas CLASSIC transfers assert (and hold) 'STB_O' until 'ACK_I';
 *  + BLOCK burst transfers only assert 'STB' once per command/request, so
 *    even though this module doesn't support burst-mode transfers, it can be
 *    (and is by default) configured to use the same conventions;
 *  + asynchronous mode (ASYNC == 1) has lower latency, and typically won't
 *    cause critical-path problems when used with a point-to-point topology;
 *  + otherwise, typically safer to use synchronous mode for buses;
 *  + for additional pipelining, 'ASYNC == 0' can be used to further reduce
 *    combination delays (but adding an extra cycle of latency);
 *  + failure should be rare, so no need for it to be fast, therefore it's
 *    always synchronous;
 *  + lowest (cycle) latency with 'ASYNC == 2 && CHECK == 0';
 *  + when a bus only has a single master then the 'CYC' signals are not
 *    typically needed, and set 'CHECK == 0' to disable the 'CYC' logic (as
 *    'CYC' primarily signals to other masters that the bus is currently in
 *    use by another master);
 * 
 * TODO:
 *  + finish/test support for `wat_i`, `rty_i`, and `err_i`;
 *  + test & document 'FRAME' & 'BURST' mode settings;
 * 
 * Changelog:
 *  + 26/10/2016  --  initial file;
 * 
 */

module wb_transfer
  #(//  Capabilities/mode parameters:
    parameter ASYNC = 1,    // combinational control signals (0/1/2)?
    parameter PIPED = 1,    // pipelined (WB SPEC B4) transfers (0/1)?
    parameter CHECK = 1,    // filter spurious "chatter," and check errors?
    parameter READ  = 1,    // enable read transfers (0/1)?
    parameter WRITE = 1,    // enable write transfers (0/1)?
    parameter FRAME = 0,    // use FRAME+WRITE to drive the bus (0/1)?
    parameter BURST = 0,    // support multiple transfers per cycle (0/1)?

    //  Simulation-only settings:
    parameter DELAY = 3)    // combinational simulation delay (ns)
   (
    input  clk_i, // bus clock & reset signals
    input  rst_i,

    output cyc_o,
    output stb_o,
    output we_o,
    input  ack_i,
    input  wat_i,
    input  rty_i,
    input  err_i,

    input  frame_i, // TODO: alternate method for bus transfers
    input  read_i,
    input  write_i,
    output wait_o,              // wait-state (only for 'BURST's)
    output busy_o,
    output done_o,
    output fail_o
    );


   //-------------------------------------------------------------------------
   //  Internal signal definitions.
   //-------------------------------------------------------------------------
   //  Synchronous Wishbone control signals.
   reg     cyc_r = 1'b0, stb_r = 1'b0, we_r  = 1'b0; // master signals
   reg     ack_r = 1'b0, err_r = 1'b0;               // slave signals

   //  Asynchronous Wishbone control signals.
   wire    cyc_w, stb_w, we_w, ack_w, err_w;
   wire    cyc_a, stb_a, we_a, ack_a, err_a, stb_p;

   //  Module-feature signals.
   wire    read_w, rd_w, wr_w, rd_en, wr_en;

   //  State signals.
   wire    cycle, write, done_w;


   //-------------------------------------------------------------------------
   //  Signals that depend on features that were enabled in the instance
   //  parameters.
   //-------------------------------------------------------------------------
   assign read_w = BURST ? read_i : !write_i;
   assign rd_w   = FRAME ? frame_i && read_w  : read_i;
   assign wr_w   = FRAME ? frame_i && write_i : write_i;

   assign rd_en  = READ  ? rd_w : 1'b0;
   assign wr_en  = WRITE ? wr_w : 1'b0;


   //-------------------------------------------------------------------------
   //  Drive the signals either synchronously, or asynchronously.
   //-------------------------------------------------------------------------
   assign cyc_o  = ASYNC > 1 ? cyc_a : cyc_r;
   assign stb_o  = ASYNC > 1 ? stb_a : stb_r;
   assign we_o   = ASYNC > 1 ? we_a  : we_r ;

   //-------------------------------------------------------------------------
   //  Prevent asynchronous signals from asserting during resets.
   assign cyc_a  = CHECK ? !rst_i && cyc_w : cyc_w;
   assign stb_a  = CHECK ? !rst_i && stb_w : stb_w;
   assign we_a   = CHECK ? we_w : wr_en || we_r;
   assign ack_a  = CHECK ? !rst_i && ack_w : ack_w;
   assign err_a  = CHECK ? !rst_i && err_w : err_w;

   //  Strobed for pipelined, or held asserted for classic transactions.
   assign stb_p  = PIPED ? cyc_r : stb_o;

   //-------------------------------------------------------------------------
   //  Aynchronous Wishbone bus master signals.
   assign cyc_w  = rd_en || wr_en || cyc_r;
   assign stb_w  = PIPED ? rd_en || wr_en : cyc_w;
   assign we_w   = wr_en && !rd_en && !cyc_r || we_r;
   assign ack_w  = (CHECK ? stb_p : 1'b1) &&  ack_i;
   assign err_w  = (CHECK ? stb_p : 1'b1) && (rty_i || err_i);


   //-------------------------------------------------------------------------
   //  Transfer result signals.
   //  NOTE: Unchecked, asynchronous modes potentially allow for spurious
   //    `done_o` assertions.
   //-------------------------------------------------------------------------
   assign wait_o = BURST ? wat_i : 1'b0;
   assign busy_o = ASYNC > 1 ? cyc_o  : cyc_r; // TODO: just use `cyc_r`?
   assign done_o = ASYNC > 0 ? done_w : ack_r;
   assign fail_o = err_r;

   //-------------------------------------------------------------------------
   //  State signals.
   assign done_w = BURST && FRAME ? cyc_r && !frame_i : ack_w;
   assign cycle  = (!cyc_r || !CHECK) && (rd_en || wr_en);
   assign write  = (!cyc_r || !CHECK) && wr_en;


   //-------------------------------------------------------------------------
   //  Synchronous Wishbone bus master signals.
   //-------------------------------------------------------------------------
   //  Keep CYC asserted for the duration of the transaction.
   always @(posedge clk_i)
     if (rst_i)                 // Required by the Wishbone protocol.
       cyc_r <= #DELAY 1'b0;
     else if (cycle)
       cyc_r <= #DELAY 1'b1;
     else if (done_w || rty_i || err_i)
       cyc_r <= #DELAY 1'b0;
     else
       cyc_r <= #DELAY cyc_r;

   //  STB is asserted for only one cycle, for Pipelined, SINGLE READ
   //  & SINGLE WRITE transactions, or until ACK for standard READ/WRITE
   //  cycles.
   //  NOTE: Not used for 'ASYNC == 2'.
   always @(posedge clk_i)
     if (rst_i || ASYNC > 1)    // Required by the Wishbone protocol.
       stb_r <= #DELAY 1'b0;
     else if (BURST && FRAME)
       stb_r <= #DELAY frame_i && (rd_en || wr_en);
     else if (cycle)
       stb_r <= #DELAY 1'b1;
     else if (PIPED)
       stb_r <= #DELAY 1'b0;
     else if (ack_i || rty_i || err_i)
       stb_r <= #DELAY 1'b0;
     else
       stb_r <= #DELAY stb_r;

   //  By default keep write-enable low, for faster asynchronous reads.
   always @(posedge clk_i)
     if (ack_i || err_i || rty_i)
       we_r  <= #DELAY 1'b0;
     else if (write)
       we_r  <= #DELAY 1'b1;
     else
       we_r  <= #DELAY we_r;

   //  For synchronous transactions, register the incomine ACK.
   always @(posedge clk_i) begin
      ack_r <= #DELAY ASYNC > 0 ? 1'b0 : ack_w;
      err_r <= #DELAY err_w;
   end


endmodule // wb_transfer
