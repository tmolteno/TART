`timescale 1ns/100ps
/*
 * Module      : verilog/bus/wb_transfer.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
 * 
 * Generates the Wishbone, master, control address signals needed to perform
 * single READ bus transactions.
 * 
 * NOTE:
 *  + Supports CLASSIC, Pipelined CLASSIC, and SPEC B4 SINGLE READ transfers;
 *  + Pipelined transfers assert `stb_o` once/transfer, whereas classic
 *    transfers assert `stb_o` until `ack_i`;
 *  + BLOCK burst transfers only assert STB once per command/request, so even
 *    though this module doesn't support burst-mode transfers, it can be
 *    configured to use the same conventions;
 *  + Asynchronous mode (ASYNC == 1) has lower latency, and typically won't
 *    cause critical-path problems when used with a point-to-point topology;
 *  + Otherwise, typically safer to use synchronous mode for buses;
 *  + For additional pipelining, `ASYNC == 2` can be used to further reduce
 *    combination delays (but adding an extra cycle of latency);
 *  + Failure should be rare, so no need for it to be fast, so it's always
 *    synchronous;
 *  + Lowest (cycle) latency with `ASYNC == 2 && CHECK == 0`;
 * 
 * TODO:
 *  + finish/test support for `wat_i`, `rty_i`, and `err_i`;
 * 
 * Changelog:
 *  + 26/10/2016  --  initial file;
 * 
 */

module wb_transfer
  #(//  Capabilities/mode parameters:
    parameter ASYNC = 1,    // Combinational control signals (0/1/2)?
    parameter PIPED = 1,    // Pipelined (WB SPEC B4) transfers (0/1)?
    parameter CHECK = 1,    // Filter spurious "chatter," and check errors?

    parameter DELAY = 3)    // Combinational simulation delay (ns)
   (
    input  clk_i,
    input  rst_i,
    output cyc_o,
    output stb_o,
    output we_o,
    input  ack_i,
    input  wat_i,
    input  rty_i,
    input  err_i,

    input  read_i,
    input  write_i,
    output busy_o,
    output done_o,
    output fail_o
    );


   //-------------------------------------------------------------------------
   //  Synchronous Wishbone control signals.
   //-------------------------------------------------------------------------
   reg     cyc_r = 1'b0;
   reg     stb_r = 1'b0;
   reg     we_r  = 1'b0;
   reg     ack_r = 1'b0;
   reg     err_r = 1'b0;


   //-------------------------------------------------------------------------
   //  Drive the signals either synchronously, or asynchronously.
   //-------------------------------------------------------------------------
   assign cyc_o  = ASYNC > 1 ? cyc_a : cyc_r;
   assign stb_o  = ASYNC > 1 ? stb_a : stb_r;
   assign we_o   = ASYNC > 1 ? we_a  : we_r ;

   //-------------------------------------------------------------------------
   //  Prevent asynchronous signals from asserting during resets.
   wire    cyc_a, stb_a, we_a, ack_a, err_a, stb_p;

   assign cyc_a  = CHECK ? !rst_i && cyc_w : cyc_w;
   assign stb_a  = CHECK ? !rst_i && stb_w : stb_w;
   assign we_a   = CHECK ? we_w : write_i || we_r;
   assign ack_a  = CHECK ? !rst_i && ack_w : ack_w;
   assign err_a  = CHECK ? !rst_i && err_w : err_w;

   //  Strobe-source for pipelined, or classic transactions.
   assign stb_p  = PIPED ? cyc_r : stb_o;

   //-------------------------------------------------------------------------
   //  Aynchronous Wishbone bus master signals.
   wire    cyc_w, stb_w, we_w, ack_w, err_w;

   assign cyc_w  = read_i || write_i || cyc_r;
   assign stb_w  = PIPED ? read_i || write_i : cyc_w;
   assign we_w   = write_i && !read_i && !cyc_r || we_r;
   assign ack_w  = (CHECK ? stb_p : 1'b1) &&  ack_i;
   assign err_w  = (CHECK ? stb_p : 1'b1) && (rty_i || err_i);

   //-------------------------------------------------------------------------
   //  Transfer result signals.
   //  NOTE: Unchecked, asynchronous modes potentially allow for spurious
   //    `done_o` assertions.
   wire    done_w;

   assign busy_o = ASYNC > 1 ? cyc_o  : cyc_r; // TODO: just use `cyc_r`?
   assign done_o = ASYNC > 0 ? done_w : ack_r;
   assign fail_o = err_r;

   assign done_w = CHECK ? stb_p && ack_i : ack_i;


   //-------------------------------------------------------------------------
   //  Synchronous Wishbone bus master signals.
   //-------------------------------------------------------------------------
   wire    cycle = (!cyc_r || !CHECK) && (read_i || write_i);
   wire    write = (!cyc_r || !CHECK) && write_i;

   //  Keep CYC asserted for the duration of the transaction.
   always @(posedge clk_i)
     if (rst_i)                 // Required by the Wishbone protocol.
       cyc_r <= #DELAY 1'b0;
     else if (cycle)
       cyc_r <= #DELAY 1'b1;
     else if (cyc_r && (ack_i || rty_i || err_i))
       cyc_r <= #DELAY 1'b0;
     else
       cyc_r <= #DELAY cyc_r;

   //  STB is asserted for only one cycle, for Pipelined, SINGLE READ
   //  & SINGLE WRITE transactions, or until ACK for standard READ/WRITE
   //  cycles.
   always @(posedge clk_i)
     if (rst_i || ASYNC > 1)    // Required by the Wishbone protocol.
       stb_r <= #DELAY 1'b0;
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
