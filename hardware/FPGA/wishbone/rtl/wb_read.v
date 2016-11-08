`timescale 1ns/100ps
/*
 * Module      : rtl/wb_read.v
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
 *  + See `wb_cycle` for a description of the Wishbone parameters and modes;
 *  + a subset of the functionality of 'wb_transfer';
 * 
 * TODO:
 * 
 * Changelog:
 *  + 26/10/2016  --  initial file;
 * 
 */

module wb_read
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
    output busy_o,
    output done_o,
    output fail_o
    );

   assign we_o = 1'b0;


   //-------------------------------------------------------------------------
   //  Frame the bus cycle.
   //-------------------------------------------------------------------------
   wb_cycle
     #( .ASYNC(ASYNC),
        .PIPED(PIPED),
        .CHECK(CHECK),
        .DELAY(DELAY)
        ) RDCYC
       (
        .clk_i  (clk_i),
        .rst_i  (rst_i),
        .cyc_o  (cyc_o),
        .stb_o  (stb_o),
        .ack_i  (ack_i),
        .wat_i  (wat_i),
        .rty_i  (rty_i),
        .err_i  (err_i),
        .start_i(read_i),
        .busy_o (busy_o),
        .done_o (done_o),
        .fail_o (fail_o)
        );


endmodule // wb_read
