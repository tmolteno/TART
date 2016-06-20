`timescale 1ns/100ps
/*
 *
 * Module      : verilog/bus/wb_stream.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Simple data-streaming circuit, connected via a Wishbone-like interconnect.
 * The purpose of the module is to stream out data from a block device (e.g.,
 * a SRAM), with the sequential addresses automatically generated per access.
 * 
 * NOTE:
 *  + supports classic, pipelined, and burst transfers;
 * 
 * Changelog:
 *  + 18/06/2016  --  initial file;
 * 
 * TODO:
 *  + currently unfinished;
 * 
 */

module wb_stream
  #(parameter WIDTH = 8,
    parameter MSB   = WIDTH-1,
    parameter WORDS = 1 << WBITS,
    parameter WBITS = 10,
    parameter ASB   = WBITS-1,
    parameter DELAY = 3)
   (
    // Common system signals:
    input              clk_i,
    input              rst_i,

    // Prefetching, Wishbone-like (master) bus interface, to block device:
    output reg         m_cyc_o = 0,
    output reg         m_stb_o = 0,
    output             m_we_o,
    output reg         m_bst_o = 0, // Bulk Sequential Transfer?
    input              m_ack_i,
    output reg [ASB:0] m_adr_o = 0,
    input [MSB:0]      m_dat_i,
    output reg [MSB:0] m_dat_o,

    // Streaming/serial, Wishbone-like (slave) bus interface:
    input              s_cyc_i,
    input              s_stb_i,
    input              s_we_i,
    input              s_bst_i,
    output reg         s_ack_o = 0,
    output reg         s_wat_o = 0,
    input [MSB:0]      s_dat_i,
    output reg [MSB:0] s_dat_o = 0
    );


   //-------------------------------------------------------------------------
   //  Logic for the Wishbone-like interconnect.
   always @(posedge clk_i)
     if (rst_i) s_ack_o <= #DELAY 0;
     else       s_ack_o <= #DELAY s_cyc_i && s_stb_i && !s_ack_o;

   always @(posedge clk_i)
     s_dat_o <= #DELAY s_cyc_i && s_stb_i && !s_we_i ? m_dat_i : s_dat_o;


endmodule // wb_stream
