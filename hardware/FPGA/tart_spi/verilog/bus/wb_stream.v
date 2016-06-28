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
 *  + 20/06/2016  --  initial file;
 * 
 * TODO:
 *  + currently unfinished;
 *  + needs to support burst-modes?
 *  + parameter to control whether pipelining is used, or not?
 * 
 */

// TODO: Not very modular, as the rest of this module is not TART-specific.
`include "tartcfg.v"

module wb_stream
  #(parameter WIDTH = 8,
    parameter MSB   = WIDTH-1,
    parameter WBITS = 10,
    parameter WORDS = 1 << WBITS,
    parameter ASB   = WBITS-1,
    parameter START = 0,
    parameter LAST  = START+WORDS-1,
    parameter STEP  = 1,
    parameter DELAY = 3)
   (
    // Common system signals:
    input              clk_i,
    input              rst_i,

    // Prefetching, Wishbone-like (master) bus interface, to block device:
    output             m_cyc_o,
    output             m_stb_o,
    output             m_we_o,
    output             m_bst_o, // Bulk Sequential Transfer?
    input              m_ack_i,
    input              m_wat_i,
    output reg [ASB:0] m_adr_o = START,
    input [MSB:0]      m_dat_i,
    output [MSB:0]     m_dat_o,

    // Streaming/serial, Wishbone-like (slave) bus interface:
    input              s_cyc_i,
    input              s_stb_i,
    input              s_we_i,
    input              s_bst_i,
    output             s_ack_o,
    output             s_wat_o,
    input [MSB:0]      s_dat_i,
    output [MSB:0]     s_dat_o
    );

   // TODO: burst-transfer support?
`ifdef __WB_CLASSIC
   wire                inc      = m_cyc_o && m_ack_i;
`else
   wire                inc      = m_cyc_o && m_stb_o && !m_wat_i;
`endif
   wire                wrap_adr = m_adr_o == LAST;
   wire [ASB:0]        next_adr = wrap_adr ? START : m_adr_o + STEP;

   //-------------------------------------------------------------------------
   //  Just pass through all of the signals, except for the address, which is
   //  managed by this logic core.
   assign m_cyc_o = s_cyc_i;
   assign m_stb_o = s_stb_i;
   assign m_we_o  = s_we_i;
   assign m_bst_o = s_bst_i;
   assign m_dat_o = s_dat_i;

   assign s_ack_o = m_ack_i;
   assign s_wat_o = m_wat_i;
   assign s_dat_o = m_dat_i;


   //-------------------------------------------------------------------------
   //  Address generation.
   always @(posedge clk_i)
     if (rst_i)    m_adr_o <= #DELAY START;
     else if (inc) m_adr_o <= #DELAY next_adr;


endmodule // wb_stream
