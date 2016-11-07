`timescale 1ns/100ps
/*
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
 *  + 27/10/2016  --  upgraded to Wishbone SPEC B4;
 * 
 * TODO:
 *  + currently unfinished;
 *  + needs to support burst-modes?
 *  + parameter to control whether pipelining is used, or not?
 *  + upgrade to Wishbone SPEC B4;
 * 
 */

// TODO: Not very modular, as the rest of this module is not TART-specific.
`include "tartcfg.v"

module wb_stream
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

    //  Wishbone bus mode parameters:
    parameter ASYNC = 1,     // combinational control signals (0/1/2)?
    parameter PIPED = 1,     // pipelined (SPEC B4) transfers (0/1)?
    parameter CHECK = 0,     // TODO: extra sanity-checking (0/1)?

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_i,
    input          rst_i,

    //  Prefetching, Wishbone-like (master) bus interface, to block device:
    output         m_cyc_o,
    output         m_stb_o,
    output         m_we_o,
`ifndef __WB_SPEC_B4
    output         m_bst_o, // Bulk Sequential Transfer?
`endif
    input          m_ack_i,
    input          m_wat_i,
    input          m_rty_i,
    input          m_err_i,
    output [ASB:0] m_adr_o,
    input [MSB:0]  m_dat_i,
    output [MSB:0] m_dat_o,

    //  Streaming/serial, Wishbone-like (slave) bus interface:
    input          s_cyc_i,
    input          s_stb_i,
    input          s_we_i,
`ifndef __WB_SPEC_B4
    input          s_bst_i,
`endif
    output         s_ack_o,
    output         s_wat_o,
    output         s_rty_o,
    output         s_err_o,
    input [MSB:0]  s_dat_i,
    output [MSB:0] s_dat_o,

    output reg     wrapped = 1'b0
    );


   wire            inc, cyc_w, inc_w, wrap_adr;
   reg [ASB:0]     adr = START;
   wire [WBITS:0]  next_adr;


   //-------------------------------------------------------------------------
   //  Just pass through all of the signals, except for the address, which is
   //  managed by this logic core.
   assign m_cyc_o  = s_cyc_i;
   assign m_stb_o  = s_stb_i;
   assign m_we_o   = s_we_i;
`ifndef __WB_SPEC_B4
   assign m_bst_o  = s_bst_i;
`endif
   assign m_adr_o  = adr;
   assign m_dat_o  = s_dat_i;

   assign s_ack_o  = m_ack_i;
   assign s_wat_o  = m_wat_i;
   assign s_rty_o  = m_rty_i;
   assign s_err_o  = m_err_i;
   assign s_dat_o  = m_dat_i;

   //-------------------------------------------------------------------------
   //  Generate the signals that handle the addresses.
   assign inc      = m_stb_o && cyc_w && inc_w;
   assign cyc_w    = CHECK ? !s_cyc_i : 1'b1;
   assign inc_w    = PIPED ? !m_wat_i : m_ack_i;

   assign wrap_adr = adr == LAST;
   assign next_adr = wrap_adr ? START : adr + STEP;


   //-------------------------------------------------------------------------
   //  Address generation.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       adr <= #DELAY START;
     else if (inc)
       adr <= #DELAY next_adr[ASB:0];
     else
       adr <= #DELAY adr;

   //  Strobe the `wrapped` signal after each complete transfer.
   always @(posedge clk_i)
     if (rst_i && RESET)
       wrapped <= #DELAY 1'b0;
     else if (inc)
       wrapped <= #DELAY wrap_adr;
     else
       wrapped <= #DELAY 1'b0;


   //-------------------------------------------------------------------------
   //  Additional debug/configuration output.
   //-------------------------------------------------------------------------
   initial begin : STREAM_BLOCK
      $display("\nModule : wb_stream (%m)\n\tWIDTH\t= %4d\n\tWORDS\t= %4d\n\tWBITS\t= %4d\n\tSTART\t= %4d\n\tSTEP\t= %4d\n\tLAST\t= %4d\n", WIDTH, WORDS, WBITS, START, STEP, LAST);
   end // STREAM_BLOCK


endmodule // wb_stream
