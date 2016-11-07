`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/capture_control.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Generates the domain-appropriate versions of the reset and control signals,
 * for the capture logic-core.
 * 
 * NOTE:
 *  + the `e` suffix is used to tag signals from the external (signal) clock-
 *    domain, and the `x` suffix for the (12x by default) sampling- and
 *    correlator- domain clock;
 *  + even though the sample-clock's frequency is an integer multiple of the
 *    external clock, the phase relationship is unknown (due to the quirky
 *    Spartan 6 DCM's), thus asynchronous domain-crossing techniques must be
 *    used;
 *  + the `ALIGN` parameter selects whether to oversample the antenna
 *    signals, and perform clock-recovery, if enabled (1) -- or instead to
 *    cross the clock-domain (to the correlator domain) using an asynchronous
 *    FIFO (0);
 * 
 * TODO:
 *  + setup a bunch of FROM/TO constraints for CDC;
 * 
 */

module capture_control
  #(//  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input  clk_b_i, // bus clock
    input  rst_b_i, // bus-domain reset

    input  clk_x_i, // sample/correlator clock
    output rst_x_o, // reset (sample domain)

    input  clk_e_i, // external clock
    output rst_e_o, // reset (external domain)

    input  ce_b_i, // capture-enable
    output ce_e_o, // external-domain CE
    output ce_x_o, // sample-domain CE

    input  debug_b_i, // enable fake-data
    output debug_e_o,

    input  store_b_i, // store raw-data in DRAM
    output store_e_o,

    input  align_b_i, // enable data-alignment unit
    output align_x_o
    );


   //-------------------------------------------------------------------------
   //  Reset CDC signals.
   (* NOMERGE = "TRUE" *)
   reg     rst_s, rst_e; // bus -> signal domain synchronisers
   (* NOMERGE = "TRUE" *)
   reg     rst_t, rst_x; // bus -> sample domain synchronisers

   //  Capture-Enable (CE) CDC signals.
   (* NOMERGE = "TRUE" *)
   reg     ce_s, ce_e;
   (* NOMERGE = "TRUE" *)
   reg     ce_t, ce_x;


   //-------------------------------------------------------------------------
   //
   //  SYNCHRONISERS.
   //
   //-------------------------------------------------------------------------
   //  Assign synchronised signals to outputs.
   assign rst_e_o = rst_e;
   assign rst_x_o = rst_x;

   assign ce_e_o  = ce_e;
   assign ce_x_o  = ce_x;


   //-------------------------------------------------------------------------
   //  Re-register input signals, so that they're "close," for the FROM/TO
   //  contraints.
   //-------------------------------------------------------------------------
   (* NOMERGE = "TRUE" *)
   reg     rst_b, ce_b;

   always @(posedge clk_e_i)
     {ce_b, rst_b} <= #DELAY {ce_b_i, rst_b_i};


   //-------------------------------------------------------------------------
   //  Signal/external-domain synchronisers.
   //-------------------------------------------------------------------------
   //  Synchronise the bus-domain reset into the signal-domain.
   always @(posedge clk_e_i)
     begin
        {rst_e, rst_s} <= #DELAY {rst_s, rst_b};
        { ce_e , ce_s} <= #DELAY { ce_s,  ce_b};
     end


   //-------------------------------------------------------------------------
   //  Sample/correlator-domain synchronisers.
   //-------------------------------------------------------------------------
   //  Synchronise the bus-domain reset into the correlator-domain.
   always @(posedge clk_x_i)
     begin
        {rst_x, rst_t} <= #DELAY {rst_t, rst_b};
        { ce_x,  ce_t} <= #DELAY { ce_t,  ce_b};
     end


endmodule // capture_control
