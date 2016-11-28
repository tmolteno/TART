`timescale 1ns/100ps
/*
 * Module      : bench/xilinx/IDDR2.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
 * 
 * Simulation-only module implementing the functionality of the Spartan 6
 * primitive of the same name.
 * 
 * NOTE:
 * 
 * TODO:
 *  + currently only a subset of the 'IDDR2' functionality is supported;
 * 
 */

module IDDR2
  #(
    parameter DDR_ALIGNMENT = "NONE",
    parameter SRTYPE = "SYNC",
    parameter Q0_INIT = 0,
    parameter Q1_INIT = 0,
    parameter DELAY = 3)        // Icarus only
   (
    input  C0,
    input  C1,
    input  R,
    input  S,
    input  CE,
    input  D,
    output Q0,
    output Q1
    );

   parameter ALIGN0 = DDR_ALIGNMENT == "C0";
   parameter ALIGN1 = DDR_ALIGNMENT == "C1";

   reg     QP, QN;
   reg     RP, RN;
   wire    CLR, PRE;
   wire    RST, SET;

   pulldown (S);
   pulldown (R);

   assign Q0 = ALIGN1 ? RP : QP;
   assign Q1 = ALIGN0 ? RN : QN;

   assign CLR = SRTYPE == "ASYNC" ? R : 1'b0;
   assign PRE = SRTYPE == "ASYNC" ? S : 1'b1;

   assign RST = SRTYPE ==  "SYNC" ? R : 1'b0;
   assign SET = SRTYPE ==  "SYNC" ? S : 1'b1;

   always @(posedge C0 or posedge CLR or posedge PRE)
     if (CLR || RST)
       {RN, QP} <= #DELAY 2'b00;
     else if (PRE || SET)
       {RN, QP} <= #DELAY 2'b11;
     else if (CE)
       {RN, QP} <= #DELAY {QN, D};
     else
       {RN, QP} <= #DELAY {RN, QP};

   always @(posedge C1 or posedge CLR or posedge PRE)
     if (CLR || RST)
       {RP, QN} <= #DELAY 2'b00;
     else if (PRE || SET)
       {RP, QN} <= #DELAY 2'b11;
     else if (CE)
       {RP, QN} <= #DELAY {QP, D};
     else
       {RP, QN} <= #DELAY {RP, QN};

endmodule // IDDR2
