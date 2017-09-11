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
 * Simulation-only module implementing the functionality of the Spartan 6
 * primitive of the same name.
 * 
 * NOTE:
 *  + WILL NOT SYNTHESISE;
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

//    pulldown (S);
//    pulldown (R);


   //-------------------------------------------------------------------------
   //  Set & reset assignments (both asynchronous and synchronous).
   //-------------------------------------------------------------------------
   assign CLR = SRTYPE == "ASYNC" ? R : 1'b0;
   assign PRE = SRTYPE == "ASYNC" ? S : 1'b0;

   assign RST = SRTYPE ==  "SYNC" ? R : 1'b0;
   assign SET = SRTYPE ==  "SYNC" ? S : 1'b0;

   assign Q0 = ALIGN1 ? RP : QP;
   assign Q1 = ALIGN0 ? RN : QN;


   //-------------------------------------------------------------------------
   //  Implement the DDR output registers.
   //-------------------------------------------------------------------------
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
