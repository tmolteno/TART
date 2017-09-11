`timescale 1ns/100ps
/*
 * Module      : bench/xilinx/DSP48A1.v
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
 * Dummy version of the Xilinx DSP48A1 primitive, for use with simulators that
 * don't have the Xilinx components.
 * 
 * NOTE:
 *  + WILL NOT SYNTHESISE;
 *  + Incomplete;
 * 
 * Changelog:
 *  + 14/07/2016  --  initial file;
 * 
 */

module DSP48A1
  #( parameter OPMODEREG   = 1'b1,
     parameter A0REG       = 1'b1,
     parameter A1REG       = 1'b1,
     parameter B0REG       = 1'b1,
     parameter B1REG       = 1'b1,
     parameter CREG        = 1'b1,
     parameter DREG        = 1'b1,
     parameter MREG        = 1'b1,
     parameter PREG        = 1'b1,
     parameter CARRYINREG  = 1'b1,
     parameter CARRYINSEL  = 1'b0,   // TODO:
     parameter CARRYOUTREG = 1'b1,
     parameter RSTTYPE     = "SYNC", // TODO:
     parameter DELAY       = 3
     )
   (
    input                CLK,

     //  Various resets:
    input                RSTOPMODE,
    input                RSTA,
    input                RSTB,
    input                RSTC,
    input                RSTD,
    input                RSTCARRYIN,
    input                RSTM,
    input                RSTP,

     //  And their corresponding enables:
    input                CEOPMODE,
    input                CEA,
    input                CEB,
    input                CEC,
    input                CED,
    input                CECARRYIN,
    input                CEM,
    input                CEP,

     //  DSP operating-mode:
    input [7:0]          OPMODE,

     //  Data inputs:
    input signed [17:0]  A,
    input signed [17:0]  B,
    input signed [47:0]  C,
    input signed [17:0]  D,

     //  And outputs:
    output               CARRYOUTF,
    output signed [35:0] M,
    output signed [47:0] P,

     //  DSP cascade ports:
    input                CARRYIN,
    output               CARRYOUT,
//     input signed [17:0]  BCIN,
    output signed [17:0] BCOUT,
    input signed [47:0]  PCIN,
    output signed [47:0] PCOUT
     );

   wire signed [17:0]    BCIN;  // TODO: magical

   reg [7:0]             reg_OP = 0;
   reg signed [17:0]     reg_A0 = 0, reg_A1 = 0;
   reg signed [17:0]     reg_B0 = 0, reg_B1 = 0;
   reg signed [47:0]     reg_C  = 0;
   reg signed [17:0]     reg_D  = 0;
   reg signed [35:0]     reg_M  = 0;
   reg signed [47:0]     reg_P  = 0;
   reg                   reg_CI = 0, reg_CO = 0;

   wire [7:0]            mux_OP = OPMODEREG  ? reg_OP : OPMODE;
   wire                  mux_CY = CARRYINSEL ? mux_OP[5] : CARRYIN;
   wire                  mux_CI = CARRYINREG ? reg_CI : mux_CY;
   wire                  mux_CO = CARRYINREG ? reg_CO : sum_CO;

   wire signed [17:0]    mux_A0 = A0REG ? reg_A0 : A;
   wire signed [17:0]    mux_A1 = A1REG ? reg_A1 : mux_A0;
   wire signed [17:0]    mux_B0 = B0REG ? reg_B0 : B;
   wire signed [17:0]    mux_B1 = B1REG ? reg_B1 : mux_preadd;
   wire signed [47:0]    mux_C  = CREG  ? reg_C  : C;
   wire signed [17:0]    mux_D  = DREG  ? reg_D  : D;
   wire signed [35:0]    mux_M  = MREG  ? reg_M  : prod;
   wire signed [47:0]    mux_P  = PREG  ? reg_P  : sum_P;


   //-------------------------------------------------------------------------
   //  If using the pre-adder (`OPMODE[4]`):
   wire signed [17:0]    pre_add    = mux_D + mux_B0;
   wire signed [17:0]    pre_sub    = mux_D - mux_B0;
   wire signed [17:0]    pre_addsub = mux_OP[6] ? pre_sub : pre_add;
   wire signed [17:0]    mux_preadd = mux_OP[4] ? pre_addsub : mux_B0;

   //  If using the multiplier (`OPMODE[1:0] == 2'b01`):
   wire signed [35:0]    prod = mux_A1 * mux_B1;

   reg signed [47:0]     mux_X;
   reg signed [47:0]     mux_Z;

   //  Accumulator result:
   wire signed [48:0]    add_P = mux_Z + mux_X + mux_CI;
   wire signed [48:0]    sub_P = mux_Z - mux_X - mux_CI;
   wire signed [47:0]    sum_P;
   wire                  sum_CO;

   assign {sum_CO, sum_P} = mux_OP[7] ? sub_P : add_P;


   //-------------------------------------------------------------------------
   //  Outputs depend on whether certain registers are being used:
   assign CARRYOUTF = CARRYOUT;
   assign CARRYOUT  = mux_CO;
   assign P = mux_P;
   assign M = mux_M;
   assign BCOUT = mux_B1;

   //-------------------------------------------------------------------------
   //  48-bit X and Z MUX's:
   always @*
     case (mux_OP[1:0])
       2'b00:   mux_X <= 48'b0;
       2'b01:   mux_X <= {{12{mux_M[35]}}, mux_M};
       2'b10:   mux_X <= mux_P;
       2'b11:   mux_X <= {mux_D[11:0], mux_A1, mux_B1};
       default: mux_X <= 'bx;
     endcase // case (mux_OP[1:0])

   always @*
     case (mux_OP[3:2])
       2'b00:   mux_Z <= 48'b0;
       2'b01:   mux_Z <= PCIN;
       2'b10:   mux_Z <= mux_P;
       2'b11:   mux_Z <= mux_C;
       default: mux_Z <= 'bx;
     endcase // case (mux_OP[3:2])


   //-------------------------------------------------------------------------
   //  DSP48A1 registers.
   //-------------------------------------------------------------------------
   //  System registers:
   always @(posedge CLK)
     if (RSTOPMODE)     reg_OP <= #DELAY 8'b0;
     else if (CEOPMODE) reg_OP <= #DELAY OPMODE;
     else               reg_OP <= #DELAY reg_OP;

   always @(posedge CLK)
     if (RSTCARRYIN)     {reg_CO, reg_CI} <= #DELAY 2'b0;
     else if (CECARRYIN) {reg_CO, reg_CI} <= #DELAY {sum_CO, mux_CY};
     else                {reg_CO, reg_CI} <= #DELAY {reg_CO, reg_CI};

   //-------------------------------------------------------------------------
   //  Input and result registers:
   always @(posedge CLK)
     if (RSTA)     {reg_A1, reg_A0} <= #DELAY 36'b0;
     else if (CEA) {reg_A1, reg_A0} <= #DELAY {mux_A0, A};
     else          {reg_A1, reg_A0} <= #DELAY {reg_A1, reg_A0};

   always @(posedge CLK)
     if (RSTB)     {reg_B1, reg_B0} <= #DELAY 36'b0;
     else if (CEB) {reg_B1, reg_B0} <= #DELAY {mux_preadd, B};
     else          {reg_B1, reg_B0} <= #DELAY {reg_B1, reg_B0};

   always @(posedge CLK)
     if (RSTC)     reg_C <= #DELAY 48'b0;
     else if (CEC) reg_C <= #DELAY C;
     else          reg_C <= #DELAY reg_C;

   always @(posedge CLK)
     if (RSTD)     reg_D <= #DELAY 18'b0;
     else if (CED) reg_D <= #DELAY D;
     else          reg_D <= #DELAY reg_D;

   always @(posedge CLK)
     if (RSTM)     reg_M <= #DELAY 36'b0;
     else if (CEM) reg_M <= #DELAY prod;
     else          reg_M <= #DELAY reg_M;

   always @(posedge CLK)
     if (RSTP)     reg_P <= #DELAY 48'b0;
     else if (CEP) reg_P <= #DELAY sum_P;
     else          reg_P <= #DELAY reg_P;


endmodule // DSP48A1
