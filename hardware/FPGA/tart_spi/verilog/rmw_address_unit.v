`timescale 1ns/100ps
/*
 * Module      : verilog/rmw_address_unit.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
 * 
 * Generate the addresses for a sequential, Read-Modify-Write (RMW)
 * operations.
 * 
 * 
 * NOTE:
 * 
 * Changelog:
 *  + 01/08/2016  --  initial file;
 * 
 */

module rmw_address_unit
  #( parameter ABITS = 4,
     parameter ASB   = ABITS-1,
     parameter LOWER = 0,
     parameter UPPER = 11,
     parameter STEPS = 3,
     parameter DELAY = 3)
   (
    input              clk_i,
    input              rst_i,
    input              ce_i,
    output reg         vld_o = 0,
    output reg [ASB:0] rd_adr_o = LOWER,
    output             rd_wrap_o,
    output reg [ASB:0] wr_adr_o,
    output             wr_wrap_o
    );

   wire [ASB:0]        next_rd_adr = rd_wrap_o ? LOWER : rd_adr_o + 1;
   wire                rd_wrap_o = rd_adr_o == UPPER;
   wire                wr_wrap_o = wr_adr_o == UPPER;
   wire [ASB:0]        wt_adr;

   always @(posedge clk_i)
     if (rst_i)     rd_adr_o <= #DELAY LOWER;
     else if (ce_i) rd_adr_o <= #DELAY next_rd_adr;
     else           rd_adr_o <= #DELAY rd_adr_o;

   always @(posedge clk_i) begin
      vld_o    <= #DELAY ce;
      wr_adr_o <= #DELAY wt_adr;
//       wr_adr_o <= #DELAY ce ? wt_adr : wr_adr_o;
   end


   //-------------------------------------------------------------------------
   //  Generate output-valid signals.
   //-------------------------------------------------------------------------
   shift_reg
     #( .STEPS(STEPS-2)) CE0
       (.clk(clk_i), .ce(1'b1), .d(ce_i), .q(ce));


   //-------------------------------------------------------------------------
   //  Wait-state addresses.
   //-------------------------------------------------------------------------
   shift_reg
     #( .STEPS(STEPS-2)) WT0 [ASB:0]
       (.clk(clk_i), .ce(1'b1), .d(rd_adr_o), .q(wt_adr));


endmodule // rmw_address_unit

module shift_reg
  #(parameter STEPS = 2,
    parameter INIT  = {STEPS{1'b0}},
    parameter MSB   = STEPS-1,
    parameter DELAY = 3)
   (
    input  clk,
    input  ce,
    input  d,
    output q
    );

   reg [MSB:0] sr = INIT;
   reg         x;

   assign q = sr[MSB];

   always @(posedge clk)
     if (ce) {x, sr} <= #DELAY {sr, d};

endmodule // shift_reg
