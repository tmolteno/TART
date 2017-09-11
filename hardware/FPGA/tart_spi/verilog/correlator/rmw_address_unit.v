`timescale 1ns/100ps
/*
 * Module      : verilog/correlator/rmw_address_unit.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
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
     parameter TICKS = 3,
     parameter DELAY = 3)
   (
    input          clk_i,
    input          rst_i,
    input          ce_i,

    output [ASB:0] rd_adr_o,
    output         rd_wrap_o,
    output [ASB:0] wr_adr_o,
    output         wr_wrap_o
    );

   wire [ASB:0]    next_rd_adr, next_adr;
   wire [ASB:0]    wr_adr_w;
   reg [ASB:0]     wr_adr_r = LOWER;

   assign rd_wrap_o = rd_adr_o == UPPER;
   assign wr_wrap_o = wr_adr_o == UPPER;

   assign next_adr[0] = ~rd_adr_o[0];
   assign next_adr[1] = rd_adr_o  [0] == 1'b1   ? ~rd_adr_o[1] : rd_adr_o[1];
   assign next_adr[2] = rd_adr_o[1:0] == 2'b11  ? ~rd_adr_o[2] : rd_adr_o[2];
   assign next_adr[3] = rd_adr_o[2:0] == 3'b111 ? ~rd_adr_o[3] : rd_adr_o[3];
   assign next_rd_adr = rd_wrap_o ? LOWER : next_adr;


   //-------------------------------------------------------------------------
   //  Does the write-address lag the read-address by 3 or 4 cycles?
   //-------------------------------------------------------------------------
   assign wr_adr_o = TICKS == 4 ? wr_adr_r : TICKS == 3 ? wr_adr_w : {ABITS{1'bx}};

   always @(posedge clk_i)
     if (rst_i)
       wr_adr_r <= #DELAY LOWER;
     else
       wr_adr_r <= #DELAY wr_adr_w;


   //-------------------------------------------------------------------------
   //  Wait-states for the addresses.
   //-------------------------------------------------------------------------
`ifdef __icarus
   reg [ASB:0]         rd_adr_r = LOWER;

   assign rd_adr_o = rd_adr_r;

   always @(posedge clk_i)
     if (rst_i)     rd_adr_r <= #DELAY LOWER;
     else if (ce_i) rd_adr_r <= #DELAY next_rd_adr;
     else           rd_adr_r <= #DELAY rd_adr_o;
`else // !`ifdef __icarus

   FDRE
     #(.INIT(LOWER))
   FD0 [ASB:0]
     ( .C(clk_i),
       .R(rst_i),
       .CE(ce_i),
       .D(next_rd_adr),
       .Q(rd_adr_o)
       );
`endif // !`ifdef __icarus

   dbl_fd
     #(.WIDTH(ABITS),
       .INIT0(LOWER),
       .INIT1(LOWER),
       .DELAY(DELAY))
   DBL_FD0
     ( .clk(clk_i),
       .d(rd_adr_o),
       .q(wr_adr_w)
       );


endmodule // rmw_address_unit

module dbl_fd
  #(parameter WIDTH = 4,
    parameter MSB = WIDTH-1,
    parameter INIT0 = {WIDTH{1'b0}},
    parameter INIT1 = {WIDTH{1'b0}},
    parameter DELAY = 3)
   (
    input          clk,
    input [MSB:0]  d,
    output [MSB:0] q
    );

`ifdef __icarus
   reg [MSB:0]     q0 = INIT0, q1 = INIT1;

   assign q = q1;

   always @(posedge clk)
     {q1, q0} <= #DELAY {q0, d};

`else
   wire [MSB:0]    r;

   (* KEEP = "TRUE" *)
   FD #(.INIT(INIT0)) FD0[MSB:0] (.D(d), .Q(r), .C(clk));

   (* KEEP = "TRUE" *)
   FD #(.INIT(INIT1)) FD1[MSB:0] (.D(r), .Q(q), .C(clk));

`endif // !`ifdef __icarus


endmodule // dbl_fd

/*
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
   wire                ce;

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
*/
