`timescale 1ns/100ps
/*
 * Module      : rtl/wb_sram_port.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
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
 * Single-port SRAM interface, with Wishbone-like interconnect, and
 * parameterised by size and bit-width.
 * 
 * NOTE:
 *  + just the interface, the SRAM memories are assumed to be external;
 *  + doesn't support "classic" Wishbone cycles, but does support pipelined
 *    transactions;
 *  + supports burst transfers -- these are indicated by `bst_i` being
 *    asserted at the beginning of a transaction, and deasserted one cycle
 *    before the final (pipelined) `ack_o`;
 *  + if CHECK == 0 the CYC signal is ignored, reducing the circuit-size when
 *    using point-to-point interconnects;
 * 
 * TODO:
 * 
 */


/*
 Instantiation template:
 
    wb_sram_port
     #(  .WIDTH(DATA), // Data and address bit-widths
         .ABITS(ADDR),
         .TICKS(1),    // Read latency of the attached SRAM
         .READ (1),    // Support SRAM reads (0/1)?
         .WRITE(1),    // Support SRAM writes (0/1)?
         .USEBE(1),    // Use individual byte-enables (0/1)?
         .BYTES(1),    // Number of byte-enables
         .PIPED(1),    // Pipelined Wishbone (SPEC B4) transfers (0/1)?
         .ASYNC(1),    // Combinational vs. synchronous (0/1/2)?
         .CHECK(1),    // Sanity-checking of inputs (0/1)?
         .DELAY(3)     // 3ns simulation combinational delay
         ) SRAM_PORT
       ( .clk_i(clk),
         .rst_i(rst),

         .cyc_i(cyc),
         .stb_i(stb),
         .we_i (we),
         .ack_o(ack),
         .wat_o(wat),
         .rty_o(rty),
         .err_o(err),
         .adr_i(adr),
         .sel_i(sel),
         .dat_i(din),
         .dat_o(dout),

         .sram_ce_o(sram_ce),
         .sram_we_o(sram_we),
         .sram_be_o(sram_be),
         .sram_ad_o(sram_ad),
         .sram_do_i(sram_do),
         .sram_di_o(sram_di)
         );
*/

module wb_sram_port
  #(parameter WIDTH = 32,       // Data bit-width
    parameter MSB   = WIDTH-1,  // MSB of data
    parameter ABITS = 10,       // Address bit-width
    parameter ASB   = ABITS-1,  // MSB of address
    parameter READ  = 1,        // Support SRAM reads (0/1)?
    parameter WRITE = 1,        // Support SRAM writes (0/1)?
    parameter USEBE = 1,        // Use individual write byte-enables (0/1)?
    parameter BYTES = WIDTH>>3, // Number of byte-enables
    parameter BSB   = BYTES-1,  // MSB of byte-enables

    //  SRAM latency parameters:
    parameter TICKS = 1,        // Read latency, in cycles
    parameter TSB   = TICKS-1,  // MSB of ticks shifter-register

    //  Wishbone mode/settings parameters:
    parameter PIPED = 1,        // SRAM supports pipelined transfers (0/1)?
    parameter ASYNC = TICKS==0, // Asynchronous reads
    parameter CHECK = 1,

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (input          clk_i,
    input          rst_i,

    //  Wishbone (SPEC B4) bus interface:
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output         ack_o,
    output         wat_o, // Equiv. to WB `STALL_O`
    output         rty_o,
    output         err_o,
    input [ASB:0]  adr_i,
    input [BSB:0]  sel_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    //  SRAM control and data signals:
    output         sram_ce_o,
    output         sram_we_o,
    output [ASB:0] sram_ad_o,
    output [BSB:0] sram_be_o,
    input [MSB:0]  sram_do_i,
    output [MSB:0] sram_di_o
    );

   wire            cyc_w, stb_w, we_w;
   reg [TSB:0]     ack_out = {TICKS{1'b0}};
   wire [TICKS:0]  ack_nxt;


   //-------------------------------------------------------------------------
   //  Line drivers for the WB and SRAM interfaces.
   //-------------------------------------------------------------------------
   assign ack_o = ASYNC ? stb_w : ack_out[TSB];
   assign wat_o = 1'b0; // TICKS > 1 && !PIPED ? wat_out : 1'b0; // TODO
   assign rty_o = 1'b0;
   assign err_o = 1'b0;
   assign dat_o = READ  ? sram_do_i : {WIDTH{1'b0}};

   assign sram_ce_o = PIPED && READ ? cyc_w : stb_w;
   assign sram_we_o = WRITE ? we_w : 1'b0;
   assign sram_ad_o = adr_i;
   assign sram_be_o = USEBE ? {BYTES{sram_we_o}} & sel_i : sel_i; // TODO
   assign sram_di_o = WRITE ? dat_i : {WIDTH{1'b0}};

   //  Choose versions of these signals depending on the Wishbone mode
   //  settings.
   assign cyc_w = CHECK ? cyc_i : 1'b1;
   assign stb_w = cyc_w && stb_i;
   assign we_w  = stb_w && we_i;

   assign ack_nxt = {ack_out, stb_w};


   //-------------------------------------------------------------------------
   //  Output (shift-)registers.
   //-------------------------------------------------------------------------
   //  Wishbone requires that a single-cycle reset pulse is enough to reset a
   //  device.
   always @(posedge clk_i)
     if (rst_i && TICKS > 1)
       ack_out <= #DELAY {TICKS{1'b0}};
     else
       ack_out <= #DELAY ack_nxt[TSB:0];


endmodule // wb_sram_port
