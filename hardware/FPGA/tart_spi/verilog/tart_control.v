`timescale 1ns/100ps
/*
 * Module      : verilog/tart_control.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * TART's control registers module, connected via a Wishbone-like
 * interconnect.
 * 
 * Has system registers for:
 *   00  --  status register;
 *   01  --  extra status-flags;
 *   10  --  miscellaneous register;
 *   11  --  reset register;
 * 
 * NOTE:
 *  + supports both classic and pipelined transfers;
 * 
 * TODO:
 * 
 */

`include "tartcfg.v"

module tart_control
  #(parameter WIDTH = 8,
    parameter MSB   = WIDTH-1,
    parameter COUNT = 24,
    parameter CSB   = COUNT-1,
    parameter RTIME = 4,
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input          clk_i,
    input          rst_i,
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output         ack_o,
    input [1:0]    adr_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    input [MSB:0]  status_i,
    input          reset_ni,
    output         reset_o,
    input [CSB:0]  checksum_i
    );

   reg [MSB:0]     c_dat;
   reg             c_ack = 0;                 

   assign ack_o = c_ack;
   assign dat_o = c_dat;


   always @(posedge clk_i)
     if (rst_i) c_ack <= #DELAY 0;
     else       c_ack <= #DELAY cyc_i && stb_i && !c_ack;

   always @(posedge clk_i)
     if (cyc_i && stb_i && !we_i)
       case (adr_i)
         0: c_dat <= #DELAY status_i;
         1: c_dat <= #DELAY checksum_i[7:0];
         2: c_dat <= #DELAY checksum_i[15:8];
         3: c_dat <= #DELAY checksum_i[23:16];
       endcase // case (adr_i)


   //-------------------------------------------------------------------------
   //     RESET HANDLER
   //-------------------------------------------------------------------------
   wire            r_stb = stb_i && adr_i == 2'b11;

   wb_reset #( .WIDTH(WIDTH), .RTIME(RTIME) ) WB_RESET0
     ( .clk_i(clk_i),
       .rst_i(rst_i),
       .cyc_i(cyc_i),
       .stb_i(r_stb),
       .we_i (we_i),
       .ack_o(),
       .dat_i(dat_i),
       .dat_o(),

       .reset_ni(reset_ni),
       .reset_o (reset_o)
       );


endmodule // tart_control
