`timescale 1ns/100ps
/*
 * Module      : verilog/xilinx/RAM32X6_SDP.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Wrapper around the Xilinx RAM32M primitive, in its SDP mode, so that it is
 * easier to instantiate multiple instances, to get larger bit-widths.
 * 
 * NOTE:
 *  + simulate with `-D__icarus` to use a behavioural version;
 * 
 * TODO:
 *  + ignores the `INITx` parameters in Icarus mode;
 *  + currently untested;
 * 
 * Changelog:
 *  + 14/07/2016  --  initial file;
 * 
 */

/*
 Instantiation template:

RAM32X6_SDP
  #(.INITA(64'h0),
    .INITB(64'h0),
    .INITC(64'h0),
    .INITD(64'h0),
    .DELAY(3)) RAM32X6_SDP_inst
   (.WCLK(WCLK),
    .WE(WE),
    .WADDR(WADDR),
    .DI(DI),
    .RADDR(RADDR),
    .DO(DO)
    );
*/

module RAM32X6_SDP
  #( parameter INITA = 64'h0,   // TODO:
     parameter INITB = 64'h0,
     parameter INITC = 64'h0,
     parameter INITD = 64'h0,
     parameter DELAY = 3)
   (
    input        WCLK,
    input        WE,
    input [4:0]  WADDR,
    input [5:0]  DI,
    input [4:0]  RADDR,
    output [5:0] DO
    );


`ifdef __icarus
   //-------------------------------------------------------------------------
   //  Behavioural description of the `RAM32X6_SDP` core.
   //-------------------------------------------------------------------------
   reg [5:0]     sram[0:31];

   assign DO = sram[RADDR];

   always @(posedge WCLK)
     if (WE) sram[WADDR] <= #DELAY DI;


`else
   //-------------------------------------------------------------------------
   //  Xilinx RAM32M primitive, and in SDP mode.
   //-------------------------------------------------------------------------
   RAM32M
     #(  .INIT_A(INITA), .INIT_B(INITB), .INIT_C(INITC), .INIT_D(INITD)
         ) RAM32M0
       ( .DOA(DO[1:0]),
         .DOB(DO[3:2]),
         .DOC(DO[5:4]),
         .DOD(),

         .ADDRA(RADDR),
         .ADDRB(RADDR),
         .ADDRC(RADDR),
         .ADDRD(WADDR),

         .WCLK(WCLK),
         .WE(WE),
         .DIA(DI[1:0]),
         .DIB(DI[3:2]),
         .DIC(DI[5:4]),
         .DID(2'b00)
         );
`endif // !`ifdef __icarus


endmodule // RAM32X6_SDP
