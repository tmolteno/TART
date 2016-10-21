`timescale 1ns/100ps
/*
 * Module      : verilog/xilinx/RAMB8X32_SDP.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Wrapper around the a 1kb Xilinx block SRAM primitive, in its SDP mode, so
 * that it is easier to instantiate multiple instances, to get larger bit-
 * widths.
 * 
 * NOTE:
 *  + simulate with `-D__icarus` to use a behavioural version;
 * 
 * TODO:
 *  + ignores the `INITx` parameters for 1kb SRAM's (see UG383);
 *  + currently untested;
 * 
 * Changelog:
 *  + 19/07/2016  --  initial file;
 * 
 */

/*
 Instantiation template:

RAMB8X32_SDP
  #(.DELAY(3)) RAMB8X32_SDP_inst
   (.WCLK(WCLK),
    .WE(WE),
    .WADDR(WADDR),
    .DI(DI),
    .RCLK(RCLK),
    .CE(CE),
    .RADDR(RADDR),
    .DO(DO)
    );
*/

module RAMB8X32_SDP #(parameter DELAY = 3)
   (
    input         WCLK, // Write port
    input         WE,
    input [7:0]   WADDR,
    input [31:0]  DI,
    input         RCLK, // Read port
    input         CE,
    input [7:0]   RADDR,
    output [31:0] DO
    );


`ifdef __icarus
   //-------------------------------------------------------------------------
   //  Behavioural description of the `RAMB8X32_SDP` core.
   //-------------------------------------------------------------------------
   reg [31:0]     sram[0:255];
   reg [31:0]     reg_DO = 0;

   assign DO = reg_DO;

   always @(posedge WCLK)
     if (WE) sram[WADDR] <= #DELAY DI;

   always @(posedge RCLK)
     if (CE) reg_DO <= #DELAY sram[RADDR];


`else
   //-------------------------------------------------------------------------
   //  Xilinx RAMB8BWER primitive, and in SDP mode.
   //-------------------------------------------------------------------------
   wire [12:0]   RADR = {RADDR, 5'b0};
   wire [12:0]   WADR = {WADDR, 5'b0};

   RAMB8BWER
     #(.DATA_WIDTH_A(36),
       .DATA_WIDTH_B(36),
       .DOA_REG(0),
       .DOB_REG(0),
       .EN_RSTRAM_A("FALSE"),
       .EN_RSTRAM_B("FALSE"),
       .INIT_A(18'h00000),
       .INIT_B(18'h00000),
       .INIT_FILE("NONE"),
       .RAM_MODE("SDP"),
       .RSTTYPE("SYNC"),
       .RST_PRIORITY_A("CE"),
       .RST_PRIORITY_B("CE"),
       .SIM_COLLISION_CHECK("NONE"),
       .SRVAL_A(18'h00000),
       .SRVAL_B(18'h00000),
       // WRITE_MODE_A/WRITE_MODE_B: "WRITE_FIRST", "READ_FIRST", or "NO_CHANGE" 
//        .WRITE_MODE_A("READ_FIRST"),
//        .WRITE_MODE_B("READ_FIRST") 
       .WRITE_MODE_A("WRITE_FIRST"),
       .WRITE_MODE_B("WRITE_FIRST") 
   )
   SRAM0
     ( // Write port:
       .CLKAWRCLK(WCLK),        // 1-bit input: Write clock
       .ENAWREN(WE),            // 1-bit input: Write enable
       .ADDRAWRADDR(WADR),      // 13-bit input: Write address
       .DIADI(DI[15:0]),        // 16-bit input: LSB data
       .DIPADIP(2'b00),         // 2-bit input: LSB parity
       .DIBDI(DI[31:16]),       // 16-bit input: MSB data
       .DIPBDIP(2'b00),         // 2-bit input: MSB parity
       // Read port:
       .CLKBRDCLK(RCLK),        // 1-bit input: Read clock
       .ENBRDEN(CE),            // 1-bit input: Read enable
       .ADDRBRDADDR(RADR),      // 13-bit input: Read address
       .DOADO(DO[15:0]),        // 16-bit output: LSB data
       .DOPADOP(),              // 2-bit output: LSB parity
       .DOBDO(DO[31:16]),       // 16-bit output: MSB data
       .DOPBDOP(),              // 2-bit output: MSB parity
       // Not used in SDP-mode or with the output registers disabled:
       .REGCEA(1'b0),           // 1-bit input: A port register enable
       .RSTA(1'b0),             // 1-bit input: A port set/reset
       .WEAWEL({WE, WE}),          // 2-bit input: A port write enable
//        .WEAWEL(2'b11),          // 2-bit input: A port write enable
       .REGCEBREGCE(1'b1),      // 1-bit input: Register enable
       .RSTBRST(1'b0),          // 1-bit input: B port set/reset
       .WEBWEU({WE, WE})           // 2-bit input: B port write enable
//        .WEBWEU(2'b11)           // 2-bit input: B port write enable
   );
`endif // !`ifdef __icarus


endmodule // RAMB8X32_SDP
