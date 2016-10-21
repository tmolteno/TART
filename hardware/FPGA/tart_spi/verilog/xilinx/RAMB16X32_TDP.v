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
 * Wrapper around the an 18kb Xilinx block SRAM primitive, in its TDP mode, so
 * that it is easier to instantiate multiple instances, to get larger bit-
 * widths.
 * 
 * NOTE:
 *  + simulate with `-D__icarus` to use a behavioural version;
 * 
 * TODO:
 *  + currently untested;
 * 
 * Changelog:
 *  + 21/10/2016  --  initial file;
 * 
 */

/*
 Instantiation template:

RAMB16X32_TDP
  #(.DELAY(3)) RAMB16X32_TDP_inst
   (.CLKA (CLKA),
    .ENA  (ENA),
    .WEA  (WEA),
    .ADDRA(ADDRA),
    .DIA  (DIA),
    .DOA  (DOA),

    .CLKB (CLKB),
    .ENB  (ENB),
    .WEB  (WEB),
    .ADDRB(ADDRB),
    .DIB  (DIB),
    .DOB  (DOB)
    );
*/

module RAMB16X32_TDP #(parameter DELAY = 3)
   (
    input         CLKA, // Port A
    input         ENA,
    input [3:0]   WEA,
    input [8:0]   ADDRA,
    input [31:0]  DIA,
    input [31:0]  DOA,

    input         CLKB, // Port B
    input         ENB,
    input [3:0]   WEB,
    input [8:0]   ADDRB,
    input [31:0]  DIB,
    output [31:0] DOB
    );


`ifdef __icarus
   //-------------------------------------------------------------------------
   //  Behavioural description of the `RAMB16X32_TDP` core.
   //-------------------------------------------------------------------------
   reg [7:0]     sram0[0:511];
   reg [7:0]     sram1[0:511];
   reg [7:0]     sram2[0:511];
   reg [7:0]     sram3[0:511];

   reg [31:0]    reg_DOB = 32'h0;
   reg [31:0]    reg_DOA = 32'h0;

   assign DOA = reg_DOA;
   assign DOB = reg_DOB;

   always @(posedge CLKA)
     if (ENA) begin
        if (WEA[0]) sram0[ADDRA] <= #DELAY DIA[7:0];
        if (WEA[1]) sram1[ADDRA] <= #DELAY DIA[15:8];
        if (WEA[2]) sram2[ADDRA] <= #DELAY DIA[23:16];
        if (WEA[3]) sram3[ADDRA] <= #DELAY DIA[31:24];
        reg_DOA <= #DELAY {sram3[ADDRA], sram2[ADDRA], sram1[ADDRA], sram0[ADDRA]};
     end

   always @(posedge CLKB)
     if (ENB) begin
        if (WEB[0]) sram0[ADDRB] <= #DELAY DIB[7:0];
        if (WEB[1]) sram1[ADDRB] <= #DELAY DIB[15:8];
        if (WEB[2]) sram2[ADDRB] <= #DELAY DIB[23:16];
        if (WEB[3]) sram3[ADDRB] <= #DELAY DIB[31:24];
        reg_DOB <= #DELAY {sram3[ADDRB], sram2[ADDRB], sram1[ADDRB], sram0[ADDRB]};
     end


`else
   //-------------------------------------------------------------------------
   //  Xilinx RAMB16BWER primitive, and in TDP mode.
   //-------------------------------------------------------------------------
   wire [13:0]   ADRA = {ADDRA, 5'b0};
   wire [13:0]   ADRB = {ADDRB, 5'b0};

   RAMB16BWER
     #(.DATA_WIDTH_A(36),
       .DATA_WIDTH_B(36),
       .DOA_REG(0),
       .DOB_REG(0),
       .EN_RSTRAM_A("FALSE"),
       .EN_RSTRAM_B("FALSE"),
       .INIT_A(36'h0),
       .INIT_B(36'h0),
       .INIT_FILE("NONE"),
       .RAM_MODE("TDP"),
       .RSTTYPE("SYNC"),
       .RST_PRIORITY_A("CE"),
       .RST_PRIORITY_B("CE"),
       .SIM_COLLISION_CHECK("NONE"),
       .SRVAL_A(36'h0),
       .SRVAL_B(36'h0),
       // WRITE_MODE_A/WRITE_MODE_B: "WRITE_FIRST", "READ_FIRST", or "NO_CHANGE" 
       .WRITE_MODE_A("WRITE_FIRST"),
       .WRITE_MODE_B("WRITE_FIRST") 
   )
   SRAM0
     ( // Port A:
       .CLKAWRCLK(CLKA),        // 1-bit input: Write clock
       .RSTA(1'b0),             // 1-bit input: A port set/reset
       .REGCEA(1'b0),           // 1-bit input: A port register enable
       .ENAWREN(ENA),           // 1-bit input: Write enable
       .WEAWEL(WEA),            // 4-bit input: A port write enable
       .ADDRAWRADDR(ADRA),      // 14-bit input: Write address
       .DIADI(DIA),             // 32-bit input: LSB data
       .DIPADIP(4'h0),          // 4-bit input: LSB parity
       .DOADO(DOA),             // 32-bit output: LSB data
       .DOPADOP(),              // 4-bit output: LSB parity
       // Port B:
       .CLKBRDCLK(CLKB),        // 1-bit input: Read clock
       .RSTBRST(1'b0),          // 1-bit input: B port set/reset
       .REGCEBREGCE(1'b0),      // 1-bit input: Register enable
       .ENBRDEN(ENB),           // 1-bit input: Read enable
       .WEBWEU(WEB),            // 4-bit input: B port write enable
       .ADDRBRDADDR(ADRB),      // 14-bit input: Read address
       .DIBDI(DIB),             // 32-bit input: MSB data
       .DIPBDIP(4'h0),          // 4-bit input: MSB parity
       .DOBDO(DOB),             // 32-bit output: MSB data
       .DOPBDOP()               // 4-bit output: MSB parity
       );
`endif // !`ifdef __icarus


endmodule // RAMB16X32_TDP
