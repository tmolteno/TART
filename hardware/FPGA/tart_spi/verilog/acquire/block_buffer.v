`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/block_buffer.v
 * Copyright   : (C) Tim Molteno     2013
 *             : (C) Max Scheel      2013
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
 * Buffers raw acquisition data, to then be written to DRAM.
 * 
 * NOTE:
 *  + sometimes XST synthesises the buffer using distributed SRAM's, but this
 *    is prevented by defining `__USE_EXPLICIT_BRAM` in the configuration
 *    file (added by Pat, 2016);
 * 
 */

`include "tartcfg.v"

`ifdef __icarus
 `undef __USE_EXPLICT_BRAM
`endif

module block_buffer
  #( parameter WIDTH = 24,
     parameter MSB   = WIDTH-1,
     parameter ABITS = 9,
     parameter ASB   = ABITS-1,
     parameter DEPTH = 1 << ABITS,
     parameter DELAY = 3)
   ( // read port:
     input          read_clock_i,
     input [ASB:0]  read_address_i,
     output [MSB:0] read_data_o,
     // write port:
     input          write_clock_i,
     input          write_enable_i,
     input [ASB:0]  write_address_i,
     input [MSB:0]  write_data_i
     );

`ifndef __USE_EXPLICT_BRAM
   reg [MSB:0] block_buffer [0:DEPTH-1];
   reg [MSB:0] block_data = {WIDTH{1'b0}};

   assign read_data_o = block_data;

 `ifdef __icarus
   //  Initialize all RAM cells to 0FF1CE at startup:
   integer j;
   initial
     for (j=0; j <DEPTH; j = j+1) block_buffer[j] = 24'h0FF1CE;
 `endif

   always @(posedge read_clock_i)
     block_data <= #DELAY block_buffer[read_address_i];

   always @(posedge write_clock_i)
     if (write_enable_i)
       block_buffer[write_address_i] <= #DELAY write_data_i;


`else // !`ifndef __USE_EXPLICT_BRAM
   wire [13:0]      ADDRA, ADDRB;
   wire [31:0]      a_data, b_data;
   wire [3:0]       web = {4{write_enable_i}};

   //  Extend addresses to the required 14-bits:
   assign ADDRA = {read_address_i , {14-ABITS{1'b0}}};
   assign ADDRB = {write_address_i, {14-ABITS{1'b0}}};

   //  Generate the individual byte-enables:
   assign web = {4{write_enable_i}};

   assign read_data_o = a_data[MSB:0];
   assign b_data = {{(32-WIDTH){1'b0}}, write_data_i};


   //-------------------------------------------------------------------------
   //  Spartan 6-specific block SRAM.
   //-------------------------------------------------------------------------
   RAMB16BWER
     #(  // DATA_WIDTH_A/DATA_WIDTH_B: 0, 1, 2, 4, 9, 18, or 36
         .DATA_WIDTH_A(36),
         .DATA_WIDTH_B(36),
         // DOA_REG/DOB_REG: Optional output register (0 or 1)
         .DOA_REG(0),
         .DOB_REG(0),
         // EN_RSTRAM_A/EN_RSTRAM_B: Enable/disable RST
         .EN_RSTRAM_A("FALSE"),
         .EN_RSTRAM_B("FALSE"),
         .RSTTYPE("SYNC"),
         .RST_PRIORITY_A("CE"),
         .RST_PRIORITY_B("CE"),
         .SIM_COLLISION_CHECK("NONE"),
         .SIM_DEVICE("SPARTAN6"),
         .INIT_A(36'h000000000),
         .INIT_B(36'h000000000),
         .INIT_FILE("NONE"),
         .SRVAL_A(36'h000000000),
         .SRVAL_B(36'h000000000),
         // WRITE_MODE_A/WRITE_MODE_B: "WRITE_FIRST", "READ_FIRST", or "NO_CHANGE" 
         .WRITE_MODE_A("WRITE_FIRST"),
         .WRITE_MODE_B("WRITE_FIRST")
       ) BLOCK_BUFFER0
       ( .ADDRA(ADDRA),
         .CLKA(read_clock_i),
         .ENA(1'b1),
         .REGCEA(1'b0),         // A port register clock enable input
         .RSTA(1'b0),           // A port register set/reset input
         .WEA(4'b0000),         // Port A byte-wide write enable input
         // Port A Data:
         .DIA(32'h0),           // 32-bit input: A port data input
         .DIPA(4'h0),           // 4-bit input: A port parity input
         .DOA(a_data),          // 32-bit output: A port data output
         .DOPA(),               // 4-bit output: A port parity output
         
         // Port B Address/Control Signals: 14-bit (each) input: Port B address and control signals
         .ADDRB(ADDRB),         // B port address input
         .CLKB(write_clock_i),  // B port clock input
         .ENB(1'b1),            // B port enable input
         .REGCEB(1'b0),         // B port register clock enable input
         .RSTB(1'b0),           // B port register set/reset input
         .WEB(web),             // Port B byte-wide write enable input
         // Port B Data:
         .DIB(b_data),          // 32-bit input: B port data input
         .DIPB(4'h0),           // 4-bit input: B port parity input
         .DOB(),                // 32-bit output: B port data output
         .DOPB()                // 4-bit output: B port parity output
         );
`endif // !`ifdef __USE_EXPLICT_BRAM


endmodule // block_buffer
