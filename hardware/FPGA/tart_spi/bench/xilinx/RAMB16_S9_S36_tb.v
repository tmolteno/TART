`timescale 1ns/100ps
/*
 * Module      : bench/xilinx/RAMB16_S9_S36_tb.v
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
 * Testbench for the fake Xilinx SRAM primitive.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

module RAMB16_S9_S36_tb;

   parameter DELAY = 3;         // in ns
   parameter CYCLE = 10;        // in ns
   parameter THALF = CYCLE >> 1;
   parameter COUNT = 4;

   //-------------------------------------------------------------------------
   //  Clocks:
   //  TODO: Separate clock for each port?
	 reg		clock = 1;
	 reg		reset = 1;
	 
	 always #THALF clock <= ~clock;

   //-------------------------------------------------------------------------
   //  Signals to the block SRAM:
	 wire [7:0] doa;
	 reg [7:0]  dia;
	 reg [10:0] addra;
	 reg        ena = 0, wea = 0;
	 
	 wire [31:0] dob;
	 reg [31:0]  dib;
	 reg [8:0]   addrb;
	 reg         enb = 0, web = 0;


   //-------------------------------------------------------------------------
   //  Simulation sequence:
	 integer     count;
	 initial begin : INIT_AND_SIM
      $dumpfile ("ramb_tb.vcd");
      $dumpvars;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing global reset:\n", $time);
      #DELAY reset <= 1; #CYCLE reset <= 0;
		  
      //----------------------------------------------------------------------
      #(CYCLE << 2);
      $display("\n%8t: Performing some writes to port A:", $time);
		  #CYCLE ena <= 1; wea <= 1; enb <= 0; web <= 0; addra <= 0; dia <= 8'hA5;
		  for (count = COUNT << 2; count; count = count - 1) begin
			   #CYCLE dia <= ~dia;
			   addra <= addra + 1;
		  end
		  #CYCLE ena <= 0; wea <= 0; enb <= 0; web <= 0;
		  
      //----------------------------------------------------------------------
      #(CYCLE << 2);
      $display("\n%8t: Reading back data from port A:", $time);
		  #CYCLE ena <= 1; wea <= 0; enb <= 0; web <= 0; addra <= 0;

		  for (count = COUNT << 2; count; count = count - 1) begin
			   #CYCLE addra <= addra + 1;
		  end
		  #CYCLE ena <= 0; wea <= 0; enb <= 0; web <= 0;
		  
      //----------------------------------------------------------------------
      #(CYCLE << 2);
      $display("\n%8t: Reading back data from port B:", $time);
		  #CYCLE ena <= 0; wea <= 0; enb <= 1; web <= 0;

		  for (addrb = 0; addrb < COUNT-1; addrb = addrb + 1) #CYCLE;
		  #CYCLE ena <= 0; wea <= 0; enb <= 0; web <= 0;
		  
      //----------------------------------------------------------------------
		  #(CYCLE << 2) $finish;
	 end // block: INIT_AND_SIM


   //-------------------------------------------------------------------------
   //  Display data from the RAMB.
   reg r_ena = 0, r_enb = 0;
   always @(posedge clock) begin
      {r_enb, r_ena} <= #DELAY {enb, ena};
      if (r_ena)
        $display ("%10t: Port A data = %08b (0x%02x)", $time, doa, doa);
      if (r_enb)
        $display ("%10t: Port B data = %032b (0x%08x)", $time, dob, dob);
   end


   //-------------------------------------------------------------------------
   //  DEVICE UNDER TEST
   //-------------------------------------------------------------------------
	 RAMB16_S9_S36 block_ram0
     ( .DIA(dia),    //insert 1-bit data_in
		   .DIPA('bx),
		   .ADDRA(addra[10:0]),  //insert 14-bit address bus ([13:0])
		   .ENA(ena),    //insert enable signal
		   .WEA(wea),    //insert write enable signal
		   .SSRA(reset),   //insert set/reset signal
		   .CLKA(clock),   //insert clock signal
		   .DOA(doa),    //insert 1-bit data_out 
		   //.DOPA(),
		  
		   .DIB(dib),    //insert 32-bit data_in bus ([31:0]) 
		   .DIPB('bx),   //insert 4-bit parity data_in bus (35:32])
		   .ADDRB(addrb[8:0]),  //insert 9-bit address bus ([8:0])
		   .ENB(enb),    //insert enable signal
		   .WEB(web),    //insert write enable signal
		   .SSRB(reset),   //insert set/reset signal
		   .CLKB(clock),   //insert clock signal
		   .DOB(dob)
		   //.DOPB()    //insert 4-bit parity data_out bus ([35:32])
	     );

	 
endmodule	//	RAMB16_S9_S36_tb
