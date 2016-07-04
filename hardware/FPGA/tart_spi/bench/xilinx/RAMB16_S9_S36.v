/***************************************************************************
 *                                                                         *
 *   RAMB16_S9_S36.v - Simulates the Xilinx primitive of the same name for *
 *     use with Icarus Verilog.                                            *
 *                                                                         *
 *   Copyright (C) 2005 by Patrick Suggate                                 *
 *   patrick@physics.otago.ac.nz                                           *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

`timescale 1ns/100ps
module RAMB16_S9_S36(
		DIA, DIPA, ADDRA, ENA, WEA, SSRA, CLKA, DOA, DOPA,
		DIB, DIPB, ADDRB, ENB, WEB, SSRB, CLKB, DOB, DOPB
	);
	
	input	[7:0]	DIA;
	input	DIPA;
	input	[10:0]	ADDRA;
	input	ENA;
	input	WEA;
	input	SSRA;
	input	CLKA;
	output	reg [7:0]	DOA;
	output	reg DOPA;
	
	input	[31:0]	DIB;
	input	[3:0]	DIPB;
	input	[8:0]	ADDRB;
	input	ENB;
	input	WEB;
	input	SSRB;
	input	CLKB;
	output	reg [31:0]	DOB;
	output	[3:0]	DOPB;
	
	reg 	[31:0]	ram_block_16k[511:0];
	reg		[3:0]	ram_block_2k[511:0];	//	Parity bits
	
	
	assign	DOPB	= 4'bzzzz;
	
	
	//	These parameters allow the block RAM to have values upon initialisation
	parameter	INIT_00 	= 256'h0;
	parameter	INIT_01 	= 256'h0;
	parameter	INIT_02 	= 256'h0;
	parameter	INIT_03 	= 256'h0;
	parameter	INIT_04 	= 256'h0;
	parameter	INIT_05 	= 256'h0;
	parameter	INIT_06 	= 256'h0;
	parameter	INIT_07 	= 256'h0;
	parameter	INIT_08 	= 256'h0;
	parameter	INIT_09 	= 256'h0;
	parameter	INIT_0A 	= 256'h0;
	parameter	INIT_0B 	= 256'h0;
	parameter	INIT_0C 	= 256'h0;
	parameter	INIT_0D 	= 256'h0;
	parameter	INIT_0E 	= 256'h0;
	parameter	INIT_0F 	= 256'h0;
	parameter	INIT_10 	= 256'h0;
	parameter	INIT_11 	= 256'h0;
	parameter	INIT_12 	= 256'h0;
	parameter	INIT_13 	= 256'h0;
	parameter	INIT_14 	= 256'h0;
	parameter	INIT_15 	= 256'h0;
	parameter	INIT_16 	= 256'h0;
	parameter	INIT_17 	= 256'h0;
	parameter	INIT_18 	= 256'h0;
	parameter	INIT_19 	= 256'h0;
	parameter	INIT_1A 	= 256'h0;
	parameter	INIT_1B 	= 256'h0;
	parameter	INIT_1C 	= 256'h0;
	parameter	INIT_1D 	= 256'h0;
	parameter	INIT_1E 	= 256'h0;
	parameter	INIT_1F 	= 256'h0;
	parameter	INIT_20 	= 256'h0;
	parameter	INIT_21 	= 256'h0;
	parameter	INIT_22 	= 256'h0;
	parameter	INIT_23 	= 256'h0;
	parameter	INIT_24 	= 256'h0;
	parameter	INIT_25 	= 256'h0;
	parameter	INIT_26 	= 256'h0;
	parameter	INIT_27 	= 256'h0;
	parameter	INIT_28 	= 256'h0;
	parameter	INIT_29 	= 256'h0;
	parameter	INIT_2A 	= 256'h0;
	parameter	INIT_2B 	= 256'h0;
	parameter	INIT_2C 	= 256'h0;
	parameter	INIT_2D 	= 256'h0;
	parameter	INIT_2E 	= 256'h0;
	parameter	INIT_2F 	= 256'h0;
	parameter	INIT_30 	= 256'h0;
	parameter	INIT_31 	= 256'h0;
	parameter	INIT_32 	= 256'h0;
	parameter	INIT_33 	= 256'h0;
	parameter	INIT_34 	= 256'h0;
	parameter	INIT_35 	= 256'h0;
	parameter	INIT_36 	= 256'h0;
	parameter	INIT_37 	= 256'h0;
	parameter	INIT_38 	= 256'h0;
	parameter	INIT_39 	= 256'h0;
	parameter	INIT_3A 	= 256'h0;
	parameter	INIT_3B 	= 256'h0;
	parameter	INIT_3C		= 256'h0;
	parameter	INIT_3D 	= 256'h0;
	parameter	INIT_3E 	= 256'h0;
	parameter	INIT_3F 	= 256'h0;
	
	//	Parity bits
	parameter	INITP_00	= 256'h0;
	parameter	INITP_01	= 256'h0;
	parameter	INITP_02	= 256'h0;
	parameter	INITP_03	= 256'h0;
	parameter	INITP_04	= 256'h0;
	parameter	INITP_05	= 256'h0;
	parameter	INITP_06	= 256'h0;
	parameter	INITP_07	= 256'h0;
	
	
	//	States for the state machine
	parameter	ST_IDLE		= 3'b0xx;
	parameter	ST_READ		= 3'b100;
	parameter	ST_WRITE	= 3'b110;
	parameter	ST_RESET	= 3'b1x1;
	
	
	always @(posedge CLKA)
	begin
		case ({ENA, WEA, SSRA})
		ST_READ:
			DOA <= get_byte(ram_block_16k[ADDRA[10:2]], ADDRA[1:0]);
			
		ST_WRITE:
			ram_block_16k[ADDRA[10:2]] <= set_byte(ram_block_16k[ADDRA[10:2]], ADDRA[1:0], DIA);
			
		ST_RESET:;
			
		default:;
		endcase
	end
	
	
	always @(posedge CLKB)
	begin
		case ({ENB, WEB, SSRB})
		ST_READ:
			DOB[31:0] <= ram_block_16k[ADDRB[8:0]];
			
		ST_WRITE:
			begin
				//$display("%% @time = %8t:  Writing data 0x%h", $time, DIB);
				ram_block_16k[ADDRB[8:0]] <= DIB[31:0];
			end
			
		ST_RESET:;
			
		default:;
		endcase
	end
	
	
	//	Outputs the byte at position 'byte_num' from 'word'
	function [7:0]	get_byte;
		input	[31:0]	word;
		input	[1:0]	byte_num;
		
		begin
			case (byte_num)
				2'b00:
					get_byte = word[7:0];
				2'b01:
					get_byte = word[15:8];
				2'b10:
					get_byte = word[23:16];
				2'b11:
					get_byte = word[31:24];
			endcase
		end
		
	endfunction	//	get_bit
	
	
	function [31:0]	set_byte;
		input	[31:0]	word;
		input	[1:0]	bit_num;
		input	[7:0]	byte;
		
		begin
			case (bit_num)
				2'b00:
					set_byte = { word[31:8], byte[7:0] };
				2'b01:
					set_byte = { word[31:16], byte[7:0], word[7:0] };
				2'b10:
					set_byte = { word[31:24], byte[7:0], word[15:0] };
				2'b11:
					set_byte = { byte[7:0], word[23:0] };
			endcase
		end
	endfunction
	
	
//--------------------------------------------------------------
//	Simulation stuff below
//
	integer	ii, jj;
	
	initial begin : Init
		//	Assign a random 'font'
		fill_ram_block_16k(3);
		
/*		#340
		$display;
		for (ii = 0; ii < 512; ii = ii + 1)
			$display("%b", ram_block_16k[ii]);*/
			
/*		$display;
		for (ii = 0; ii < 16; ii = ii + 1)
		begin
			for (jj = 0; jj < 8; jj = jj + 1)
				$write("%1b", get_bit(ram_block_16k[{9'd364 + ii[11:2]}], {ii[1:0], jj[2:0]}));
			$display;
		end*/
		
/*		#10
		$finish;*/
		
	end	//	Init
	
	
	//	Outputs the 32-bit long at position 'long_num' from 'big_long'
	function [31:0]	get_long;
		input	[255:0]	big_long;
		input	[2:0]	long_num;
		
		begin
			case (long_num)
			3'b000:
				get_long = big_long[31:0];
			3'b001:
				get_long = big_long[63:32];
			3'b010:
				get_long = big_long[95:64];
			3'b011:
				get_long = big_long[127:96];
			3'b100:
				get_long = big_long[159:128];
			3'b101:
				get_long = big_long[191:160];
			3'b110:
				get_long = big_long[223:192];
			3'b111:
				get_long = big_long[255:224];
			endcase
		end
		
	endfunction	//	get_long
	
	
	//	Outputs a 4-bit nibble at position 'nib_num' from 'big_num'
	function [3:0]	get_nibble;
		input	[255:0]	big_long;
		input	[5:0]	nib_num;
		
//		wire	[7:0]	high_bit = {nib_num, 2'b11};
//		wire	[7:0]	low_bit  = {nib_num, 2'b0};
		reg		[255:0]	temp;
		integer	i;
		begin
			temp	= big_long;
			for (i=0; i<nib_num; i=i+1)
			begin
				//	Shift right by four until the nibble we want is the low
				//	word
				temp	= {4'b0, temp[255:4]};
			end
			get_nibble	= temp[3:0];
		end
// 		begin
// 			get_nibble	<= big_long[high_bit:low_bit];
// 		end
	endfunction
	
	
	//	Used to initialise the 'ram_block_16k' for a test bench
	task fill_ram_block_16k;
		input	mode;
		integer	n, mode;
	
	begin : Fill_Ram_Block_16k
		
		for (n = 0; n < 512; n = n+1)
		begin
			case (mode)
				0:	ram_block_16k[n] = 32'h0000_0000;
				1:	ram_block_16k[n] = $random;
				2:	ram_block_16k[n] = 32'h3030_3030;
				3:;
			endcase
		end
		
		if (mode == 3)
		begin
			for ( n = 0; n < 8; n = n+1 )
			begin
				ram_block_16k[{6'h00, n[2:0]}] = get_long(INIT_00, n[2:0]);
				ram_block_16k[{6'h01, n[2:0]}] = get_long(INIT_01, n[2:0]);
				ram_block_16k[{6'h02, n[2:0]}] = get_long(INIT_02, n[2:0]);
				ram_block_16k[{6'h03, n[2:0]}] = get_long(INIT_03, n[2:0]);
				ram_block_16k[{6'h04, n[2:0]}] = get_long(INIT_04, n[2:0]);
				ram_block_16k[{6'h05, n[2:0]}] = get_long(INIT_05, n[2:0]);
				ram_block_16k[{6'h06, n[2:0]}] = get_long(INIT_06, n[2:0]);
				ram_block_16k[{6'h07, n[2:0]}] = get_long(INIT_07, n[2:0]);
				ram_block_16k[{6'h08, n[2:0]}] = get_long(INIT_08, n[2:0]);
				ram_block_16k[{6'h09, n[2:0]}] = get_long(INIT_09, n[2:0]);
				ram_block_16k[{6'h0A, n[2:0]}] = get_long(INIT_0A, n[2:0]);
				ram_block_16k[{6'h0B, n[2:0]}] = get_long(INIT_0B, n[2:0]);
				ram_block_16k[{6'h0C, n[2:0]}] = get_long(INIT_0C, n[2:0]);
				ram_block_16k[{6'h0D, n[2:0]}] = get_long(INIT_0D, n[2:0]);
				ram_block_16k[{6'h0E, n[2:0]}] = get_long(INIT_0E, n[2:0]);
				ram_block_16k[{6'h0F, n[2:0]}] = get_long(INIT_0F, n[2:0]);
				ram_block_16k[{6'h10, n[2:0]}] = get_long(INIT_10, n[2:0]);
				ram_block_16k[{6'h11, n[2:0]}] = get_long(INIT_11, n[2:0]);
				ram_block_16k[{6'h12, n[2:0]}] = get_long(INIT_12, n[2:0]);
				ram_block_16k[{6'h13, n[2:0]}] = get_long(INIT_13, n[2:0]);
				ram_block_16k[{6'h14, n[2:0]}] = get_long(INIT_14, n[2:0]);
				ram_block_16k[{6'h15, n[2:0]}] = get_long(INIT_15, n[2:0]);
				ram_block_16k[{6'h16, n[2:0]}] = get_long(INIT_16, n[2:0]);
				ram_block_16k[{6'h17, n[2:0]}] = get_long(INIT_17, n[2:0]);
				ram_block_16k[{6'h18, n[2:0]}] = get_long(INIT_18, n[2:0]);
				ram_block_16k[{6'h19, n[2:0]}] = get_long(INIT_19, n[2:0]);
				ram_block_16k[{6'h1A, n[2:0]}] = get_long(INIT_1A, n[2:0]);
				ram_block_16k[{6'h1B, n[2:0]}] = get_long(INIT_1B, n[2:0]);
				ram_block_16k[{6'h1C, n[2:0]}] = get_long(INIT_1C, n[2:0]);
				ram_block_16k[{6'h1D, n[2:0]}] = get_long(INIT_1D, n[2:0]);
				ram_block_16k[{6'h1E, n[2:0]}] = get_long(INIT_1E, n[2:0]);
				ram_block_16k[{6'h1F, n[2:0]}] = get_long(INIT_1F, n[2:0]);
				ram_block_16k[{6'h20, n[2:0]}] = get_long(INIT_20, n[2:0]);
				ram_block_16k[{6'h21, n[2:0]}] = get_long(INIT_21, n[2:0]);
				ram_block_16k[{6'h22, n[2:0]}] = get_long(INIT_22, n[2:0]);
				ram_block_16k[{6'h23, n[2:0]}] = get_long(INIT_23, n[2:0]);
				ram_block_16k[{6'h24, n[2:0]}] = get_long(INIT_24, n[2:0]);
				ram_block_16k[{6'h25, n[2:0]}] = get_long(INIT_25, n[2:0]);
				ram_block_16k[{6'h26, n[2:0]}] = get_long(INIT_26, n[2:0]);
				ram_block_16k[{6'h27, n[2:0]}] = get_long(INIT_27, n[2:0]);
				ram_block_16k[{6'h28, n[2:0]}] = get_long(INIT_28, n[2:0]);
				ram_block_16k[{6'h29, n[2:0]}] = get_long(INIT_29, n[2:0]);
				ram_block_16k[{6'h2A, n[2:0]}] = get_long(INIT_2A, n[2:0]);
				ram_block_16k[{6'h2B, n[2:0]}] = get_long(INIT_2B, n[2:0]);
				ram_block_16k[{6'h2C, n[2:0]}] = get_long(INIT_2C, n[2:0]);
				ram_block_16k[{6'h2D, n[2:0]}] = get_long(INIT_2D, n[2:0]);
				ram_block_16k[{6'h2E, n[2:0]}] = get_long(INIT_2E, n[2:0]);
				ram_block_16k[{6'h2F, n[2:0]}] = get_long(INIT_2F, n[2:0]);
				ram_block_16k[{6'h30, n[2:0]}] = get_long(INIT_30, n[2:0]);
				ram_block_16k[{6'h31, n[2:0]}] = get_long(INIT_31, n[2:0]);
				ram_block_16k[{6'h32, n[2:0]}] = get_long(INIT_32, n[2:0]);
				ram_block_16k[{6'h33, n[2:0]}] = get_long(INIT_33, n[2:0]);
				ram_block_16k[{6'h34, n[2:0]}] = get_long(INIT_34, n[2:0]);
				ram_block_16k[{6'h35, n[2:0]}] = get_long(INIT_35, n[2:0]);
				ram_block_16k[{6'h36, n[2:0]}] = get_long(INIT_36, n[2:0]);
				ram_block_16k[{6'h37, n[2:0]}] = get_long(INIT_37, n[2:0]);
				ram_block_16k[{6'h38, n[2:0]}] = get_long(INIT_38, n[2:0]);
				ram_block_16k[{6'h39, n[2:0]}] = get_long(INIT_39, n[2:0]);
				ram_block_16k[{6'h3A, n[2:0]}] = get_long(INIT_3A, n[2:0]);
				ram_block_16k[{6'h3B, n[2:0]}] = get_long(INIT_3B, n[2:0]);
				ram_block_16k[{6'h3C, n[2:0]}] = get_long(INIT_3C, n[2:0]);
				ram_block_16k[{6'h3D, n[2:0]}] = get_long(INIT_3D, n[2:0]);
				ram_block_16k[{6'h3E, n[2:0]}] = get_long(INIT_3E, n[2:0]);
				ram_block_16k[{6'h3F, n[2:0]}] = get_long(INIT_3F, n[2:0]);
			end
			for (n=0; n<64; n=n+1)
			begin
				//	9-bit address, 4-bit wide data
				ram_block_2k[{3'h0,n[5:0]}] = get_nibble(INITP_00, n);
				ram_block_2k[{3'h1,n[5:0]}] = get_nibble(INITP_01, n);
				ram_block_2k[{3'h2,n[5:0]}] = get_nibble(INITP_02, n);
				ram_block_2k[{3'h3,n[5:0]}] = get_nibble(INITP_03, n);
				ram_block_2k[{3'h4,n[5:0]}] = get_nibble(INITP_04, n);
				ram_block_2k[{3'h5,n[5:0]}] = get_nibble(INITP_05, n);
				ram_block_2k[{3'h6,n[5:0]}] = get_nibble(INITP_06, n);
				ram_block_2k[{3'h7,n[5:0]}] = get_nibble(INITP_07, n);
			end
		end
		
	end	//	Fill_Ram_Block_16k
	endtask	//	fill_ram_block_16k
	
	
endmodule	//	RAMB16_S9_S36
