/***************************************************************************
 *                                                                         *
 *   fifo15l.v - A First-In/First-Out circular buffer, n-bits wide with    *
 *     15 entries. For increased clock frequencies and reduced gate count, *
 *     LFSRs instead of normal adders are used. This has the side-effect   *
 *     of leaving one address unreachable in the 16 entry RAM.             *
 *                                                                         *
 *     The registered versions of the output flags require that the read   *
 *     and write clocks have the exact same phase and frequency.           *
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
module fifo15l (
		reset_i,
		
		rd_clk_i,
		rd_en_i,
		rd_data_o,
		
		wr_clk_i,
		wr_en_i,
		wr_data_i,
		
		empty_no,
		full_o,
		one_o,
		
		empty_nro,	// Registered versions
		full_ro,
		one_ro
	);
	
	parameter	WIDTH	= 16;
	
	input	reset_i;
	
	input	rd_clk_i;
	input	rd_en_i;
	output	[WIDTH-1:0]	rd_data_o;
	
	input	wr_clk_i;
	input	wr_en_i;
	input	[WIDTH-1:0]	wr_data_i;
	
	output	empty_no;
	output	full_o;
	output	one_o;		// One item in FIFO
	
	output	empty_nro;
	output	full_ro;
	output	one_ro;		// One item in FIFO
	
	reg	[WIDTH-1:0]	mem [0:15];
	
	// Pointers to the start and end of the queue.
	reg	[3:0]	f_start	= 4'b0001;
	reg	[3:0]	f_end	= 4'b0001;
	wire	[3:0]	next_st, next_end;
	
	// TODO: Use look-ahead and registered logic to reduce combinational
	// propagation delay.
	wire	empty_nw	= (f_start != f_end);
	wire	full_w		= (next_end == f_start);
	wire	one_w		= (next_st == f_end);
	
	assign	empty_no	= empty_nw;
	assign	full_o		= full_w;
	assign	one_o		= one_w;
	
	
	// Registered flags have a shorter combinatorial propagation delay.
	// These flags can only be used when `rd_clk_i' == `wr_clk_i'. If the
	// clocks are of a different phase or frequency, these flags will
	// probably be incorrect.
	// The logic for this is quite greedy so hopefully the synthesiser
	// will do a good job at trimming it if not used.
	wire	[3:0]	next_next_end;	// `f_end + 2' ish :)
	wire	[3:0]	next_next_st;
	wire	sorta_gettin_full	= (next_next_end == f_start);
	wire	sorta_two_left		= (next_next_st == f_end);
	reg	empty_nro	= 0;
	reg	full_ro		= 0;
	reg	one_ro		= 0;
	always @(posedge rd_clk_i)
	begin
		if (reset_i)
		begin
			empty_nro	<= 0;
			one_ro		<= 0;
			full_ro		<= 0;
		end
		else
		begin
			case ({sorta_gettin_full, full_w, rd_en_i, wr_en_i})
			4'b1001:	full_ro	<= 1;
			4'b0111:	full_ro	<= 1;
			4'b0100:	full_ro	<= 1;
			default:	full_ro	<= 0;
			endcase
			
			if (!empty_nw && !rd_en_i && wr_en_i)	// empty + write
				empty_nro	<= 1;
			else if (empty_nw && !one_w)	// items > 1
				empty_nro	<= 1;
			else if (one_w && rd_en_i && wr_en_i)
				empty_nro	<= 1;	// items == 1 + rd + wr
			else if (empty_nw && !rd_en_i && !wr_en_i)
				empty_nro	<= 1;
			else if (empty_nw && wr_en_i)	// items > 1 + wr
				empty_nro	<= 1;
			else
				empty_nro	<= 0;
			
			if (one_w && rd_en_i && wr_en_i)
				one_ro	<= 1;
			else if (one_w && !rd_en_i && !wr_en_i)
				one_ro	<= 1;
			else if (sorta_two_left && rd_en_i)
				one_ro	<= 1;
			else if (!empty_nw && wr_en_i)
				one_ro	<= 1;
			else
				one_ro	<= 0;
		end
	end
	
	
	// Reads take data off the start of the queue.
	always @(posedge rd_clk_i)
	begin
		if (reset_i)
			f_start	<= 4'b0001;
		else
		begin
			if (rd_en_i)
				f_start	<= next_st;
		end
	end
	
	// Writes add data to the end of the queue.
	always @(posedge wr_clk_i)
	begin
		if (reset_i)
			f_end	<= 4'b0001;
		else
		begin
			if (wr_en_i)
			begin
				mem [f_end]	<= wr_data_i;
//				$display ("%%%5t:  Writing: %h", $time, wr_data_i);
				f_end		<= next_end;
			end
		end
	end
	
	
	// Data comes off the start of the queue.
	assign	rd_data_o	= mem [f_start];
	
	
	wire	[WIDTH-1:0]	test1	= mem [1];
	wire	[WIDTH-1:0]	test2	= mem [2];
	wire	[WIDTH-1:0]	test4	= mem [4];
	
	// LFSRs have less logic layers than an equivalent garden-variety
	// binary adder.
	lfsr4 LFSR0 (
		.count_i(f_start),
		.count_o(next_st)
	);
	
	lfsr4 LFSR1 (
		.count_i(f_end),
		.count_o(next_end)
	);
	
	// Used for the registered `full_ro' flag.
	lfsr4 LFSR2 (
		.count_i(next_end),
		.count_o(next_next_end)
	);
	
	// Used for the registered `one_ro' flag.
	lfsr4 LFSR3 (
		.count_i(next_st),
		.count_o(next_next_st)
	);
	
	
	//-----------------------------------------------------------------------
	//	Simulation only:
	//	Zero the SRAM for simulation as this is the default state for Xilinx.
`ifdef __icarus
	initial begin : Init
		init_mem (0);
	end	//	Init
	
	
	task init_mem;
		input	val;
		integer	val, n;
	begin : Init_Mem
		for (n = 0; n < 16; n = n + 1)
			mem [n]	= val;
	end	//	Init_Mem
	endtask	//	init_mem
`endif
	
endmodule	// fifo15l
