/***************************************************************************
 *                                                                         *
 *   sfifo16.v - A synchronous First-In/First-Out circular buffer, n-bits  *
 *     wide with 16 entries.                                               *
 *                                                                         *
 *   Copyright (C) 2008 by Patrick Suggate                                 *
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

// TODO: Tweak this to easily run at 200 MHz.
`timescale 1ns/100ps
module sfifo16 (
	clock_i,
	reset_ni,
	
	read_i,
	write_i,
	data_i,
	data_o,
	
	empty_no,
	full_no
);

// Default width of 16-bits, but is user specifiable.
parameter	WIDTH	= 16;
parameter	MSB	= WIDTH - 1;

input		clock_i;
input		reset_ni;

input		read_i;
input		write_i;
input	[MSB:0]	data_i;
output	[MSB:0]	data_o;

output	empty_no;
output	full_no;
// output	almost_empty_o;	// TODO


// Pointers for the FIFO.
// Using 5-bit points enables full/empty conditions to be checked
// easily.
reg	[4:0]	f_start	= 5'h0;
reg	[4:0]	f_end	= 5'h0;

wire	ptrs_equal;

// synthesis attribute ram_style of mem is distributed ;
reg	[MSB:0]	mem [0:15];


// Status signals.
// TODO: These have long combinational logic path delays. They need
// to be shorter, which means registered?

/*
assign	#2 inc	= write_i && !read_i;
assign	#2 dec	= !write_i && read_i;
assign	#2 empty_w	= (f_start == f_end) && (read_i ^ write_i);
assign	#2 full_w	= (f_start [4] != f_end [4]) && (f_start [3:0] == f_end [3:0]) && (read_i ^ write_i);
*/

assign	ptrs_equal	= (f_start [3:0] == f_end [3:0]);
assign	empty_no	= !((f_start [4] == f_end [4]) && ptrs_equal);
assign	full_no		= !((f_start [4] != f_end [4]) && ptrs_equal);


assign data_o		= mem [f_start [3:0]];


// Dequeue data from the FIFO.
always @(posedge clock_i)
	if (!reset_ni)
		f_start	<= 0;
	else begin
		if (read_i && empty_no)
			f_start	<= f_start + 1;
		else
			f_start	<= f_start;
	end


// Add data to the FIFO.
always @(posedge clock_i)
	if (!reset_ni)
		f_end	<= 0;
	else begin
		if (write_i && full_no)
			f_end	<= f_end + 1;
		else
			f_end	<= f_end;
	end


always @ (posedge clock_i)
	if (write_i && full_no)
		mem [f_end [3:0]]  <= data_i;


`ifdef __icarus
	//-----------------------------------------------------------------------
	//  Simulation only:
	//  Zero the SRAM for simulation as this is the default state for
	//  Xilinx.
	initial begin : Init
		init_mem (0);
	end	// Init
	
	
	task init_mem;
		input	val;
		integer	val, n;
	begin : Init_Mem
		for (n = 0; n < 16; n = n + 1)
			mem[n]	= val;
	end	// Init_Mem
	endtask	// init_mem
`endif
	
	
endmodule	// sfifo16
