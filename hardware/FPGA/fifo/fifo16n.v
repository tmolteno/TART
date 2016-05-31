/***************************************************************************
 *                                                                         *
 *   fifo16.v - A First-In/First-Out circular buffer, n-bits wide with     *
 *     16 entries. This version is asynchronous.                           *
 *                                                                         *
 *   Copyright (C) 2007 by Patrick Suggate                                 *
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

`define	ADBITS	4
`define	ADMSB	`ADBITS - 1

`timescale 1ns/100ps
module fifo16n (
	reset_ni,
	
	rd_clk_i,
	rd_en_i,
	rd_data_o,
	
	wr_clk_i,
	wr_en_i,
	wr_data_i,
	
	rempty_o,
	wfull_o
);

parameter	WIDTH	= 8;

input	reset_ni;

input	rd_clk_i;
input	rd_en_i;
output	[WIDTH-1:0]	rd_data_o;

input	wr_clk_i;
input	wr_en_i;
input	[WIDTH-1:0]	wr_data_i;

output	rempty_o;
output	wfull_o;


reg	[`ADMSB:0]	bin_rd_ptr	= 0;
reg	[`ADMSB:0]	bin_wr_ptr	= 0;

wire	[`ADMSB:0]	next_rd_ptr;
wire	[`ADMSB:0]	next_wr_ptr;

wire	[`ADMSB:0]	gray_rd_ptr_w;
wire	[`ADMSB:0]	gray_wr_ptr_w;

reg	[`ADMSB:0]	gray_rd_ptr_r	= 0;
reg	[`ADMSB:0]	gray_wr_ptr_r	= 0;

wire	aempty_n;
wire	afull_n;
reg	rempty2	= 0;
reg	wfull2	= 0;
reg	rempty_o	= 1;
reg	wfull_o		= 0;


/////////////////////////////////////////////////////////////////////
// FIFO memory.
//

// This is a block of distributed RAM for the FIFO.
// synthesis attribute ram_style of mem is distributed ;
// pragma attribute mem ram_block FALSE ;
reg    [WIDTH-1:0]  mem [0:15];

// TODO: Is it best to use the gray-encoded or binary encoded
// pointers?
always @ (posedge wr_clk_i)
begin
	if (wr_en_i && reset_ni)
		mem [gray_wr_ptr_r [`ADMSB:0]]	<= wr_data_i;
end

assign	rd_data_o	= mem [gray_rd_ptr_r [`ADMSB:0]];


/////////////////////////////////////////////////////////////////////
// Maintain a copy of the counters/pointers in both formats.
//

assign	next_rd_ptr	= !rempty_o ? bin_rd_ptr + rd_en_i : bin_rd_ptr;
always @(posedge rd_clk_i or negedge reset_ni)
begin
	if (!reset_ni)
	begin
		bin_rd_ptr	<= 0;
		gray_rd_ptr_r	<= 0;
	end
	else
	begin
		// TODO: Quite a long combinational delay, so this will
		// limit the upper frequency.
		//if (rd_en_
		gray_rd_ptr_r	<= gray_rd_ptr_w;
		bin_rd_ptr	<= next_rd_ptr;
	end
end


assign	next_wr_ptr	= !wfull_o ? bin_wr_ptr + wr_en_i : bin_wr_ptr;
always @(posedge wr_clk_i or negedge reset_ni)
begin
	if (!reset_ni)
	begin
		bin_wr_ptr	<= 0;
		gray_wr_ptr_r	<= 0;
	end
	else
	begin
		gray_wr_ptr_r	<= gray_wr_ptr_w;
		bin_wr_ptr	<= next_wr_ptr;
	end
end



/////////////////////////////////////////////////////////////////////
// Generate the `full_o' and `empty_no' status signals.
//

// Use quadrant detection since the read-pointer trails the write
// pointer by one quadrant.
// This section is based on the paper by Clifford E. Cummings and
// Peter Alfke called `Simulation and Synthesis Techniques for
// Asynchronous FIFO Design with Asynchronous Pointer Comparisons'.

reg	direction	= 0;
wire	dirset_n	= ~((gray_wr_ptr_r [`ADMSB] ^ gray_rd_ptr_r [`ADMSB-1]) & ~(gray_wr_ptr_r [`ADMSB-1] ^ gray_rd_ptr_r [`ADMSB]));
wire	dirclr_n	= ~((~(gray_wr_ptr_r [`ADMSB] ^ gray_rd_ptr_r [`ADMSB-1]) & (gray_wr_ptr_r [`ADMSB-1] ^ gray_rd_ptr_r [`ADMSB])) | ~reset_ni);

always @(negedge dirset_n or negedge dirclr_n)
	if	(!dirclr_n)	direction <= 1'b0;
	else			direction <= 1'b1;

assign	aempty_n	= ~((gray_wr_ptr_r == gray_rd_ptr_r) && !direction);
assign	afull_n		= ~((gray_wr_ptr_r == gray_rd_ptr_r) && direction);


// Generate the synchronized empty_n signal.
always @(posedge rd_clk_i or negedge aempty_n)
	if (!aempty_n)	{rempty_o, rempty2}	<= 2'b11;
	else		{rempty_o, rempty2}	<= {rempty2, ~aempty_n};


// Generate the synchronized full signal.
always @(posedge wr_clk_i or negedge reset_ni or negedge afull_n)
	if	(!reset_ni)	{wfull_o, wfull2}	<= 2'b00;
	else if	(!afull_n)	{wfull_o, wfull2}	<= 2'b11;
	else			{wfull_o, wfull2}	<= {wfull2, ~afull_n};


// Binary to Gray converters, 5-bits wide.
defparam	B2G0.WIDTH	= `ADBITS;
bin2gray B2G0 (
	.bin_i	(next_rd_ptr),
	.gray_o	(gray_rd_ptr_w)
);


defparam	B2G1.WIDTH	= `ADBITS;
bin2gray B2G1 (
	.bin_i	(next_wr_ptr),
	.gray_o	(gray_wr_ptr_w)
);


endmodule	// fifo16n
