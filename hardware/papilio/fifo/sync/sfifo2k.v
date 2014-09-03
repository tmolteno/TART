/***************************************************************************
 *                                                                         *
 *   sfifo2k.v - Synchronous, 2kB FIFO.                                    *
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

// TODO: Add the pre-read module.
// Optional: MFSRs should clock faster.
`ifndef __icarus
`define __use_mfsrs
`else
`define	__sim_only
`endif


`timescale 1ns/100ps
module sfifo2k #(
`ifdef __use_mfsrs
	parameter	INIT	= 1,
`else
	parameter	INIT	= 0,
`endif
	parameter	WIDTH	= 32,
	parameter	SIZE	= 2048,
	parameter	PWIDTH	= 10,
	parameter	MSB	= WIDTH - 1,
	parameter	PSB	= PWIDTH - 1
) (
	input		clock_i,
	input		reset_ni,

	input		read_i,
	input		write_i,
	input	[MSB:0]	data_i,
`ifdef __sim_only
	output	[MSB:0]	data_o,
`else
	output	reg	[MSB:0]	data_o,
`endif
	output	reg	full_o	= 0,
	output	reg	empty_o	= 1,
	output		rack_ao,
	output		wack_ao,
	output		full_ao,
	output		empty_ao
);

reg	[MSB:0]	fmem [SIZE-1:0];

reg	[PWIDTH:0]	rd_ptr	= INIT;
reg	[PWIDTH:0]	wr_ptr	= INIT;

wire	[PWIDTH:0]	next_rd_ptr, next_wr_ptr;

wire	empty_w	= (wr_ptr == rd_ptr) || (next_rd_ptr == wr_ptr && read_i);
wire	full_w	= (wr_ptr [PSB:0] == rd_ptr [PSB:0]) && (wr_ptr [PWIDTH] != rd_ptr [PWIDTH]);

assign	#2 full_ao	= full_w;
assign	#2 empty_ao	= empty_w;

`ifdef __use_mfsrs
mfsr10 MFSR0 (
	.count_i	(rd_ptr),
	.count_o	(next_rd_ptr)
);

mfsr10 MFSR1 (
	.count_i	(wr_ptr),
	.count_o	(next_wr_ptr)
);
`else
assign	#2 next_rd_ptr	= rd_ptr + 1;
assign	#2 next_wr_ptr	= wr_ptr + 1;
`endif

assign	#2 rack_ao	= (rd_ptr != wr_ptr) && read_i;
assign	#2 wack_ao	= !full_w && write_i;


`ifdef __sim_only
assign	#2 data_o	= fmem[rd_ptr [PSB:0]];
`else
always @(posedge clock_i)
	data_o	<= #2 fmem[rd_ptr [PSB:0]];
`endif


// Pointer handling.
// TODO: Change to MFSRs?
always @(posedge clock_i)
	if (!reset_ni)
		rd_ptr	<= #2 INIT;
	else if (rack_ao)
		rd_ptr	<= #2 next_rd_ptr;

always @(posedge clock_i)
	if (!reset_ni)
		wr_ptr	<= #2 INIT;
	else if (wack_ao)
		wr_ptr	<= #2 next_wr_ptr;


// FIFO updating.
always @(posedge clock_i)
	if (write_i && !full_w)
		fmem [wr_ptr [PSB:0]]	<= #2 data_i;


// State signal generation.
always @(posedge clock_i)
	if (!reset_ni)
		full_o	<= #2 0;
	else
		full_o	<= #2 full_w;

always @(posedge clock_i)
	if (!reset_ni)
		empty_o	<= #2 1;
	else
		empty_o	<= #2 empty_w;


endmodule	// fifo2k
