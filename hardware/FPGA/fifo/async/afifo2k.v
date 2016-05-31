/***************************************************************************
 *                                                                         *
 *   afifo2k.v - A First-In/First-Out circular buffer, 36-bits wide with   *
 *     512 entries (2kB). This version is asynchronous.                    *
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

// FIXME: Since the Block RAM has an extra cycle of latency, add `ready' or
// `valid' signals.

`define	ADBITS	9
`define	ADMSB	`ADBITS - 1

`timescale 1ns/100ps
module afifo2k (
	reset_ni,
	
	rd_clk_i,
	rd_en_i,
	rd_data_o,
	
	wr_clk_i,
	wr_en_i,
	wr_data_i,
	
	rempty_o,
	wfull_o,
	rhalfish_o,
	whalfish_o
);

parameter	WIDTH	= 36;
parameter	ADDRESS	= 9;
parameter	MSB	= WIDTH - 1;
parameter	ASB	= ADDRESS - 1;

input	reset_ni;

input		rd_clk_i;
input		rd_en_i;
output	[MSB:0]	rd_data_o;

input		wr_clk_i;
input		wr_en_i;
input	[MSB:0]	wr_data_i;

output		rempty_o;
output		wfull_o;
output		rhalfish_o;
output		whalfish_o;


reg	[ASB:0]	bin_rd_ptr	= 0;
reg	[ASB:0]	bin_wr_ptr	= 0;

wire	[ASB:0]	next_rd_ptr;
wire	[ASB:0]	next_wr_ptr;

wire	[ASB:0]	gray_rd_ptr_w;
wire	[ASB:0]	gray_wr_ptr_w;

reg	[ASB:0]	gray_rd_ptr_r	= 0;
reg	[ASB:0]	gray_wr_ptr_r	= 0;

wire	aempty_n;
wire	afull_n;
reg	ahalf_n;
reg	rempty2	= 0;
reg	wfull2	= 0;
reg	rhalf2	= 0;
reg	whalf2	= 0;
reg	rempty_o	= 1;
reg	wfull_o		= 0;
reg	rhalfish_o	= 0;
reg	whalfish_o	= 0;

wire	[31:0]	dob, dia;
wire	[3:0]	dopb, dipa;


assign	rd_data_o	= WIDTH > 32 ? {dopb, dob} : dob;
assign	{dipa, dia}	= wr_data_i;


/////////////////////////////////////////////////////////////////////
// Maintain a copy of the counters/pointers in both formats.
//

assign	#2 next_rd_ptr	= !rempty_o ? bin_rd_ptr + rd_en_i : bin_rd_ptr;

always @(posedge rd_clk_i or negedge reset_ni)
	if (!reset_ni) begin
		bin_rd_ptr	<= #2 0;
		gray_rd_ptr_r	<= #2 0;
	end else begin
		// TODO: Quite a long combinational delay, so this will
		// limit the upper frequency.
		//if (rd_en_
		gray_rd_ptr_r	<= #2 gray_rd_ptr_w;
		bin_rd_ptr	<= #2 next_rd_ptr;
	end


assign	#2 next_wr_ptr	= !wfull_o ? bin_wr_ptr + wr_en_i : bin_wr_ptr;

always @(posedge wr_clk_i or negedge reset_ni)
	if (!reset_ni) begin
		bin_wr_ptr	<= #2 0;
		gray_wr_ptr_r	<= #2 0;
	end else begin
		gray_wr_ptr_r	<= #2 gray_wr_ptr_w;
		bin_wr_ptr	<= #2 next_wr_ptr;
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
wire	#2 dirset_n	= ~((gray_wr_ptr_r [ASB] ^ gray_rd_ptr_r [ASB-1]) & ~(gray_wr_ptr_r [ASB-1] ^ gray_rd_ptr_r [ASB]));
wire	#2 dirclr_n	= ~((~(gray_wr_ptr_r [ASB] ^ gray_rd_ptr_r [ASB-1]) & (gray_wr_ptr_r [ASB-1] ^ gray_rd_ptr_r [ASB])) | ~reset_ni);

always @(negedge dirset_n or negedge dirclr_n)
	if	(!dirclr_n)	direction <= #2 1'b0;
	else			direction <= #2 1'b1;

assign	#2 aempty_n	= ~((gray_wr_ptr_r == gray_rd_ptr_r) && !direction);
assign	#2 afull_n	= ~((gray_wr_ptr_r == gray_rd_ptr_r) && direction);

// Generate the synchronized empty_n signal.
always @(posedge rd_clk_i or negedge aempty_n)
	if (!aempty_n)	{rempty_o, rempty2}	<= #2 2'b11;
	else		{rempty_o, rempty2}	<= #2 {rempty2, ~aempty_n};


// Generate the synchronized full signal.
always @(posedge wr_clk_i or negedge reset_ni or negedge afull_n)
	if	(!reset_ni)	{wfull_o, wfull2}	<= #2 2'b00;
	else if	(!afull_n)	{wfull_o, wfull2}	<= #2 2'b11;
	else			{wfull_o, wfull2}	<= #2 {wfull2, ~afull_n};


// Quadrant based method for determining whether the FIFO is half-full-ish.
// TODO: This is very approximate. The error is 25% .  :)
wire	[1:0] qr	= gray_rd_ptr_r [ASB:ASB-1];
wire	[1:0] qw	= gray_wr_ptr_r [ASB:ASB-1];
always @(qr or qw)
	case ({qr, qw})
	4'b00_00:	ahalf_n	<= #2 1;
	4'b00_01:	ahalf_n	<= #2 1;
	4'b00_11:	ahalf_n	<= #2 0;
	4'b00_10:	ahalf_n	<= #2 0;
	
	4'b01_00:	ahalf_n	<= #2 0;
	4'b01_01:	ahalf_n	<= #2 1;
	4'b01_11:	ahalf_n	<= #2 1;
	4'b01_10:	ahalf_n	<= #2 0;
	
	4'b11_00:	ahalf_n	<= #2 0;
	4'b11_01:	ahalf_n	<= #2 0;
	4'b11_11:	ahalf_n	<= #2 1;
	4'b11_10:	ahalf_n	<= #2 1;
	
	4'b10_00:	ahalf_n	<= #2 1;
	4'b10_01:	ahalf_n	<= #2 0;
	4'b10_11:	ahalf_n	<= #2 0;
	4'b10_10:	ahalf_n	<= #2 1;
	endcase

always @(posedge rd_clk_i or negedge reset_ni or negedge ahalf_n)
	if (!reset_ni)	{rhalfish_o, rhalf2}	<= #2 2'b00;
	else if	(!ahalf_n)	{rhalfish_o, rhalf2}	<= #2 2'b11;
	else			{rhalfish_o, rhalf2}	<= #2 {rhalf2, ~ahalf_n};

always @(posedge wr_clk_i or negedge reset_ni or negedge ahalf_n)
	if (!reset_ni)	{whalfish_o, whalf2}	<= #2 2'b00;
	else if	(!ahalf_n)	{whalfish_o, whalf2}	<= #2 2'b11;
	else			{whalfish_o, whalf2}	<= #2 {whalf2, ~ahalf_n};


// Binary to Gray converters, 5-bits wide.
bin2gray #(
	.WIDTH	(ADDRESS)
) B2G0 (
	.bin_i	(next_rd_ptr),
	.gray_o	(gray_rd_ptr_w)
);


bin2gray #(
	.WIDTH	(ADDRESS)
) B2G1 (
	.bin_i	(next_wr_ptr),
	.gray_o	(gray_wr_ptr_w)
);


RAMB16_S36_S36 BRAM0 (
	.DIA	(dia),
	.DIPA	(dipa),
	.ADDRA	(gray_wr_ptr_r [ASB:0]),
	.ENA	(1'b1),
	.WEA	(wr_en_i),
	.SSRA	(1'b0),
	.CLKA	(wr_clk_i),
	.DOA	(),
	.DOPA	(),
	
	.DIB	(32'b0),
	.DIPB	(4'b0),
	.ADDRB	(gray_rd_ptr_w),	// TODO: Metastability?
	.ENB	(rd_en_i || rempty_o),
	.WEB	(1'b0),
	.SSRB	(1'b0),
	.CLKB	(rd_clk_i),
	.DOB	(dob),
	.DOPB	(dopb)
);


endmodule	// fifo16n
