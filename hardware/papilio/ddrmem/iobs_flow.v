/***************************************************************************
 *                                                                         *
 *   iobs_flow.v - Instantiates the IOBs used for controlling the transfer *
 *     if data to/from the DDR SDRAM.                                      *
 *     This file is Xilinx Spartan III specific.                           *
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

`timescale 1ns/100ps
module iobs_flow (
	clock_i,
	reset_ni,
	
	ctl_dm_i,
	ctl_dqs_i,
	ctl_dqs_o,
	
	ddr_dm_o,
	ddr_dqs_io
);

input	clock_i;
input	reset_ni;

input	[1:0]	ctl_dm_i;
input	[1:0]	ctl_dqs_i;
output	[1:0]	ctl_dqs_o;

output	[1:0]	ddr_dm_o;
inout	[1:0]	ddr_dqs_io;


// synthesis attribute iob or ddr_dm_o is true ;
reg	[1:0]	ddr_dm_o	= 2'b11;

reg	send	= 0;
wire	send_w;

wire	[1:0]	ddr_dqs_w;


assign	ctl_dqs_o	= ddr_dq_io;	// FIXME: This isn't the best

assign	send_w	= send | ctl_dqs_i;


always @(posedge clock_i)
begin
	if (reset_ni)
		ddr_dm_o	<= ctl_dm_i;
end


always @(posedge clock_i)
begin
	if (!reset_ni)
		send	<= 1'b0;
	else
		send	<= #0.1 ctl_dqs_i;
end


FDDRRSE dqs_fddr0 (
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(1'b0),
	.D1	(1'b1),
	.R	(~send & ~reset_ni),
	.S	(1'b0),
	.Q	(ddr_dqs_w [0])
);

FDDRRSE dqs_fddr1 (
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(1'b0),
	.D1	(1'b1),
	.R	(~send & ~reset_ni),
	.S	(1'b0),
	.Q	(ddr_dqs_w [1])
);

// About 4.6ns of delay.
OBUFT dqs_obuf0 (
	.T	(ctl_dqs_i [0] & reset_ni),
	.I	(ddr_dqs_w [0]),
	.O	(ddr_dqs_io [0])
);

OBUFT dqs_obuf1 (
	.T	(ctl_dqs_i [1] & reset_ni),
	.I	(ddr_dqs_w [1]),
	.O	(ddr_dqs_io [1])
);


endmodule	// iobs_flow
