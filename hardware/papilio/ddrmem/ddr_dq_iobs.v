/***************************************************************************
 *                                                                         *
 *   ddr_dq_iobs.v - A bank of FDDR IOBs for a DDR memory controller.      *
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
module ddr_dq_iobs (
	clock_i,
	clock_ni,
	
	send_i,
	capture_i,
	
	ddr_dqs_i,
	ddr_dq_io,
	
	data_i,
	data_o
);

input	clock_i;
input	clock_ni;

input	send_i;
input	capture_i;

input	ddr_dqs_i;
inout	[7:0]	ddr_dq_io;

input	[15:0]	data_i;
output	[15:0]	data_o;


ddr_io DIO0 (
	.clk_i (clock_i),
	.clk_ni (~clock_i),
	.send_i	(send_i),
	.cap_i	(capture_i),
	.dqs_i (ddr_dqs_i),
	.data_i	({data_i [8], data_i [0]}),
	.data_o	({data_o [8], data_o [0]}),
	.ddr_io	(ddr_dq_io [0])
);


ddr_io DIO1 (
	.clk_i (clock_i),
	.clk_ni (~clock_i),
	.send_i	(send_i),
	.cap_i	(capture_i),
	.dqs_i (ddr_dqs_i),
	.data_i	({data_i [9], data_i [1]}),
	.data_o	({data_o [9], data_o [1]}),
	.ddr_io	(ddr_dq_io [1])
);


ddr_io DIO2 (
	.clk_i (clock_i),
	.clk_ni (~clock_i),
	.send_i	(send_i),
	.cap_i	(capture_i),
	.dqs_i (ddr_dqs_i),
	.data_i	({data_i [10], data_i [2]}),
	.data_o	({data_o [10], data_o [2]}),
	.ddr_io	(ddr_dq_io [2])
);


ddr_io DIO3 (
	.clk_i (clock_i),
	.clk_ni (~clock_i),
	.send_i	(send_i),
	.cap_i	(capture_i),
	.dqs_i (ddr_dqs_i),
	.data_i	({data_i [11], data_i [3]}),
	.data_o	({data_o [11], data_o [3]}),
	.ddr_io	(ddr_dq_io [3])
);


ddr_io DIO4 (
	.clk_i (clock_i),
	.clk_ni (~clock_i),
	.send_i	(send_i),
	.cap_i	(capture_i),
	.dqs_i (ddr_dqs_i),
	.data_i	({data_i [12], data_i [4]}),
	.data_o	({data_o [12], data_o [4]}),
	.ddr_io	(ddr_dq_io [4])
);


ddr_io DIO5 (
	.clk_i (clock_i),
	.clk_ni (~clock_i),
	.send_i	(send_i),
	.cap_i	(capture_i),
	.dqs_i (ddr_dqs_i),
	.data_i	({data_i [13], data_i [5]}),
	.data_o	({data_o [13], data_o [5]}),
	.ddr_io	(ddr_dq_io [5])
);


ddr_io DIO6 (
	.clk_i (clock_i),
	.clk_ni (~clock_i),
	.send_i	(send_i),
	.cap_i	(capture_i),
	.dqs_i (ddr_dqs_i),
	.data_i	({data_i [14], data_i [6]}),
	.data_o	({data_o [14], data_o [6]}),
	.ddr_io	(ddr_dq_io [6])
);


ddr_io DIO7 (
	.clk_i (clock_i),
	.clk_ni (~clock_i),
	.send_i	(send_i),
	.cap_i	(capture_i),
	.dqs_i (ddr_dqs_i),
	.data_i	({data_i [15], data_i [7]}),
	.data_o	({data_o [15], data_o [7]}),
	.ddr_io	(ddr_dq_io [7])
);


endmodule	// ddr_dq_iobs
