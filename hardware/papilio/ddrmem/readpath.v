/***************************************************************************
 *                                                                         *
 *   readpath.v - Takes in DQS aligned data from the DDR device and stores *
 *     it in the read data FIFOs.                                          *
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

module readpath (
		clock_i,
		reset_i,
		
	);
	
	parameter	WIDTH	= 16;
	parameter	STROBES	= 2;
	
	input	clock_i;
	input	reset_i;
	
	input	[STROBES-1:0]	ddr_dqs;
	input	[WIDTH-1:0]	ddr_dq;
	
	input	read_i;
	output	ready_o;
	output	[WIDTH*2-1:0]	data_o;
	
	
	wire	ddr_dqs_0	= ddr_dqs [0];
	wire	ddr_dqs_1	= ddr_dqs [1];
	wire	ddr_dqs_2	= ~ddr_dqs [0];
	wire	ddr_dqs_3	= ~ddr_dqs [1];
	
	dqs_delay	DELAY0 (
		.dqs_i	(ddr_dqs_0),
		.delay	(delay),
		.dqs_o	(fifo_wr_en0)
	);
	
	dqs_delay	DELAY1 (
		.dqs_i	(ddr_dqs_1),
		.delay	(delay),
		.dqs_o	(fifo_wr_en1)
	);
	
	dqs_delay	DELAY2 (
		.dqs_i	(ddr_dqs_2),
		.delay	(delay),
		.dqs_o	(fifo_wr_en2)
	);
	
	dqs_delay	DELAY3 (
		.dqs_i	(ddr_dqs_3),
		.delay	(delay),
		.dqs_o	(fifo_wr_en3)
	);
	
	
	// TODO: Needs to be a little more advanced than this. Ready
	// shouldn't go high until both halves of the word have been
	// written.
	defparam	FIFO0.WIDTH	= WIDTH;
	fifo16 FIFO0 (
		.write_clock_i	(ddr_dqs_0),
		.write_i	(1'b1),
		.data_i		(ddr_dq [7:0])
		
		.read_clock_i	(clock_i),
		.read_i		(read_i),
		.data_o		(data_o [7:0]),
		.not_empty_o	(ready_o)
	);
	
endmodule	// readpath
