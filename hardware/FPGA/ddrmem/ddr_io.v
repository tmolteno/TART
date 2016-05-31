/***************************************************************************
 *                                                                         *
 *   ddr_io.v - FDDR IOBs for a DDR memory controller.                     *
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
module ddr_io (
		clk_i,
		clk_ni,
		
		send_i,
		cap_i,
		dqs_i,
		data_i,
		data_o,
		
		ddr_io
	);
	
	input	clk_i;
	input	clk_ni;
	
	input	send_i;
	input	cap_i;
	input	dqs_i;
	input	[1:0]	data_i;
	output	[1:0]	data_o;
	
	inout	ddr_io;	// synthesis attribute iob of ddr_io is true ;
	
	
	wire	ddr_dq_w;	// synthesis attribute iob of ddr_dq_w is true ;
	
	
	// synthesis attribute iob of odata_fddr0 is true ;
	FDDRRSE odata_fddr0 (
		.C0	(clk_ni),
		.C1	(clk_i),
		.CE	(1'b1),
		.D0	(data_i [0]),
		.D1	(data_i [1]),
		.R	(1'b0),
		.S	(1'b0),
		.Q	(ddr_dq_w)
	);
	
	// synthesis attribute iob of idata_fddr0 is true ;
	IFDDRRSE idata_fddr0 (
		.C0	(dqs_i),
		.C1	(~dqs_i),
		.CE	(cap_i),
		.D	(ddr_io),
		.R	(1'b0),
		.S	(1'b0),
		.Q0	(data_o [0]),
		.Q1	(data_o [1])
	);
	
//	assign	#4 ddr_io	= send_i ? ddr_dq_w : 1'bz;
	
	// synthesis attribute iob of data_obuf0 is true ;
	// synthesis attribute slew of odata_fddr0 is fast ;
	OBUFT #(4.0) data_obuf0 (
		.T	(send_i),
		.I	(ddr_dq_w),
		.O	(ddr_io)
	);
	
endmodule	// ddr_io
