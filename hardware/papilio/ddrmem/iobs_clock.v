/***************************************************************************
 *                                                                         *
 *   iobs_clock.v - The clock pin IOBs of the DDR controller.              *
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
module iobs_clock (
	clock_i,
	reset_ni,
	
	enable_i,
	
	ddr_cke_o,
	ddr_ck_o,
	ddr_ck_no
);

input	clock_i;
input	reset_ni;

input	enable_i;

output	ddr_cke_o;
output	ddr_ck_o;
output	ddr_ck_no;


wire	ddr_ck_w;
wire	ddr_ck_nw;

// synthesis attribute of ddr_cke_o is true ;
reg	ddr_cke_o	= 0;


always @(posedge clock_i)
	ddr_cke_o	<= enable_i;


FDDRRSE fddr_ck_n (
	.Q	(ddr_ck_w),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(0),
	.D1	(1),
	.R	(!reset_ni),
	.S	(0)
);

FDDRRSE fddr_ck (
	.Q	(ddr_ck_nw),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(1),
	.D1	(0),
	.R	(0),
	.S	(!reset_ni)
);

OBUF ck_obuf	( .I (ddr_ck_w), .O (ddr_ck_o));
OBUF ckn_obuf	( .I (ddr_ck_nw), .O (ddr_ck_no));


endmodule	// iobs_clock
