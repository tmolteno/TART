/***************************************************************************
 *                                                                         *
 *   iobs_control.v - The control pin IOBs of the DDR controller.          *
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
module iobs_control (
	clock_i,
	reset_ni,
	
	ctl_cs_ni,
	ctl_ras_ni,
	ctl_cas_ni,
	ctl_we_ni,
	
	ddr_cs_no,
	ddr_ras_no,
	ddr_cas_no,
	ddr_we_no
);

input	clock_i;
input	reset_ni;

input	ctl_cs_ni;
input	ctl_ras_ni;
input	ctl_cas_ni;
input	ctl_we_ni;

output	ddr_cs_no;
output	ddr_ras_no;
output	ddr_cas_no;
output	ddr_we_no;


// synthesis attribute iob of ddr_cs_no is true ;
// synthesis attribute iob of ddr_ras_no is true ;
// synthesis attribute iob of ddr_cas_no is true ;
// synthesis attribute iob of ddr_we_no is true ;
reg	ddr_cs_no	= 1;
reg	ddr_ras_no	= 1;
reg	ddr_cas_no	= 1;
reg	ddr_we_no	= 1;


always @(posedge clock_i)
begin
	if (!reset_ni)
	begin
		ddr_cs_no	<= 1;
		ddr_ras_no	<= 1;
		ddr_cas_no	<= 1;
		ddr_we_no	<= 1;
	end
	else
	begin
		ddr_cs_no	<= ctl_cs_ni;
		ddr_ras_no	<= ctl_ras_ni;
		ddr_cas_no	<= ctl_cas_ni;
		ddr_we_no	<= ctl_we_ni;
	end
end


endmodule	// iobs_control
