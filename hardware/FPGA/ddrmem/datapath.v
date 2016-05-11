/***************************************************************************
 *                                                                         *
 *   datapath.v - This module sends write data to the IOBs, and receives   *
 *     from the IOBs. It uses the sequencer and controller signals to      *
 *     synchronize the flow of data to and from the DDR memory, and        *
 *     generate the output owner signals.                                  *
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
module datapath (
		clock_i,
		reset_i,
		
		usr_data_i,
		usr_bes_ni,
		usr_owner_i,
		usr_data_o,
		usr_owner_o,
		usr_ready_o,
		
		ctl_start_i,
		ctl_block_i,
		ctl_suspend_i,
		ctl_read_i,
		ctl_write_i,
		
		ddr_send_o,
		ddr_bes_no,
		ddr_dqs_o,
		ddr_data_o,
		ddr_data_i
	);
	
	parameter	WIDTH	= 32;
	parameter	BYTES	= WIDTH / 8;
	parameter	OWNERS	= 2;
	
	input	clock_i;
	input	reset_i;
	
	input	[WIDTH-1:0]	usr_data_i;
	input	[BYTES-1:0]	usr_bes_ni;
	input	[OWNERS-1:0]	usr_owner_i;
	output	[WIDTH-1:0]	usr_data_o;
	output	[OWNERS-1:0]	usr_owner_o;
	output	usr_ready_o;
	
	input	ctl_start_i;
	input	ctl_block_i;
	input	ctl_suspend_i;
	input	ctl_read_i;	// Controller is issuing a read command
	input	ctl_write_i;
	
	output	ddr_send_o;
	output	[BYTES-1:0]	ddr_bes_no;
	output	ddr_dqs_o;
	output	[WIDTH-1:0]	ddr_data_o;
	input	[WIDTH-1:0]	ddr_data_i;
	
	
	reg	usr_ready_o	= 0;
	
	
	// Generate the signals needed for coordinating a DDR write.
	reg	delay0		= 0;
	reg	preamble	= 0;
	reg	ddr_dqs_o	= 0;
	reg	postamble	= 0;
	// reg	wdf_read_o	= 0;
	always @(posedge clock_i)
	begin
		if (reset_i)
		begin
			delay0		<= 0;
			preamble	<= 0;
			ddr_dqs_o	<= 0;
			postamble	<= 0;
		end
		else
		begin
			delay0		<= ctl_write_i;
			preamble	<= delay0;
			ddr_dqs_o	<= preamble;
			postamble	<= ddr_dqs_o;
		end
	end
	
	// Generate the ready signal when read data is present.
	reg	ready0 = 0, ready1 = 0, ready2 = 0, ready3 = 0;
	always @(posedge clock_i)
	begin
		if (reset_i)
			usr_ready_o	<= 0;
		else
		begin
			ready0	<= ctl_read_i;
			ready1	<= ready0;
			ready2	<= ready1;
			ready3	<= ready2;
			usr_ready_o	<= ready3;
		end
	end
	
	
	assign	ddr_send_o	= preamble | ddr_dqs_o | postamble;
	
	assign	ddr_bes_no	= usr_bes_ni;
	assign	ddr_data_o	= usr_data_i;
	assign	usr_data_o	= ddr_data_i;
	
	
endmodule	// datapath
