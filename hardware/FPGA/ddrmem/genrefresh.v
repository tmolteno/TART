/***************************************************************************
 *                                                                         *
 *   genrefresh.v - Generates the refresh signal for the DDR memory        *
 *     controller.                                                         *
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
module genrefresh (
		clock_i,
		reset_ni,
		refresh_o,
		ack_i
	);
	
	parameter	TICKS	= 20000;
	parameter	BITS	= 15;
	
	input	clock_i;
	input	reset_ni;
	output	refresh_o;
	input	ack_i;
	
	reg	[BITS-1:0]	counter;
	reg	refresh_o	= 0;
	always @(posedge clock_i)
	begin
		if (~reset_ni)
		begin
			counter		<= 0;
			refresh_o	<= 0;
		end
		else
		begin
			if (counter == TICKS)
			begin
				counter		<= 0;
				refresh_o	<= 1;
			end
			else
			begin
				if (ack_i)
					refresh_o	<= 0;
				counter	<= counter + 1;
			end
		end
	end
	
endmodule	// genrefresh
