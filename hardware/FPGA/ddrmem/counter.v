/***************************************************************************
 *                                                                         *
 *   counter.v - A simple paramaterizable counter.                         *
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
module counter (
		clock_i,
		reset_i,
		start_i,
		done_o
	);
	
	parameter	COUNT	= 200;
	parameter	BITS	= 8;
	
	input	clock_i;
	input	reset_i;
	input	start_i;
	
	output	done_o;
	
	reg	[BITS-1:0]	counter	= COUNT - 1;
	reg	started	= 0;
	
	
	always @(posedge clock_i)
	begin
		if (reset_i)
		begin
			started	<= 0;
			counter	<= COUNT - 1;
		end
		else
		begin
			if (start_i)
			begin
				counter	<= COUNT - 1;
				started	<= 1;
			end
			else if (counter == 0)
				started	<= 0;
			else if (started)
				counter	<= counter - 1;
		end
	end
	
	assign	done_o	= reset_i ? 0 : (counter == 0);
	
endmodule	// counter
