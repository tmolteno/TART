/***************************************************************************
 *                                                                         *
 *   fifo32.v - A First-In/First-Out circular buffer, n-bits wide with     *
 *     32 entries.                                                         *
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
 
module fifo32 (
		read_clock_i,
		write_clock_i,
		reset_i,
		read_i,		//	Read so advance to next item
		write_i,	//	Write input data into FIFO
		data_i,
		data_o,
		almost_full_o,
		almost_empty_o,
		not_empty_o
	);
	
	//	Default width of 16-bits, but is user specifiable.
	parameter	FIFO_WIDTH	= 16;
	parameter	FIFO_MSB	= FIFO_WIDTH - 1;
	
	input	read_clock_i;
	input	write_clock_i;
	input	reset_i;
	input	read_i;
	input	write_i;
	input	[FIFO_MSB:0]	data_i;
	output	[FIFO_MSB:0]	data_o;
	output	almost_full_o;
	output	almost_empty_o;
	output	not_empty_o;
	
	
	//	Pointers for the FIFO
	reg		[4:0]	f_start		= 5'h0;
	reg		[4:0]	f_end		= 5'h0;
	
	
	//	Control signals
	assign	almost_full_o	= (f_start - 1) == f_end;
	assign	almost_empty_o	= (f_start + 1) == f_end;
	assign	not_empty_o		= f_start != f_end;
	
	
	//	Remove data from the FIFO
	always @(posedge read_clock_i)
	begin
		if (reset_i)
			f_start	<= 5'b0;
		else
		begin
			if (read_i)
				f_start	<= f_start + 1;
			else
				f_start	<= f_start;
		end
	end
	
	
	//	Add data to the FIFO
	always @(posedge write_clock_i)
	begin
		if (reset_i)
			f_end	<= 5'b0;
		else
		begin
			if (write_i)
				f_end	<= f_end + 1;
			else
				f_end	<= f_end;
		end
	end
	
	
	//synthesis attribute ram_style of mem is distributed
	reg    [FIFO_MSB:0]  mem[0:31]; //pragma attribute mem ram_block FALSE
	
	always @ (posedge write_clock_i)
	begin
		if (write_i)
		begin     
			mem[f_end]  <= data_i;  
		end
	end
	
	assign data_o = mem[f_start];  //unregistered read            
	
	
endmodule	//	fifo32
