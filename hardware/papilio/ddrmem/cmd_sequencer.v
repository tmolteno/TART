/***************************************************************************
 *                                                                         *
 *   cmd_sequencer.v - Generates read/write requests using a simple set of *
 *     rules. Reads have priority over writes. Single transfers take       *
 *     priority over block transfers.                                      *
 *                                                                         *
 *     Currently this module is optimised for simplicity (size).           *
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

// TODO: It is conceivable to make this module more complicated to allow for
// command re-ordering, grouping of single word reads, etc.

// If the DDR is in the middle of a block transfer and a word read or write
// command is detected, the block transfer is paused while the word transfer
// occurs. Even if multiple word transfers are executed, as long as these are
// in the same bank and row, they will complete far faster than the PCI bus
// can supply requests, so there is little danger of starving the re-draw
// operation.
module cmd_sequencer (
		clock_i,
		reset_i,
		enable_i,
		
		ddr_idle_i,
		
		raf_empty_ni,
		raf_word_i,
		raf_addr_i,
		
		waf_empty_ni,	// Data isn't needed ofr sequencing
		raf_word_i,
		waf_addr_i,
		
		read_cmd_o,
		write_cmd_o,
		intr_cmd_o,	// Interrupt the current operation
		ap_cmd_o,	// Auto-precharge after read/write if not a block transfer
		bank_addr_o,
		row_addr_o,
		col_addr_o
	);
	
	input	clock_i;
	input	reset_i;
	input	enable_i;
	
	input	ddr_idle_i;
	
	input	raf_empty_ni;	// From the user's Read Address FIFO
	input	raf_word_i;
	input	[14:0]	raf_addr_i;
	
	input	waf_empty_ni;	// From the user's Write Address FIFO
	input	waf_word_i;
	input	[14:0]	waf_addr_i;
	
	// User commands that are ordered by the sequencing logic.
	output	cmd_start_o;	// Initiate a transfer, ACTIVATE a row
	output	cmd_read_o;	// Read if asserted, else write
	output	cmd_last_o;	// Causes an AP to be issued
	input	cmd_exec_i;	// Can be used to increment address counters etc
	output	[1:0]	cmd_bank_o;
	output	[12:0]	cmd_row_o;
	output	[8:0]	cmd_col_o;
	
	
`define	SEQST_IDLE	3'b000
`define	SEQST_READ	3'b001
`define	SEQST_WRITE	3'b010
`define	SEQST_REFRESH	3'b100
	
	
	// A block burst transfer can be suspended if a single-word read or
	// write command arrives. If so, the following flag is set, and the
	// current column address is saved until the burst transfer is
	// resumed.
	reg	burst_suspend	= 0;
	reg	[8:0]	burst_addr	= 0;
	
	// If the next write command is to the same bank+row address as the
	// current write command, then make it part of the same operation.
	wire	same_row_bank	= ({bank_addr, row_addr} == waf_addr_i);
	
	// wire	burst_end	= cmd_col_o
	
	reg	[2:0]	state
	always @(posedge clock_i)
	begin
		if (reset_i)
			state	<= `SEQST_IDLE;
		else
		begin
			case (state)
			
			`SEQST_IDLE:
			begin
				if (raf_empty_ni)
					state	<= `SEQST_READ;
				else if (waf_empty_ni)
					state	<= `SEQST_WRITE;
				else
					state	<= state;
			end
			
			`SEQST_READ:;
			
			`SEQST_WRITE:;
			
			endcase
		end
	end
	
endmodule	// cmd_sequencer
