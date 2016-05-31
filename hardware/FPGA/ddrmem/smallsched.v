/***************************************************************************
 *                                                                         *
 *   smallsched.v - Generates read/write requests using a simple set of    *
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
module smallsched (
		clock_i,
		reset_i,
		enable_i,	// FIXME: Not implemented yet!
		
		raf_empty_ni,
		raf_one_i,
		raf_block_i,
		raf_read_o,
		raf_addr_i,
		
		waf_empty_ni,
		waf_read_o,
		waf_addr_i,
		// wdf_read_o,	// Send data to the IOBs when HI
		
		cmd_start_o,
		cmd_read_o,
		cmd_last_o,	// Auto-precharge after read/write if set
		cmd_susp_o,	// Suspend
		cmd_bank_o,
		cmd_row_o,
		cmd_col_o,
		
		ctl_read_i,	// Controller is issuing a read
		ctl_write_i,
		ctl_active_i,	// A row is being made ACTIVE
		
		rfc_req_i,	// The controller requests a refresh
		rfc_ack_o,
		rfc_end_i
	);
	
	input	clock_i;
	input	reset_i;
	input	enable_i;	// From DDR controller's `init_done' signal
	
	input	raf_empty_ni;	// From the user's Read Address FIFO
	input	raf_one_i;
	input	raf_block_i;
	output	raf_read_o;
	input	[22:0]	raf_addr_i;	// {bank, row, col}
	
	input	waf_empty_ni;	// From the user's Write Address FIFO
	output	waf_read_o;
	input	[22:0]	waf_addr_i;	// {bank, row, col}
	// output	wdf_read_o;
	
	// User commands that are ordered by the sequencing logic.
	output	cmd_start_o;	// Initiate a transfer, ACTIVATE a row
	output	cmd_read_o;	// Read if asserted, else write
	output	cmd_last_o;	// Causes an AP to be issued
	output	cmd_susp_o;	// A block data transfer has been suspended
	output	[1:0]	cmd_bank_o;
	output	[12:0]	cmd_row_o;
	output	[7:0]	cmd_col_o;	// The lowest bit is always zero, but this is done in the controller
	
	// Signals from the controller. These allow different timings to be
	// parameterised.
	input	ctl_read_i;	// Can be used to increment address counters etc
	input	ctl_write_i;
	input	ctl_active_i;
	
	// A refresh won't occur until allowed by this module. Since there is
	// a little bit of play allowed with refreshing, this module could
	// complete the current operation if it calculates it can.
	input	rfc_req_i;
	output	rfc_ack_o;
	input	rfc_end_i;
	
`define	SEQST_IDLE	3'b000
`define	SEQST_START	3'b001
`define	SEQST_RDWORD	3'b100
`define	SEQST_RDBLOCK	3'b101
`define	SEQST_SUSPEND	3'b111
`define	SEQST_WRITE	3'b010
	reg	[2:0]	state	= `SEQST_IDLE;
	
	always @(posedge clock_i)
	begin
		if (reset_i)
			state	<= `SEQST_IDLE;
		else
		begin
			case (state)
			
			endcase
		end
	end
	
endmodule	// smallsched
