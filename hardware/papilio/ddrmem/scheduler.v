/***************************************************************************
 *                                                                         *
 *   scheduler.v - Generates read/write requests using a simple set of     *
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
module scheduler (
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
	
	
	// If the next read or write commands are in the same bank+row
	// address as the current read or write command, then make it part of
	// the same operation.
	// FIXME: These are disabled ATM.
/*	wire	rd_same_rowbank	= ({cmd_bank_o, cmd_row_o} == raf_addr_i [22:8]) & raf_empty_ni & ~(raf_one_i & raf_read_o);	// TODO: Needs rd_req_i
	wire	wr_same_rowbank	= ({cmd_bank_o, cmd_row_o} == waf_addr_i [22:8]) & waf_empty_ni;
	*/
	wire	rd_same_rowbank	= 0;
	wire	wr_same_rowbank	= 0;
	
	// Asserts at the end of a block transfer (which is an entire column
	// read).
	wire	last_in_block	= (cmd_col_o == 9'h1FE);
	
	// Word read request.
	wire	rd_word_req	= (raf_empty_ni & ~raf_block_i);
	
	// Block read request.
	wire	rd_block_req	= (raf_empty_ni & raf_block_i);
	
	// These advance their respective read or write FIFOs
	reg	raf_read_o	= 0;
	reg	waf_read_o	= 0;
	
	// State variables.
	// A block burst transfer can be suspended if a single-word read or
	// write command arrives. If so, the following flag is set, and the
	// current column address is saved until the burst transfer is
	// resumed.
	reg	suspend	= 0;
	reg	block	= 0;
	reg	[2:0]	state	= `SEQST_IDLE;
	
	// Determine the next bank, row, and column addresses;
	wire	rd_next	= ~waf_empty_ni | (raf_empty_ni & ~raf_block_i);
//	wire	rd_next	= ~waf_empty_ni | rd_word_req;
	wire	[1:0]	next_bank	= rd_next ? raf_addr_i [22:21] : waf_addr_i [22:21];
	wire	[12:0]	next_row	= rd_next ? raf_addr_i [20:8] : waf_addr_i [20:8];
	wire	[7:0]	next_col	= rd_next ? raf_addr_i [7:0] : waf_addr_i [7:0];
	
	
	
	reg	cmd_read_o	= 0;	// Read if asserted, else write
	reg	cmd_last_o	= 0;	// Causes an AP to be issued
	reg	[1:0]	cmd_bank_o	= 0;
	reg	[12:0]	cmd_row_o	= 0;
	reg	[7:0]	cmd_col_o	= 0;
	
	// When a block transfer is suspended, save the address.
	reg	[22:0]	burst_addr	= 0;
	
	
	// Big fat state machine that handles the state transition logic and
	// determines the scheduling order of the incoming commands.
	always @(posedge clock_i)
	begin
		if (reset_i || !enable_i)
		begin
			state		<= `SEQST_IDLE;
			suspend		<= 0;
			block		<= 0;
			
			cmd_read_o	<= 0;
			cmd_last_o	<= 0;
		end
		else
		begin
			case (state)
			
			// The AUTO PRECHARGE bit is set when in this state
			// terminating any current actions.
			// Word transfers take priority over block transfers,
			// and reads take priority over writes.
			`SEQST_IDLE:
			begin
				// Issue an ACTIVE command.
				cmd_read_o	<= rd_next;
				cmd_last_o	<= 0;
				
				cmd_bank_o	<= next_bank;
				cmd_row_o	<= next_row;
				cmd_col_o	<= next_col;
				
				suspend		<= 0;
				block		<= raf_block_i;
				
				if (rfc_req_i || !rfc_end_i)	// Refresh
					state	<= state;
				else if (raf_empty_ni || waf_empty_ni)
				begin
					state	<= `SEQST_START;
					// Advance the read/write FIFO.
					waf_read_o	<= ~rd_next;
					raf_read_o	<= rd_next;
				end
				else
					state	<= state;
			end
			
			// This state is so an ACTIVE command is issued so
			// that the bank and row are activated.
			`SEQST_START:
			begin
				cmd_last_o	<= 0;
				raf_read_o	<= 0;
				waf_read_o	<= 0;
				
				if (ctl_active_i)
				begin	// ACTIVE is being issued
					if (cmd_read_o && (block || suspend))
						state	<= `SEQST_RDBLOCK;
					else if (cmd_read_o)
						state	<= `SEQST_RDWORD;
					else
						state	<= `SEQST_WRITE;
				end
				else	// Wait for ACTIVE
					state	<= state;
			end
			
			// The fastest way to PRECHARGE a row is using the
			// AUTO PRECHARGE command. This requires atleast one
			// read to take place.
			`SEQST_RDBLOCK:
			begin
				block	<= 0;
				suspend	<= 0;
				
				if (ctl_read_i)	// Column address for burst
					cmd_col_o	<= cmd_col_o + 1;
				else
					cmd_col_o	<= cmd_col_o;
				
				if (last_in_block && ctl_read_i)	// TODO
				begin
					state	<= `SEQST_IDLE;
					cmd_last_o	<= 1;
				end
				else if (rd_word_req || rfc_req_i || waf_empty_ni)
				begin
					state	<= `SEQST_SUSPEND;
					cmd_last_o	<= 1;
				end
				else
				begin
					state	<= state;
					cmd_last_o	<= 0;
				end
			end
			
			// Keep reading a word at a time. Once the current
			// read phase has benn completed, resume a suspended
			// block command if there is one, or else return to
			// the IDLE state.
			`SEQST_RDWORD:
			begin
				// If there is another read or write pending
				// that uses the same row and bank, do this
				// back to back, or else finish and return to
				// the idle state.
				
				// De-queue read commands when performing
				// back-to-back reads.
				if (rd_same_rowbank && !rfc_req_i)
				begin
					raf_read_o	<= 1;
					cmd_last_o	<= 0;
					cmd_col_o	<= raf_addr_i [7:0];
				end
				else
				begin
					raf_read_o	<= 0;
					cmd_last_o	<= 1;
				end
				
				if (ctl_read_i)
				begin
					if (rfc_req_i)
						state	<= `SEQST_IDLE;
					else if (rd_same_rowbank)
						state	<= state;
					else if (suspend)
					begin
						state	<= `SEQST_START;
						{cmd_bank_o, cmd_row_o, cmd_col_o}	<= burst_addr;
					end
					else
						state	<= `SEQST_IDLE;
				end
				else
					state	<= state;
			end
			
			// Suspend a block transfer so a single word read or
			// write can be performed. A suspend is also
			// triggered by a refresh occurring during a block
			// transfer. In this case, once the refresh ends, the
			// block transfer resumes.
			`SEQST_SUSPEND:
			begin
				// Save prev. address on entering suspend.
				if (!suspend)
					burst_addr	<= {cmd_bank_o, cmd_row_o, cmd_col_o};
				
				suspend	<= 1;
				if (rfc_req_i || !rfc_end_i)
					state	<= state;
				else if (rd_word_req | waf_empty_ni)
				begin
					state	<= `SEQST_START;
					cmd_bank_o	<= next_bank;
					cmd_row_o	<= next_row;
					cmd_col_o	<= next_col;
					cmd_read_o	<= rd_next;
					
					raf_read_o	<= rd_next;
					waf_read_o	<= ~rd_next;
				end
				else
				begin	// Refresh finished, resume.
					state	<= `SEQST_START;
					{cmd_bank_o, cmd_row_o, cmd_col_o}	<= burst_addr;
				end
				cmd_read_o	<= rd_next;
				
			end
			
			`SEQST_WRITE:
			begin
				// If there is another read or write pending
				// that uses the same row and bank, do this
				// back to back, or else finish and return to
				// the idle state.
				if (wr_same_rowbank && !rfc_req_i)
					cmd_last_o	<= 0;
				else
					cmd_last_o	<= 1;
				
				if (ctl_write_i)
				begin
					if (rfc_req_i)
						state	<= `SEQST_IDLE;
					else if (wr_same_rowbank)
						state	<= state;
					else if (suspend)
					begin
						state	<= `SEQST_START;
						{cmd_bank_o, cmd_row_o, cmd_col_o}	<= burst_addr;
					end
					else
						state	<= `SEQST_IDLE;
				end
				else
					state	<= state;
			end
			
			endcase
		end
	end
	
	
/*	FIXME TODO
	// Generate the next address.
	always @(posedge clock_i)
	begin
		case (state)
	*/
	
	// Generate the refresh acknowedge signal, `rfc_ack_o'.
	reg	rfc_ack_o	= 0;
	reg	rfc_triggered	= 0;
	// These are states in which a refresh can be acknowledged.
	wire	rfc_state	= (state == `SEQST_IDLE) | (state == `SEQST_SUSPEND);
	always @(posedge clock_i)
	begin
		if (reset_i)
		begin
			rfc_ack_o	<= 0;
			rfc_triggered	<= 0;
		end
		else
		begin
			// Generate an ACK once in a suitable state.
			if (rfc_state && rfc_req_i)
			begin
				if (~rfc_ack_o & ~rfc_triggered)
				begin
					rfc_ack_o	<= 1;
					rfc_triggered	<= 1;
				end
				else
					rfc_ack_o	<= 0;
			end
			else if (rfc_end_i)
				rfc_triggered	<= 0;
		end
	end
	
	
	// Notify the datapath of a block transfer suspend.
	// TODO: Write suspends are unimportant?
	assign	cmd_susp_o	= suspend;
	assign	cmd_start_o	= (state == `SEQST_START);
	
endmodule	// scheduler
