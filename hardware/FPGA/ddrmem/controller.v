/***************************************************************************
 *                                                                         *
 *   controller.v - Controls the DDR memory. This module is responsible    *
 *     for initialising the DDR, refreshing it, and issuing reads, writes, *
 *     and idles.                                                          *
 *                                                                         *
 *     Currently this module is optimised for size.                        *
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

/*
// Memory timings for 133 MHz
`define	tRAS_COUNT	6
`define	tRAS_BITS	3

`define	tRC_COUNT	8
`define	tRC_BITS	3

`define	tRFC_COUNT	10
`define	tRFC_BITS	4
*/

// Memory timings for 100 MHz
`define	tRAS_COUNT	5
`define	tRAS_BITS	3

`define	tRC_COUNT	6
`define	tRC_BITS	3

`define	tRFC_COUNT	8
`define	tRFC_BITS	3

/* writeup
\section{controller}

This module bbdlad


*/

`timescale 1ns/100ps
module controller (
		clock_i,	// 133 MHz system clock
		reset_i,	// Goes high once signal
		
		ddr_cke_o,	// DDR controls and address signals
		
		ddr_cs_no,
		ddr_ras_no,
		ddr_cas_no,
		ddr_we_no,
		
		ddr_ba_o,
		ddr_a_o,
		
		init_done_o,
		
		cmd_start_i,	// Commands from the sequencing logic
		cmd_read_i,
		cmd_last_i,
		cmd_active_o,	// An ACTIVE command is being issued?
		cmd_exec_o,	// High whenever a READ or WRITE is executed
		cmd_bank_i,
		cmd_row_i,
		cmd_col_i,
		
		data_read_o,
		data_write_o,
		
		rfc_req_o,
		rfc_start_i,
		rfc_done_o
	);
	
	parameter	RFC_TICKS	= 1040;	// 133 MHz
	
	input	clock_i;	// 133 MHz (may work lower than this)
	input	reset_i;	// Hold reset high for 200 us after clocks are stable
	
	// Used for auto-refresh
	output	ddr_cke_o;
	
	// DDR control signals
	output	ddr_cs_no;
	output	ddr_ras_no;
	output	ddr_cas_no;
	output	ddr_we_no;
	
	// DDR address pins
	output	[1:0]	ddr_ba_o;	// Bank address
	output	[12:0]	ddr_a_o;	// Address (row and column, A10 is auto-precharge)
	
	// Lets the command sequencer know when the DRAM is initialised.
	output	init_done_o;
	
	// User commands that are ordered by the sequencing logic.
	input	cmd_start_i;
	input	cmd_read_i;	// Used with `cmd_start_i'
	input	cmd_last_i;	// Causes an AP to be issued
	output	cmd_active_o;	// Used to advance the scheduler state
	output	cmd_exec_o;	// Can be used to increment address counters etc
	input	[1:0]	cmd_bank_i;
	input	[12:0]	cmd_row_i;
	input	[7:0]	cmd_col_i;	// this col. addr. excludes bit-0
	
	output	data_read_o;
	output	data_write_o;
	
	// The sequencer can play with refresh timing if needed.
	output	rfc_req_o;	// DRAM controller requests a refresh operation
	input	rfc_start_i;	// Refresh reques acknowledged and start
	output	rfc_done_o;	// Refresh operation completed
	
	
	
// DDR states. Non-command (like initialisation and waiting for refresh to
// complete) states are preceded by a one
`define	DDRST_INIT	4'b1111
`define	DDRST_IDLE	4'b0111
`define	DDRST_NOP	4'b0111
`define	DDRST_LOADMODE	4'b0000
`define	DDRST_MODEWAIT	4'b1000
`define	DDRST_RFCSTART	4'b0001
`define	DDRST_RFCWAIT	4'b1001
`define	DDRST_ACTIVE	4'b0011
`define	DDRST_READ	4'b0101
`define	DDRST_WRITE	4'b0100
`define	DDRST_PRECHARGE	4'b0010
`define	DDRST_PREWAIT	4'b1010
	
	
	/////////////////////////////////////////////////////////////////////
	// Registers and wires for the controller.
	//
	
	// These values depend on `reset_i'.
	wire	ddr_cke_o;
	wire	ddr_cs_no;
	
/*	// These will be in IOBs.
	reg	ddr_ras_no	= 1;
	reg	ddr_cas_no	= 1;
	reg	ddr_we_no	= 1;
	
	reg	[1:0]	ddr_ba_o	= 0;
	reg	[12:0]	ddr_a_o		= 0;
	*/
	
	// These are calculated in CLBs, then passed to the IOBs above so
	// that the output delay is lower and the clock-rate higher.
	reg	ddr_ras_n	= 1;	// NOP command
	reg	ddr_cas_n	= 1;
	reg	ddr_we_n	= 1;
	
	reg	[1:0]	ddr_ba	= 0;
	reg	[12:0]	ddr_a	= 0;
	
	// Am I being a little unconventional here? I use `next_state' with
	// `state' to allow NOPs to be inserted into commands. This works by
	// jumping into `DDRST_IDLE' for one clock, and then jumping to
	// `next_state'. This cuts down on conditionals (LUTs)?
	reg	[3:0]	state		= `DDRST_INIT;
	reg	[3:0]	next_state	= `DDRST_INIT;
	
	// Initialisation requires 10 steps.
	reg	[3:0]	init_state	= 0;
	wire	init_done	= (init_state == 4'd9);
	
	// Used for the LOAD MODE REGISTER ROM.
	reg	[1:0]	mrd_addr	= 0;
	
	wire	count200_start	= (state == `DDRST_INIT) & (init_state == 4'h4);
	wire	count200_done;
	
	// Enable refreshing once the DRAM is initialised.
	reg	refresh_en	= 0;
	wire	refresh_start;
	wire	refresh_ack	= (state == `DDRST_RFCSTART);
	
	reg	[8:0]	col_addr	= 0;
	
	wire	tRAS_done;	// ACTIVE to PRECHARGE time has elapsed?
	wire	tRC_done;	// ACTIVE to ACTIVE/AUTO REFRESH time has elapsed?
	wire	tREFI;		// REFresh Interval
	
	
	
	/////////////////////////////////////////////////////////////////////
	// State machine description.
	//
	//    On power-up, the DDR controller enters the initialisation
	// state. This module assumes the 200 us for CK/CK# to stablise has
	// already passed when reset applied to this module de-asserts.
	//
	//   Single word reads take priority over single word writes, and
	// block reads have the lowest priority. This is because it is
	// assumed that PCI reads/writes will be infrequent compared to pixel
	// data reads.
	//
	// TODO: This state-machine has a lot of the timings `hard-coded'.
	always @(posedge clock_i)
	begin
		if (reset_i)
		begin
			state		<= `DDRST_INIT;
			next_state	<= `DDRST_INIT;
			init_state	<= 0;
		end
		else
		begin
			case (state)
			
			// The DDR DRAM has a long initialisation sequence
			// which has to be executed.
			`DDRST_INIT:
			begin
				case (init_state)
				
				// Issue a NOP (default behaviour).
				4'h0:	init_state	<= init_state + 1;
				
				// Issue a PRECHARGE ALL.
				4'h1:
				begin
					state	<= `DDRST_PRECHARGE;
					init_state	<= init_state + 1;
				end
				
				// Enable the DLL.
				4'h2:
				begin
					state	<= `DDRST_LOADMODE;
					init_state	<= init_state + 1;
				end
				
				// Issue a DLL reset.
				4'h3:
				begin
					state	<= `DDRST_LOADMODE;
					init_state	<= init_state + 1;
				end
				
				// Start the 200 cycle count.
				4'h4:	init_state	<= init_state + 1;
				
				// Wait 200 cycles for DLL to lock, then Precharge.
				4'h5:
				if (count200_done)
				begin
					init_state	<= init_state + 1;
					state		<= `DDRST_PRECHARGE;
				end
				
				// Issue first AUTO REFRESH.
				4'h6:
				begin
					init_state	<= init_state + 1;
					state		<= `DDRST_RFCSTART;
				end
				
				// Issue second AUTO REFRESH.
				4'h7:
				begin
					init_state	<= init_state + 1;
					state		<= `DDRST_RFCSTART;
				end
				
				// Load operating parameters.
				4'h8:
				begin
					init_state	<= init_state + 1;
					state		<= `DDRST_LOADMODE;
				end
				
				// Initialisation finished.
				default:
				begin
					state		<= `DDRST_IDLE;
					next_state	<= `DDRST_IDLE;
				end
				
				endcase
			end
			
			// In the IDLE state, all banks are precharged?
			`DDRST_IDLE:
			begin
				if (rfc_start_i)
					state	<= `DDRST_RFCSTART;
				else if (state != next_state)	// NOPping, not IDLEing
					state	<= next_state;
				else if (cmd_start_i && tRC_done)
				begin
					// FIXME: Put in a ACTIVE WAIT state
					// so that tRC is not violated.
					state		<= `DDRST_ACTIVE;
					// next_state	<= {`DDRST_READ, cmd_read_i};
					if (cmd_read_i)	// This should be equivalent to the above line
						next_state	<= `DDRST_READ;
					else
						next_state	<= `DDRST_WRITE;
				end
				else
					state	<= state;
			end
			
			// The Mode Register can only be loaded during
			// initialisation. After a load, 2 NOPs must be
			// issued.
			`DDRST_LOADMODE:
			begin
				state		<= `DDRST_NOP;
				next_state	<= `DDRST_INIT;
			end
			
			// Issue an AUTO REFRESH command.
			`DDRST_RFCSTART:
				state	<= `DDRST_RFCWAIT;
			
			// Wait for the refresh to finish.
			`DDRST_RFCWAIT:
			begin
				if (rfc_done_o)
					state	<= `DDRST_IDLE;
				else
					state	<= state;
			end
			
			// Issue a PRECHARGE, then NOP for one cycle until
			// the precharge is done.
			`DDRST_PRECHARGE:
			begin
				state		<= `DDRST_NOP;
				// next_state	<= `DDRST_NOP;
			end
			
			// After an ACTIVE command has beem issued, issue a
			// NOP before a READ or WRITE. `next_state' has
			// already been set to the correct state.
			`DDRST_ACTIVE:
				state	<= `DDRST_NOP;
			
			// Stay in read state until an AP is issued.
			`DDRST_READ:
			begin
				if (cmd_last_i)
				begin
					state		<= `DDRST_IDLE;
					next_state	<= `DDRST_IDLE;
				end
			end
			
			// Stay in write state until an AP is issued.
			`DDRST_WRITE:
			begin
				if (cmd_last_i)
				begin
					state		<= `DDRST_IDLE;
					next_state	<= `DDRST_IDLE;
				end
			end
			
			default:;
			
			endcase
		end
	end
	
	
	
	/////////////////////////////////////////////////////////////////////
	// Using the current `state', set the DDR control signals.
	//
	
	// The lower three bits of the `state' register match RAS#, CAS#, and
	// WE# if the upper bit isn't set.
	always @(posedge clock_i)
	begin
		if (reset_i)
		begin
			ddr_ras_n	<= 1;
			ddr_cas_n	<= 1;
			ddr_we_n	<= 1;
		end
		else
		begin
			if (state [3] == 0)
			begin
				ddr_ras_n	<= state [2];
				ddr_cas_n	<= state [1];
				ddr_we_n	<= state [0];
			end
			else
			begin	// This is a NOP command
				ddr_ras_n	<= 1;
				ddr_cas_n	<= 1;
				ddr_we_n	<= 1;
			end
		end
	end
	
	
	// The LOAD MODE REGISTER is issued three times, with the data
	// appearing on {BA[1:0], A[12:0]}. This data is stored in a small
	// ROM. The ROM address is increased after each LOAD MODE REGISTER
	// command is issued.
	//
	// During normal operation, the band and row address pins are set
	// to the address coming in from the user's FIFOs. The column address
	// is an automatically incrementing counter for block reads, and gets
	// the value of the column address from the FIFOs for single word
	// read/writes.
	always @(posedge clock_i)
	begin
		if (reset_i)
			mrd_addr	<= 0;
		else
		begin
			if (state == `DDRST_LOADMODE)
			begin
				case (mrd_addr)	// This is a small ROM
				
				// Enable the DLL and low drive strength
				2'b00:
					{ddr_ba, ddr_a}	<= 15'b01_00000_0000_0010;
				
				// DLL reset + CL=2 + Seq. + Burst=2
				2'b01:
					{ddr_ba, ddr_a}	<= 15'b00_000010_010_0_001;
				
				// no DLL reset + CL=2 + Seq. + Burst=2
				default:
					{ddr_ba, ddr_a}	<= 15'b00_000000_010_0_001;
				
				endcase
				
				mrd_addr	<= mrd_addr + 1;
			end
			else
			begin
				ddr_ba	<= cmd_bank_i;
				
				if (state == `DDRST_READ || state == `DDRST_WRITE)
					ddr_a	<= {2'b00, cmd_last_i, 1'b0, cmd_col_i, 1'b0};
				else if (state == `DDRST_PRECHARGE)
					ddr_a	<= 13'b0_0100_0000_0000;
				else
					ddr_a	<= cmd_row_i;
			end
		end
	end
	
	
/*	// These registers will be in the IOBs.
	always @(posedge clock_i)
	begin
		if (reset_i)
		begin
			ddr_ras_no	<= 1;
			ddr_cas_no	<= 1;
			ddr_we_no	<= 1;
			
			ddr_ba_o	<= 0;	// These don't really matter
			ddr_a_o		<= 0;
		end
		else
		begin
			ddr_ras_no	<= ddr_ras_n;
			ddr_cas_no	<= ddr_cas_n;
			ddr_we_no	<= ddr_we_n;
			
			ddr_ba_o	<= ddr_ba;
			ddr_a_o		<= ddr_a;
		end
	end
	*/
	
	assign	ddr_ras_no	= ddr_ras_n;
	assign	ddr_cas_no	= ddr_cas_n;
	assign	ddr_we_no	= ddr_we_n;
	
	assign	ddr_ba_o	= ddr_ba;
	assign	ddr_a_o		= ddr_a;
	
	
	// `init_done' has to wait until initialisation has completed, and
	// once `state' is `DDRST_IDLE', assert.
	// TODO: I think this is one LUT smaller (since it can be a SRL) than
	// using a `state' test after `init_done' asserts.
	reg	init_done0	= 0, init_done1	= 0, init_done2	= 0;
	always @(posedge clock_i)
	begin
		init_done0	<= init_done;
		init_done1	<= init_done0;
		init_done2	<= init_done1;
	end
	
	
	assign	init_done_o	= init_done2;
	
	assign	ddr_cke_o	= ~reset_i;
	assign	ddr_cs_no	= reset_i;
	
	// Allows the sequencer to increase column address counters and de-
	// queue commands.
	assign	cmd_exec_o	= (state == `DDRST_READ) | (state == `DDRST_WRITE);
	assign	cmd_active_o	= (state == `DDRST_ACTIVE);
	
	// These synchronise the data flow.
	assign	data_read_o	= (state == `DDRST_READ);
	assign	data_write_o	= (state == `DDRST_WRITE);
	
	// An AUTO-REFRESH cannot be issued until tRC has been met.
	assign	rfc_req_o	= tREFI & tRC_done;
	
	
	
	/////////////////////////////////////////////////////////////////////
	// These are various data-sheet defined timers.
	//
	
	// tRAS - 6 clock cyles from ACTIVE command to PRECHARGE.
	defparam	COUNTtRAS.COUNT	= `tRAS_COUNT;
	defparam	COUNTtRAS.BITS	= `tRAS_BITS;
	counter COUNTtRAS (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		.start_i	(state == `DDRST_ACTIVE),
		.done_o		(tRAS_done)
	);
	
	
	// tRC - 8 clock cyles from ACTIVE to ACTIVE/AUTO REFRESH command.
	reg	[`tRC_BITS-1:0]	tRC_count	= 0;
	always @(posedge clock_i)
	begin
		if (reset_i)
			tRC_count	<= 0;
		else
		begin
			if (state == `DDRST_ACTIVE)
				tRC_count	<= `tRC_COUNT - 1;
			else if (tRC_count != 0)
				tRC_count	<= tRC_count - 1;
			else
				tRC_count	<= tRC_count - 1;
		end
	end
	
	assign	tRC_done	= (tRC_count == 0);
/*	
	defparam	COUNTtRC.COUNT	= `tRC_COUNT;
	defparam	COUNTtRC.BITS	= `tRC_BITS;
	counter COUNTtRC (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		.start_i	(state == `DDRST_ACTIVE),
		.done_o		(tRC_done)
	);
	*/
	
	// tRFC - 10 clock cyles (9 NOPs) for the AUTO REFRESH command.
	defparam	COUNTtRFC.COUNT	= `tRFC_COUNT;
	defparam	COUNTtRFC.BITS	= `tRFC_BITS;
	counter COUNTtRFC (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		.start_i	(state == `DDRST_RFCSTART),
		.done_o		(rfc_done_o)
	);
	
	// Wait 200 cycles after the DDR DLL clock has reset.
	defparam	COUNT200.COUNT	= 200;
	defparam	COUNT200.BITS	= 8;
	counter COUNT200 (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		.start_i	(count200_start),
		.done_o		(count200_done)
	);
	
	
	// All timing is for 100 MHz and a 7.8 us refresh interval.
	defparam	GRFC0.TICKS	= RFC_TICKS;
	defparam	GRFC0.BITS	= 11;
	genrefresh GRFC0 (
		.clock_i	(clock_i),
		.reset_ni	(init_done_o),
		.refresh_o	(tREFI),
		.ack_i		(state == `DDRST_RFCSTART)
	);
	
endmodule	// controller
