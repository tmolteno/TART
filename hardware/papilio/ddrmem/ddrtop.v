/***************************************************************************
 *                                                                         *
 *   ddrtop.v - The top level module of the DDR memory controller.         *
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

`timescale 1ns/100ps
module ddrtop (
		clock_i,
		clock_270i,
		reset_i,
		
		// DDR pins
		ddr_clk_po,
		ddr_clk_no,
		ddr_cke_o,
		
		ddr_cs_no,
		ddr_ras_no,
		ddr_cas_no,
		ddr_we_no,
		
		ddr_ba_o,
		ddr_a_o,
		
		ddr_dm_o,
		ddr_dq_io,
		ddr_dqs_io,
		
		// User interface
		rd_req_i,	// Read a single 32-bit word
		rd_block_i,	// Read a whole row (1 kB)
		rd_owner_i,	// Bus address of receiving module
		rd_busy_o,	// Cannot queue up any more commands
		rd_addr_i,	// bank+row 1st cycle, column address 2nd cycle
		rd_owner_o,	// Bus address of receiving module
		rd_data_o,	// 32-bits per clock data output
		rd_ready_o,	// Data waiting on the outputs
		
		wr_req_i,
		wr_block_i,
		wr_busy_o,
		wr_bes_ni,	// Write byte enables
		wr_addr_i,
		wr_data_i
	);
	
	parameter	DWIDTH		= 16;	// DDR data width
	parameter	DRADDR		= 13;	// DDR row bits
	parameter	DCADDR		= 9;	// DDR column bits
	parameter	STROBES		= 2;	// No. of DQS/DM signals
	
	parameter	UWIDTH		= 32;	// User data bits
	parameter	UBYTES		= 4;	// No. of byte enables
	parameter	UADDR		= 23;	// User address bits
	parameter	OWNERS		= 2;	// No. of owner bits
	
	parameter	REFRESH_TIMER	= 585;	// Refresh interval @75 MHz
	parameter	CAS_LATENCY	= 2;	// TODO: Works upto 133 MHz
	parameter	BURST_LENGTH	= 2;	// TODO:
	
	// System-wide pins
	input	clock_i;
	input	clock_270i;
	input	reset_i;
	
	// DDR pins
	output	ddr_clk_po;	// Clocking pins
	output	ddr_clk_no;
	output	ddr_cke_o;	// Also used for auto-refresh
	
	output	ddr_cs_no;	// Control pins
	output	ddr_ras_no;
	output	ddr_cas_no;
	output	ddr_we_no;
	
	output	[1:0]		ddr_ba_o;	// Bank/address pins
	output	[DRADDR-1:0]	ddr_a_o;
	
	output	[STROBES-1:0]	ddr_dm_o;	// Data pins
	inout	[STROBES-1:0]	ddr_dqs_io;
	inout	[DWIDTH-1:0]	ddr_dq_io;
	
	// User interface
	input	rd_req_i;
	input	rd_block_i;	// Read a whole row then precharge
	output	rd_busy_o;
	input	[OWNERS-1:0]	rd_owner_i;
	output	[OWNERS-1:0]	rd_owner_o;
	input	[UADDR-1:0]	rd_addr_i;
	output	[UWIDTH-1:0]	rd_data_o;
	output	rd_ready_o;
	
	input	wr_req_i;
	input	wr_block_i;	// Auto-precharge after write
	output	wr_busy_o;
	input	[UADDR-1:0]	wr_addr_i;
	input	[UBYTES-1:0]	wr_bes_ni;
	input	[UWIDTH-1:0]	wr_data_i;
	
	
	// Wires connecting the user FIFOs to the command scheduler.
	wire	raf_read;
	wire	raf_block;
	wire	raf_empty_n;
	wire	raf_one;
	wire	[OWNERS-1:0]	raf_owner;
	wire	[UADDR-1:0]	raf_addr;
	
	wire	waf_read;
	wire	waf_empty_n;
	wire	[UADDR-1:0]	waf_addr;
	wire	[UBYTES-1:0]	wdf_bes_n;
	wire	[UWIDTH-1:0]	wdf_data;
	
	// DDR signals coming from the controller going to the IOBs.
	wire	cntrl_cke;
	wire	cntrl_cs_n;
	wire	cntrl_ras_n;
	wire	cntrl_cas_n;
	wire	cntrl_we_n;
	wire	[1:0]		cntrl_ba;
	wire	[DRADDR-1:0]	cntrl_a;
	
	// Control signals between the scheduler to the controller.
	wire	ctl_start;
	wire	ctl_read;
	wire	ctl_last;
	wire	[1:0]		ctl_bank;
	wire	[DRADDR-1:0]	ctl_row;
	wire	[DCADDR-2:0]	ctl_col;	// bit-0 is always 0
	
	wire	ctl_exec;
	wire	ctl_active;
	
	// Datapath control signals.
	wire	dp_read;
	wire	dp_write;
	wire	dp_suspend;
	
	// Datapath to IOBs.
	wire	dp_send;
	wire	[UBYTES-1:0]	dp_bes_n;
	wire	dp_dqs;			// This controls the toggling of DQS
	wire	[UWIDTH-1:0]	dp_data_to;
	wire	[UWIDTH-1:0]	dp_data_from;
	
	// After write data has been sent to the DDR device, advance the
	// write FIFO.
	reg	wdf_read	= 0;
	
	// Delay the `data_read' signal so that the FIFO is advanced at the
	// correct time.
	reg	wdf_read0, wdf_read1;
	always @(posedge clock_i)
	begin
		if (reset_i)
			wdf_read0	<= 0;
		else
		begin
			wdf_read0	<= dp_write;
			wdf_read1	<= wdf_read0;
			wdf_read	<= wdf_read1;
		end
	end
	
	
	// Queue incoming commands and let the command sequencer figure out
	// the best order to dispatch them.
	user_fifos UFIFO0 (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		
		// User interface.
		.rd_req_i	(rd_req_i),
		.rd_block_i	(rd_block_i),
		.rd_busy_o	(rd_busy_o),
		.rd_owner_i	(rd_owner_i),
		.rd_addr_i	(rd_addr_i),
		
		.wr_req_i	(wr_req_i),
		.wr_busy_o	(wr_busy_o),
		.wr_addr_i	(wr_addr_i),
		.wr_bytes_i	(wr_bes_ni),
		.wr_data_i	(wr_data_i),
		
		// FIFO interface (to command sequencer).
		.raf_read_i	(raf_read),
		.raf_block_o	(raf_block),
		.raf_one_o	(raf_one),
		.raf_empty_no	(raf_empty_n),
		.raf_owner_o	(raf_owner),
		.raf_addr_o	(raf_addr),
		
		.waf_read_i	(waf_read),
		.waf_empty_no	(waf_empty_n),
		.waf_addr_o	(waf_addr),
		
		.wdf_read_i	(wdf_read),
		.wdf_bytes_o	(wdf_bes_n),
		.wdf_data_o	(wdf_data)
	);
	
	
	scheduler SHED0 (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		.enable_i	(init_done),
		
		.raf_empty_ni	(raf_empty_n),
		.raf_one_i	(raf_one),
		.raf_block_i	(raf_block),
		.raf_read_o	(raf_read),
		.raf_addr_i	(raf_addr),
		
		.waf_empty_ni	(waf_empty_n),
		.waf_read_o	(waf_read),
		.waf_addr_i	(waf_addr),
		
		// Commands sent to the DDR controller.
		.cmd_start_o	(ctl_start),
		.cmd_read_o	(ctl_read),
		.cmd_last_o	(ctl_last),
		.cmd_susp_o	(dp_suspend),
		.cmd_bank_o	(ctl_bank),
		.cmd_row_o	(ctl_row),
		.cmd_col_o	(ctl_col),
		
		// Signals back from the controller.
		.ctl_read_i	(dp_read),
		.ctl_write_i	(dp_write),
		.ctl_active_i	(ctl_active),
		
		.rfc_req_i	(rfc_req),	// Receive refresh req.
		.rfc_ack_o	(rfc_start),
		.rfc_end_i	(rfc_done)
	);
	
	
	// The refresh interval is a maximum of 7.8 us.
	defparam	CNTRL0.RFC_TICKS	= REFRESH_TIMER;
	controller CNTRL0 (
		.clock_i	(clock_i),	// 133 MHz system clock
		.reset_i	(reset_i),	// Goes high once signal
		
		.ddr_cke_o	(cntrl_cke),	// DDR controls and address signals
		
		.ddr_cs_no	(cntrl_cs_n),
		.ddr_ras_no	(cntrl_ras_n),
		.ddr_cas_no	(cntrl_cas_n),
		.ddr_we_no	(cntrl_we_n),
		
		.ddr_ba_o	(cntrl_ba),
		.ddr_a_o	(cntrl_a),
		
		.init_done_o	(init_done),
		
		.cmd_start_i	(ctl_start),	// Commands from the scheduler
		.cmd_read_i	(ctl_read),
		.cmd_last_i	(ctl_last),
		.cmd_exec_o	(ctl_exec),
		.cmd_active_o	(ctl_active),
		.cmd_bank_i	(ctl_bank),
		.cmd_row_i	(ctl_row),
		.cmd_col_i	(ctl_col),
		
		.data_read_o	(dp_read),
		.data_write_o	(dp_write),
		
		.rfc_req_o	(rfc_req),
		.rfc_start_i	(rfc_start),
		.rfc_done_o	(rfc_done)
	);
	
	
	// Moves data from the FIFOs to the DDR, or from the DDR to the
	// module outputs.
	datapath DP0 (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		
		.usr_data_i	(wdf_data),
		.usr_bes_ni	(wdf_bes_n),
		.usr_owner_i	(raf_owner),
		.usr_data_o	(rd_data_o),
		.usr_owner_o	(rd_owner_o),
		.usr_ready_o	(rd_ready_o),
		
		.ctl_start_i	(ctl_start),
		.ctl_block_i	(block),	// FIXME!
		.ctl_suspend_i	(dp_suspend),
		.ctl_read_i	(dp_read),
		.ctl_write_i	(dp_write),
		
		.ddr_send_o	(dp_send),
		.ddr_bes_no	(dp_bes_n),
		.ddr_dqs_o	(dp_dqs),
		.ddr_data_o	(dp_data_to),
		.ddr_data_i	(dp_data_from)
	);
	
	
	iobs IOBS0 (
		.clock_i	(clock_i),
		.clock_270i	(clock_270i),
		.reset_i	(reset_i),
		
		// Signals to/from DDR controller.
		.cntrl_cke_i	(cntrl_cke),
		
		.cntrl_cs_ni	(cntrl_cs_n),
		.cntrl_ras_ni	(cntrl_ras_n),
		.cntrl_cas_ni	(cntrl_cas_n),
		.cntrl_we_ni	(cntrl_we_n),
		
		.cntrl_ba_i	(cntrl_ba),
		.cntrl_a_i	(cntrl_a),
		
		.cntrl_dm_i	(dp_bes_n),
		.cntrl_data_i	(dp_data_to),
		.cntrl_dqs_i	(dp_dqs),
		.cntrl_send_i	(dp_send),
		.cntrl_data_o	(dp_data_from),
		
		// Signals to/from DDR device.
		.ddr_ck_o	(ddr_clk_po),
		.ddr_ck_no	(ddr_clk_no),
		.ddr_cke_o	(ddr_cke_o),
		
		.ddr_cs_no	(ddr_cs_no),
		.ddr_ras_no	(ddr_ras_no),
		.ddr_cas_no	(ddr_cas_no),
		.ddr_we_no	(ddr_we_no),
		
		.ddr_ba_o	(ddr_ba_o),
		.ddr_a_o	(ddr_a_o),
		
		.ddr_dm_o	(ddr_dm_o),
		.ddr_dq_io	(ddr_dq_io),
		.ddr_dqs_io	(ddr_dqs_io)
	);
	
endmodule	// ddrtop
