/***************************************************************************
 *                                                                         *
 *   ddr_lite.v - A 99% fat-free DDR interface.                            *
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

// Address, Data and Byte-enables widths.
`define	ABITS	10
`define	AMSB	`ABITS-1
`define	DBITS	32
`define	DMSB	`DBITS-1
`define	BBITS	`DBITS/8
`define	BMSB	`BBITS-1

`timescale 1ns/100ps
module ddr_lite (
	clock_i,
	clock_90i,
	clock_270i,
	reset_ni,
	
	init_done_o,
	
	read_i,
	rfull_o,
	rack_o,
	ready_o,
	raddr_i,
	rdata_o,
	
	write_i,
	wbes_ni,
	wfull_o,
	wack_o,
	waddr_i,
	wdata_i,
	
	ddr_ck_o,
	ddr_ck_no,
	ddr_cke_o,
	
	ddr_cs_no,
	ddr_ras_no,
	ddr_cas_no,
	ddr_we_no,
	
	ddr_ba_o,
	ddr_a_o,
	
	ddr_dqs_io,
	ddr_dm_o,
	ddr_dq_io
);

parameter	REFRESH_TIMER	= 585;	// Refresh interval @75 MHz
parameter	CAS_LATENCY	= 2;	// TODO: Works upto 133 MHz
parameter	BURST_LENGTH	= 2;	// TODO:

input	clock_i;
input	clock_90i;
input	clock_270i;
input	reset_ni;

output	init_done_o;

// Memory controller interface.
input	read_i;
output	rfull_o;
output	rack_o;
output	ready_o;
input	[`AMSB:0]	raddr_i;
output	[`DMSB:0]	rdata_o;

input	write_i;
input	[`BMSB:0]	wbes_ni;
output	wfull_o;
output	wack_o;
input	[`AMSB:0]	waddr_i;
input	[`DMSB:0]	wdata_i;

// DDR pins.
output	ddr_ck_o;
output	ddr_ck_no;
output	ddr_cke_o;

output	ddr_cs_no;
output	ddr_ras_no;
output	ddr_cas_no;
output	ddr_we_no;

output	[1:0]	ddr_ba_o;
output	[12:0]	ddr_a_o;

inout	[1:0]	ddr_dqs_io;
output	[1:0]	ddr_dm_o;
inout	[15:0]	ddr_dq_io;


// Simple state machine for the controller. After power on, the DDR needs to
// be initialised, and this takes quite a while.
`define	DLST_IDLE	2'b00
`define	DLST_READ	2'b01
`define	DLST_WRITE	2'b10
`define	DLST_BUSY	2'b11
reg	[1:0]	state	= `DLST_IDLE;

reg	start	= 0;
reg	[1:0]	row_lo;
reg	[7:0]	col;

reg	rd_ack	= 0;
reg	wr_ack	= 0;

reg	[1:0]	send	= 0;

reg	dqs_r	= 1;

reg	[3:0]	ready	= 0;

reg	[3:0]	bes_n;
reg	[31:0]	wdata;

reg	rfc_req_r	= 0;

wire	init_done;

wire	ctl_start	= start;
wire	ctl_read	= (state == `DLST_READ);
wire	ctl_last	= (state == `DLST_READ) || (state == `DLST_WRITE);
wire	ctl_active;
wire	ctl_exec;
wire	[1:0]	ctl_bank	= 2'b00;
wire	[12:0]	ctl_row		= {9'b0, row_lo};
wire	[7:0]	ctl_col		= col;

wire	send_w	= send [0] | send [1];

wire	rfc_req;
wire	rfc_start;
wire	rfc_done;

wire	ddr_ck_w;
wire	ddr_ck_nw;

wire	[1:0]	ddr_dqs_i;
wire	[1:0]	ddr_dqs_w;
wire	[1:0]	ddr_dm_w;


assign	init_done_o	= init_done;

assign	rfull_o	= (state != `DLST_IDLE);
assign	wfull_o	= (state != `DLST_IDLE);

assign	rack_o	= rd_ack;
assign	wack_o	= wr_ack;

assign	ready_o	= ready [3];

assign	rfc_start	= (state == `DLST_BUSY) && rfc_req_r;


always @(posedge clock_i)
begin
	if (~reset_ni)
		state	<= `DLST_BUSY;
	else
	case (state)
		`DLST_IDLE:
		begin
			if (rfc_req)
				state	<= `DLST_BUSY;
			else if (read_i)
				state	<= `DLST_READ;
			else if (write_i)
				state	<= `DLST_WRITE;
			else
				state	<= state;
		end
		
		`DLST_READ, `DLST_WRITE:
		begin
			if (ctl_exec)
				state	<= `DLST_IDLE;
		end
		
		default:
		begin
			if (init_done && rfc_done)
				state	<= `DLST_IDLE;
			else
				state	<= `DLST_BUSY;
		end
	endcase
end


always @(posedge clock_i)
begin
	if (~reset_ni)
		start	<= 0;
	else
	case (state)
	`DLST_IDLE:
	begin
		if ((read_i || write_i) && (state == `DLST_IDLE))
			start	<= 1;
		else
			start	<= 0;
		
		if (read_i)
		begin
			row_lo	<= raddr_i [9:8];
			col	<= raddr_i [7:0];
		end
		else
		begin
			row_lo	<= waddr_i [9:8];
			col	<= waddr_i [7:0];
		end
	end
	
	endcase
end


// Register the incoming write data.
always @(posedge clock_i)
begin
	if (state == `DLST_IDLE)
	begin
		bes_n	<= wbes_ni;
		wdata	<= wdata_i;
	end
end


// TODO: May be too slow, and have to be combinatorial.
always @(posedge clock_i)
begin
	if (~reset_ni)
	begin
		rd_ack	<= 0;
		wr_ack	<= 0;
	end
	else
	begin
		rd_ack	<= (state == `DLST_IDLE) && read_i && !rfc_req;
		wr_ack	<= (state == `DLST_IDLE) && (write_i && !read_i) && !rfc_req;
	end
end


wire	cap_w	= ctl_exec && (state == `DLST_READ);
reg	capture	= 0;
always @(posedge clock_i)
	capture	<= ready [1];

always @(posedge clock_i)
	ready	<= {ready [2:0], cap_w};


// Assert the memory bus for three clock cycles.
always @(posedge clock_i)
	send	<= {send [0], ctl_exec && (state == `DLST_WRITE)};


always @(posedge clock_i)
begin
	if (~reset_ni)
		rfc_req_r	<= 0;
	else
	begin
		if (rfc_req)
			rfc_req_r	<= 1;
		else if (rfc_req_r && (state == `DLST_BUSY))
			rfc_req_r	<= 0;
	end
end


// Disable the DQS strobes unless sending the write data.
always @(posedge clock_i)
begin
	if (~reset_ni)
		dqs_r	<= 1;
	else if (send_w && dqs_r)
		dqs_r	<= 0;
	else
		dqs_r	<= 1;
end


`ifndef __icarus
reg	[14:0]	clk_count	= 0;
always @(posedge clock_i)
begin
	if (!reset_ni)
		clk_count	<= 0;
	else
	begin
		if (!clk_count [14])
			clk_count	<= clk_count + 1;
	end
end
`endif


// This is the DDR contoller that handles the initialisation and the issuing
// of DRAM commands.
// The refresh interval is a maximum of 7.8 us.
defparam	CNTRL0.RFC_TICKS	= REFRESH_TIMER;
controller CNTRL0 (
	.clock_i	(clock_i),	// Upto 133 MHz system clock
`ifdef __icarus
	.reset_i	(~reset_ni),
`else
	.reset_i	(~clk_count [14]),
`endif
	
	.ddr_cke_o	(ddr_cke_o),	// DDR controls and address signals
	
	.ddr_cs_no	(ddr_cs_no),
	.ddr_ras_no	(ddr_ras_no),
	.ddr_cas_no	(ddr_cas_no),
	.ddr_we_no	(ddr_we_no),
	
	.ddr_ba_o	(ddr_ba_o),
	.ddr_a_o	(ddr_a_o),
	
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


/////////////////////////////////////////////////////////////////////////////
// Generate the differential clocks using the FDDRRSE primitive so that it
// has similar behaviour to the data pins.
//

// TODO: Should this use OBUFS instead?
FDDRRSE fddr_ck (
	.Q	(ddr_ck_w),
	.C0	(clock_90i),
	.C1	(clock_270i),
	.CE	(1'b1),
	.D0	(1'b1),
	.D1	(1'b0),
	.R	(1'b0),
	.S	(1'b0)
);

FDDRRSE fddr_ck_n (
	.Q	(ddr_ck_nw),
	.C0	(clock_90i),
	.C1	(clock_270i),
	.CE	(1'b1),
	.D0	(1'b0),
	.D1	(1'b1),
	.R	(1'b0),
	.S	(1'b0)
);

// synthesis attribute iob of ck_obuf is true ;
// synthesis attribute iob of ckn_obuf is true ;
// synthesis attribute slew of ck_obuf is fast ;
// synthesis attribute slew of ckn_obuf is fast ;
OBUF ck_obuf	( .I (ddr_ck_w), .O (ddr_ck_o));
OBUF ckn_obuf	( .I (ddr_ck_nw), .O (ddr_ck_no));


/////////////////////////////////////////////////////////////////////////////
// Send the DDR commands to the device.
//

// FIXME: A board fix is required since pairs of IOBs share the same ICLK and
// OTCLK signals. The problem is that WE# and LDQS share the same signals.
`define	__old_spartan3_board

// Generate the two data strobes.
FDDRRSE dqs_fddr0 (
`ifdef	__old_spartan3_board
	.C0	(clock_i),
	.C1	(~clock_i),
`else
	.C0	(clock_90i),
	.C1	(clock_270i),
`endif
	.CE	(1'b1),
	.D0	(1'b0),
	.D1	(1'b1),
	.R	(~send_w),
	.S	(1'b0),
	.Q	(ddr_dqs_w [0])
);

FDDRRSE dqs_fddr1 (
	.C0	(clock_90i),
	.C1	(clock_270i),
	.CE	(1'b1),
	.D0	(1'b0),
	.D1	(1'b1),
	.R	(~send_w),
	.S	(1'b0),
	.Q	(ddr_dqs_w [1])
);

// About 4.6ns of delay.
// FIXME: This is about 7ns using the LVCMOS25 protocol.
// FIXME: A better solution would be to move WE# out of an IOB?
// synthesis attribute iob of dqs_obuf0 is true ;
// synthesis attribute iob of dqs_obuf1 is true ;
// synthesis attribute slew of dqs_obuf0 is fast ;
// synthesis attribute slew of dqs_obuf1 is fast ;
OBUFT #(7.0) dqs_obuf0 (
	.T	(send_w),
	.I	(ddr_dqs_w [0]),
	.O	(ddr_dqs_io [0])
);

OBUFT dqs_obuf1 (
	.T	(send_w),
	.I	(ddr_dqs_w [1]),
	.O	(ddr_dqs_io [1])
);

// Using an IOBDELAY that adds about 2ns.
IBUF #(2.5) dqs_ibuf0 ( .I (ddr_dqs_io [0]), .O (ddr_dqs_i [0]) );
IBUF #(2.5) dqs_ibuf1 ( .I (ddr_dqs_io [1]), .O (ddr_dqs_i [1]) );


// Generate the data masks (masks when high).
FDDRRSE dm_fddr0 (
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(send_w),
	.D0	(bes_n [2]),
	.D1	(bes_n [0]),
	.R	(1'b0),
	.S	(1'b0),
	.Q	(ddr_dm_w [0])
);

FDDRRSE dm_fddr1 (
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(send_w),
	.D0	(bes_n [3]),
	.D1	(bes_n [1]),
	.R	(1'b0),
	.S	(1'b0),
	.Q	(ddr_dm_w [1])
);

// synthesis attribute iob of dm_obuf0 is true ;
// synthesis attribute iob of dm_obuf1 is true ;
// synthesis attribute slew of dm_obuf0 is fast ;
// synthesis attribute slew of dm_obuf1 is fast ;
OBUF dm_obuf0 ( .I (ddr_dm_w [0]),	.O (ddr_dm_o [0]));
OBUF dm_obuf1 ( .I (ddr_dm_w [1]),	.O (ddr_dm_o [1]));


ddr_dq_iobs DQIOBS0 (
	.clock_i	(clock_i),
	.clock_ni	(~clock_i),
	
	.send_i		(send_w),
	.capture_i	(capture),
	
	.ddr_dqs_i	(ddr_dqs_i [0]),
	.ddr_dq_io	(ddr_dq_io [7:0]),
	
	.data_i		({wdata [23:16], wdata [7:0]}),
	.data_o		({rdata_o [23:16], rdata_o [7:0]})
);


ddr_dq_iobs DQIOBS1 (
	.clock_i	(clock_i),
	.clock_ni	(~clock_i),
	
	.send_i		(send_w),
	.capture_i	(capture),
	
	.ddr_dqs_i	(ddr_dqs_i [1]),
	.ddr_dq_io	(ddr_dq_io [15:8]),
	
	.data_i		({wdata [31:24], wdata [15:8]}),
	.data_o		({rdata_o [31:24], rdata_o [15:8]})
);


endmodule	// ddr_lite
