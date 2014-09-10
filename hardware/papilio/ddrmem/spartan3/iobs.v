/***************************************************************************
 *                                                                         *
 *   iobs.v - Instantiates the IOBs that communicate with the DDR SDRAM.   *
 *     This file is Xilinx Spartan III specific.                           *
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

`define	OBUF_delay	#4.0
`define	IBUF_delay	#0.8

module iobs (
		clock_i,
		clock_270i,	// Delayed 270 degrees for DQs
		reset_i,
		
		// Signals to/from DDR controller.
		cntrl_cke_i,
		
		cntrl_cs_ni,
		cntrl_ras_ni,
		cntrl_cas_ni,
		cntrl_we_ni,
		
		cntrl_ba_i,
		cntrl_a_i,
		
		cntrl_dm_i,
		cntrl_data_i,
		cntrl_dqs_i,
		cntrl_send_i,
		cntrl_data_o,
		
		// Signals to/from DDR device.
		ddr_ck_o,
		ddr_ck_no,
		ddr_cke_o,
		
		ddr_cs_no,
		ddr_ras_no,
		ddr_cas_no,
		ddr_we_no,
		
		ddr_ba_o,
		ddr_a_o,
		
		ddr_dm_o,
		ddr_dq_io,
		ddr_dqs_io
	);
	
	parameter	WIDTH	= 16;
	parameter	ADDRESS	= 13;
	parameter	STROBES	= 2;
	parameter	PORTMSB	= WIDTH / STROBES - 1;
	
	input	clock_i;
	input	clock_270i;
	input	reset_i;
	
	input	cntrl_cke_i;
	
	input	cntrl_cs_ni;
	input	cntrl_ras_ni;
	input	cntrl_cas_ni;
	input	cntrl_we_ni;
	
	input	[1:0]	cntrl_ba_i;
	input	[ADDRESS-1:0]	cntrl_a_i;
	
	input	cntrl_send_i;
	input	[STROBES*2-1:0]	cntrl_dm_i;
	input	[WIDTH*2-1:0]	cntrl_data_i;
	input	cntrl_dqs_i;	// Just a DQS control signal, not the values
	output	[WIDTH*2-1:0]	cntrl_data_o;
	// output	[STROBES-1:0]	ddr_dqs_o,
	
	
	output	ddr_ck_o;	// synthesis attribute iob of ddr_ck_o is true ;
	output	ddr_ck_no;	// synthesis attribute iob of ddr_ck_no is true ;
	output	ddr_cke_o;
	
	output	ddr_cs_no;
	output	ddr_cas_no;
	output	ddr_ras_no;
	output	ddr_we_no;
	
	output	[1:0]		ddr_ba_o;
	output	[ADDRESS-1:0]	ddr_a_o;
	
	output	[STROBES-1:0]	ddr_dm_o;
	inout	[WIDTH-1:0]	ddr_dq_io;
	inout	[STROBES-1:0]	ddr_dqs_io;
	
	
	
	reg	ddr_cke_o	= 0;	// synthesis attribute of ddr_cke_o is true ;
	
	reg	ddr_cs_no	= 1;	// synthesis attribute iob of ddr_cs_no is true ;
	reg	ddr_ras_no	= 1;	// synthesis attribute iob of ddr_ras_no is true ;
	reg	ddr_cas_no	= 1;	// synthesis attribute iob of ddr_cas_no is true ;
	reg	ddr_we_no	= 1;	// synthesis attribute iob of ddr_we_no is true ;
	
	reg	[1:0]	ddr_ba_o	= 0;	// synthesis attribute iob of ddr_ba_o is true ;
	reg	[12:0]	ddr_a_o		= 0;	// synthesis attribute iob of ddr_a_o is true ;
	
	
	// Generate the DDR differential clock.
`ifdef __icarus
	defparam	fddr_ck_n.INIT	= 0;
	defparam	fddr_ck_n.DELAY	= 4.0;
`endif
	OFDDRRSE fddr_ck_n (
		.Q	(ddr_ck_o),
		.C0	(clock_i),
		.C1	(~clock_i),
		.CE	(locked),
		.D0	(0),
		.D1	(1),
		.R	(reset_i),
		.S	(0)
	);
	
`ifdef __icarus
	defparam	fddr_ck.INIT	= 1;
	defparam	fddr_ck.DELAY	= 4.0;
`endif
	OFDDRRSE fddr_ck (
		.Q	(ddr_ck_no),
		.C0	(clock_i),
		.C1	(~clock_i),
		.CE	(locked),
		.D0	(1),
		.D1	(0),
		.R	(0),
		.S	(reset_i)
	);
	
	
	// DDR control signals.
	always @(posedge clock_i)
	begin
		if (reset_i)
		begin
			ddr_cke_o	<= `OBUF_delay 0;
			
			ddr_cs_no	<= `OBUF_delay 1;
			ddr_ras_no	<= `OBUF_delay 1;
			ddr_cas_no	<= `OBUF_delay 1;
			ddr_we_no	<= `OBUF_delay 1;
		end
		else
		begin
			ddr_cke_o	<= `OBUF_delay 1;
			
			ddr_cs_no	<= `OBUF_delay cntrl_cs_ni;
			ddr_ras_no	<= `OBUF_delay cntrl_ras_ni;
			ddr_cas_no	<= `OBUF_delay cntrl_cas_ni;
			ddr_we_no	<= `OBUF_delay cntrl_we_ni;
		end
	end
	
	
	// Address signals
	reg	[1:0]	ddr_ba_i	= 0;
	reg	[12:0]	ddr_a_i		= 0;
	always @(posedge clock_i)
	begin
		ddr_ba_o	<= `OBUF_delay cntrl_ba_i;
		ddr_a_o		<= `OBUF_delay cntrl_a_i;
	end
	
	reg	send	= 0;
	always @(negedge clock_i)
	begin
		if (reset_i)
			send	<= 0;
		else if (cntrl_send_i)
			send	<= 1;
		else
			send	<= 0;
	end
	
	
	// Allow the number of data IOBs to be parametised
	wire	[WIDTH-1:0]	ddr_dq;
	wire	[STROBES-1:0]	ddr_dqs;
	wire	[STROBES-1:0]	ddr_dm;
	wire	[WIDTH-1:0]	dout_top	= cntrl_data_i [WIDTH*2-1:WIDTH];
	wire	[WIDTH-1:0]	dout_bot	= cntrl_data_i [WIDTH-1:0];
	wire	[WIDTH-1:0]	din_top;
	wire	[WIDTH-1:0]	din_bot;
	assign	cntrl_data_o [WIDTH-1:0]		= din_bot;
	assign	cntrl_data_o [WIDTH*2-1:WIDTH]	= din_top;
	wire	[WIDTH-1:0]	dqs_w;
	
	wire	[STROBES-1:0]	dm_top	= cntrl_dm_i [STROBES*2-1:STROBES];
	wire	[STROBES-1:0]	dm_bot	= cntrl_dm_i [STROBES-1:0];
	
	// FIXME: Parameterise this.
	//assign	#1.5	dqs_w [WIDTH/2-1:0]	= {WIDTH/2{ddr_dqs_io [0]}};
	assign	#1.5	dqs_w [7:0]	= {ddr_dqs_io [0], ddr_dqs_io [0], ddr_dqs_io [0], ddr_dqs_io [0], ddr_dqs_io [0], ddr_dqs_io [0], ddr_dqs_io [0], ddr_dqs_io [0]};
	assign	#1.5	dqs_w [15:8]	= {ddr_dqs_io [1], ddr_dqs_io [1], ddr_dqs_io [1], ddr_dqs_io [1], ddr_dqs_io [1], ddr_dqs_io [1], ddr_dqs_io [1], ddr_dqs_io [1]};
	
	// DDR data outputs. Delayed by 270 degrees so that
	// the DQS signal is centre aligned.
	OFDDRRSE odata_fddr [WIDTH-1:0] (
		.C0	(clock_270i),
		.C1	(~clock_270i),
		.CE	(1'b1),
		.D0	(dout_top),
		.D1	(dout_bot),
		.R	(1'b0),
		.S	(1'b0),
		.Q	(ddr_dq)
	);
	
	// The delayed `dqs_w' signals from the DDR device clock data into
	// the IFDDRRSEs.
	IFDDRRSE idata_fddr [WIDTH-1:0] (
		.C0	(dqs_w),
		.C1	(~dqs_w),
		.CE	(1'b1),
		.D	(ddr_dq_io),
		.R	(1'b0),
		.S	(1'b0),
		.Q0	(din_bot),
		.Q1	(din_top)
	);
	
	// TODO: Implement display masks.
	OFDDRRSE dm_fddr [STROBES-1:0] (
		.C0	(clock_270i),
		.C1	(~clock_270i),
		.CE	(1'b1),
		.D0	(dm_top),
		.D1	(dm_bot),
		.R	(1'b0),
		.S	(1'b0),
		.Q	(ddr_dm)
	);
	
	// TODO: Implement DQS.
	OFDDRRSE dqs_fddr [STROBES-1:0] (
		.C0	(clock_i),
		.C1	(~clock_i),
		.CE	(1'b1),
		.D0	(1'b0),
		.D1	(1'b1),
		.R	(~cntrl_dqs_i),
		.S	(1'b0),
		.Q	(ddr_dqs)
	);
	
	OBUFT data_obuf [WIDTH-1:0] (
		.T	(cntrl_send_i & send),
		.I	(ddr_dq),
		.O	(ddr_dq_io)
	);
	
	OBUFT dqs_obuf [STROBES-1:0] (
		.T	(cntrl_send_i & send),
		.I	(ddr_dqs),
		.O	(ddr_dqs_io)
	);
	
	OBUFT dm_obuf [STROBES-1:0] (
		.T	(~reset_i),
		.I	(ddr_dm),
		.O	(ddr_dm_o)
	);
	
endmodule	// iobs
