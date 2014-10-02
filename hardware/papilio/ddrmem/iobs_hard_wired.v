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

// `define	WIDTH	16
// `define	ADDRESS	13
// `define	PORTS	2
// `define	STROBES	`PORTS
// 
// `define	WMSB	`WIDTH - 1
// `define	AMSB	`ADDRESS - 1
// `define	SMSB	`STROBES - 1
// `define	PMSB	`PORTS - 1
// `define	PWIDTH	`WDITH / `PORTS
// `define	PWMSB	`PWIDTH - 1
// 


// This is the non-paramaterised version since Icarus doesn't support all of
// the Verilog 2001 features.
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
	
	// These signals come from the controller.
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
	
	// These go to the DDR-SDRAM.
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
	
	
	wire	ddr_ck_w;
	wire	ddr_ck_nw;
	
	// Control IOBs.
	reg	ddr_cke_r	= 1;	// synthesis attribute of ddr_cke_r is true ;
	reg	ddr_cs_nr	= 1;	// synthesis attribute iob of ddr_cs_nr is true ;
	reg	ddr_ras_nr	= 1;	// synthesis attribute iob of ddr_ras_nr is true ;
	reg	ddr_cas_nr	= 1;	// synthesis attribute iob of ddr_cas_nr is true ;
	reg	ddr_we_nr	= 1;	// synthesis attribute iob of ddr_we_nr is true ;
	
	// Address IOBs.
	reg	[1:0]	ddr_ba_r;	// synthesis attribute iob of ddr_ba_o is true ;
	reg	[12:0]	ddr_a_r;	// synthesis attribute iob of ddr_a_o is true ;
	
	wire	[1:0]	ddr_dqs_w;
	wire	[1:0]	ddr_dm_w;
	wire	[1:0]	ddr_dqs_ibuf;	// About 2 ns of delay can be set
	
	
	/////////////////////////////////////////////////////////////////////
	// Generate the differential clocks using the FDDRRSE primitive so
	// that it has similar behaviour to the data pins.
	//
	
	// TODO: Should this use OBUFS instead?
	FDDRRSE fddr_ck_n (
		.Q	(ddr_ck_w),
		.C0	(clock_i),
		.C1	(~clock_i),
		.CE	(locked),
		.D0	(0),
		.D1	(1),
		.R	(reset_i),
		.S	(0)
	);
	
	FDDRRSE fddr_ck (
		.Q	(ddr_ck_nw),
		.C0	(clock_i),
		.C1	(~clock_i),
		.CE	(locked),
		.D0	(1),
		.D1	(0),
		.R	(0),
		.S	(reset_i)
	);
	
	OBUF ck_obuf	( .I (ddr_ck_w), .O (ddr_ck_o));
	OBUF ckn_obuf	( .I (ddr_ck_nw), .O (ddr_ck_no));
	
	
	
	/////////////////////////////////////////////////////////////////////
	// DDR control signals.
	//
	
	always @(posedge clock_i)
	begin
		if (reset_i)
		begin
			ddr_cke_r	<= 0;
			
			ddr_cs_nr	<= 1;
			ddr_ras_nr	<= 1;
			ddr_cas_nr	<= 1;
			ddr_we_nr	<= 1;
		end
		else
		begin
			ddr_cke_r	<= 1;
			
			ddr_cs_nr	<= cntrl_cs_ni;
			ddr_ras_nr	<= cntrl_ras_ni;
			ddr_cas_nr	<= cntrl_cas_ni;
			ddr_we_nr	<= cntrl_we_ni;
		end
	end
	
	// These have about 4.6 ns delay on a Spartan III in SSTL2 mode?
	OBUF cke_obuf	( .I (ddr_cke_r),	.O (ddr_cke_o));
	OBUF csn_obuf	( .I (ddr_cs_nr),	.O (ddr_cs_no));
	OBUF rasn_obuf	( .I (ddr_ras_nr),	.O (ddr_ras_no));
	OBUF casn_obuf	( .I (ddr_cas_nr),	.O (ddr_cas_no));
	OBUF wen_obuf	( .I (ddr_we_nr),	.O (ddr_we_no));
	
	
	
	/////////////////////////////////////////////////////////////////////
	// Address signals.
	//
	
	always @(posedge clock_i)
	begin
		ddr_ba_r	<= cntrl_ba_i;
		ddr_a_r		<= cntrl_a_i;
	end
	
	OBUF ba_obuf0	( .I (ddr_ba_r [0]),	.O (ddr_ba_o [0]));
	OBUF ba_obuf1	( .I (ddr_ba_r [1]),	.O (ddr_ba_o [1]));
	OBUF a_obuf0	( .I (ddr_a_r [0]),	.O (ddr_a_o [0]));
	OBUF a_obuf1	( .I (ddr_a_r [1]),	.O (ddr_a_o [1]));
	OBUF a_obuf2	( .I (ddr_a_r [2]),	.O (ddr_a_o [2]));
	OBUF a_obuf3	( .I (ddr_a_r [3]),	.O (ddr_a_o [3]));
	OBUF a_obuf4	( .I (ddr_a_r [4]),	.O (ddr_a_o [4]));
	OBUF a_obuf5	( .I (ddr_a_r [5]),	.O (ddr_a_o [5]));
	OBUF a_obuf6	( .I (ddr_a_r [6]),	.O (ddr_a_o [6]));
	OBUF a_obuf7	( .I (ddr_a_r [7]),	.O (ddr_a_o [7]));
	OBUF a_obuf8	( .I (ddr_a_r [8]),	.O (ddr_a_o [8]));
	OBUF a_obuf9	( .I (ddr_a_r [9]),	.O (ddr_a_o [9]));
	OBUF a_obuf10	( .I (ddr_a_r [10]),	.O (ddr_a_o [10]));
	OBUF a_obuf11	( .I (ddr_a_r [11]),	.O (ddr_a_o [11]));
	OBUF a_obuf12	( .I (ddr_a_r [12]),	.O (ddr_a_o [12]));
	
	
	
	/////////////////////////////////////////////////////////////////////
	// Data pins are bi-directional so they are a bit harder.
	//
	
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
	
	
	// Generate the two data strobes.
	FDDRRSE dqs_fddr0 (
		.C0	(clock_i),
		.C1	(~clock_i),
		.CE	(1'b1),
		.D0	(1'b0),
		.D1	(1'b1),
		.R	(~cntrl_dqs_i),
		.S	(1'b0),
		.Q	(ddr_dqs_w [0])
	);
	
	FDDRRSE dqs_fddr1 (
		.C0	(clock_i),
		.C1	(~clock_i),
		.CE	(1'b1),
		.D0	(1'b0),
		.D1	(1'b1),
		.R	(~cntrl_dqs_i),
		.S	(1'b0),
		.Q	(ddr_dqs_w [1])
	);
	
	// About 4.6ns of delay.
	OBUFT dqs_obuf0 (
		.T	(cntrl_send_i & send),
		.I	(ddr_dqs_w [0]),
		.O	(ddr_dqs_io [0])
	);
	
	OBUFT dqs_obuf1 (
		.T	(cntrl_send_i & send),
		.I	(ddr_dqs_w [1]),
		.O	(ddr_dqs_io [1])
	);
	
	wire	[1:0]	ddr_dqs_i;
	IBUF dqs_ibuf0 ( .I (ddr_dqs_io [0]), .O (ddr_dqs_i [0]) );
	IBUF dqs_ibuf1 ( .I (ddr_dqs_io [1]), .O (ddr_dqs_i [1]) );
	
	// Generate the data masks (masks when high).
	FDDRRSE dm_fddr0 (
		.C0	(clock_270i),
		.C1	(~clock_270i),
		.CE	(1'b1),
		.D0	(cntrl_dm_i [2]),
		.D1	(cntrl_dm_i [0]),
		.R	(1'b0),
		.S	(1'b0),
		.Q	(ddr_dm_w [0])
	);
	
	FDDRRSE dm_fddr1 (
		.C0	(clock_270i),
		.C1	(~clock_270i),
		.CE	(1'b1),
		.D0	(cntrl_dm_i [3]),
		.D1	(cntrl_dm_i [1]),
		.R	(1'b0),
		.S	(1'b0),
		.Q	(ddr_dm_w [1])
	);
	
	OBUF dm_obuf0 ( .I (ddr_dm_w [0]),	.O (ddr_dm_o [0]));
	OBUF dm_obuf1 ( .I (ddr_dm_w [1]),	.O (ddr_dm_o [1]));
	
	
	// The delayed `dqs_w' signals from the DDR device clock data into
	// the IFDDRRSEs.
	assign	#2 ddr_dqs_ibuf	= ddr_dqs_io;
	
	// Sixteen double-data rate output flip-flops.
	ddr_io DIO0 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [0]),
		.data_i	({cntrl_data_i [16], cntrl_data_i [0]}),
		.data_o	({cntrl_data_o [16], cntrl_data_o [0]}),
		.ddr_io	(ddr_dq_io [0]) );
	
	ddr_io DIO1 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [0]),
		.data_i	({cntrl_data_i [17], cntrl_data_i [1]}),
		.data_o	({cntrl_data_o [17], cntrl_data_o [1]}),
		.ddr_io	(ddr_dq_io [1]) );
	
	ddr_io DIO2 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [0]),
		.data_i	({cntrl_data_i [18], cntrl_data_i [2]}),
		.data_o	({cntrl_data_o [18], cntrl_data_o [2]}),
		.ddr_io	(ddr_dq_io [2]) );
	
	ddr_io DIO3 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [0]),
		.data_i	({cntrl_data_i [19], cntrl_data_i [3]}),
		.data_o	({cntrl_data_o [19], cntrl_data_o [3]}),
		.ddr_io	(ddr_dq_io [3]) );
	
	ddr_io DIO4 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [0]),
		.data_i	({cntrl_data_i [20], cntrl_data_i [4]}),
		.data_o	({cntrl_data_o [20], cntrl_data_o [4]}),
		.ddr_io	(ddr_dq_io [4]) );
	
	ddr_io DIO5 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [0]),
		.data_i	({cntrl_data_i [21], cntrl_data_i [5]}),
		.data_o	({cntrl_data_o [21], cntrl_data_o [5]}),
		.ddr_io	(ddr_dq_io [5]) );
	
	ddr_io DIO6 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [0]),
		.data_i	({cntrl_data_i [22], cntrl_data_i [6]}),
		.data_o	({cntrl_data_o [22], cntrl_data_o [6]}),
		.ddr_io	(ddr_dq_io [6]) );
	
	ddr_io DIO7 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [0]),
		.data_i	({cntrl_data_i [23], cntrl_data_i [7]}),
		.data_o	({cntrl_data_o [23], cntrl_data_o [7]}),
		.ddr_io	(ddr_dq_io [7]) );
	
	// Second 8-bit data port.
	ddr_io DIO8 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [1]),
		.data_i	({cntrl_data_i [24], cntrl_data_i [8]}),
		.data_o	({cntrl_data_o [24], cntrl_data_o [8]}),
		.ddr_io	(ddr_dq_io [8]) );
	
	ddr_io DIO9 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [1]),
		.data_i	({cntrl_data_i [25], cntrl_data_i [9]}),
		.data_o	({cntrl_data_o [25], cntrl_data_o [9]}),
		.ddr_io	(ddr_dq_io [9]) );
	
	ddr_io DIO10 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [1]),
		.data_i	({cntrl_data_i [26], cntrl_data_i [10]}),
		.data_o	({cntrl_data_o [26], cntrl_data_o [10]}),
		.ddr_io	(ddr_dq_io [10]) );
	
	ddr_io DIO11 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [1]),
		.data_i	({cntrl_data_i [27], cntrl_data_i [11]}),
		.data_o	({cntrl_data_o [27], cntrl_data_o [11]}),
		.ddr_io	(ddr_dq_io [11]) );
	
	ddr_io DIO12 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [1]),
		.data_i	({cntrl_data_i [28], cntrl_data_i [12]}),
		.data_o	({cntrl_data_o [28], cntrl_data_o [12]}),
		.ddr_io	(ddr_dq_io [12]) );
	
	ddr_io DIO13 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [1]),
		.data_i	({cntrl_data_i [29], cntrl_data_i [13]}),
		.data_o	({cntrl_data_o [29], cntrl_data_o [13]}),
		.ddr_io	(ddr_dq_io [13]) );
	
	ddr_io DIO14 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [1]),
		.data_i	({cntrl_data_i [30], cntrl_data_i [14]}),
		.data_o	({cntrl_data_o [30], cntrl_data_o [14]}),
		.ddr_io	(ddr_dq_io [14]) );
	
	ddr_io DIO15 ( .clk_i (~clock_270i), .clk_ni (clock_270i),
		.send_i	(cntrl_send_i & send), .dqs_i (ddr_dqs_i [1]),
		.data_i	({cntrl_data_i [31], cntrl_data_i [15]}),
		.data_o	({cntrl_data_o [31], cntrl_data_o [15]}),
		.ddr_io	(ddr_dq_io [15]) );
	
	
endmodule	// iobs
