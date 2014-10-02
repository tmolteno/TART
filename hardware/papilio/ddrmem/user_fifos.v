/***************************************************************************
 *                                                                         *
 *   user_fifos.v - Uses FIFOs to queue incoming read and write requests.  *
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

module user_fifos (
		clock_i,
		reset_i,
		
		// User interface
		rd_req_i,	// Read a whole row (1 kB)
		rd_block_i,	// Read a single 32-bit word
		rd_busy_o,	// Cannot queue up any more commands
		rd_owner_i,	// Bus address of receiving module
		rd_addr_i,	// bank+row 1st cycle, column address 2nd cycle
		
		wr_req_i,
		wr_busy_o,
		wr_addr_i,
		wr_bytes_i,	// Write byte enables
		wr_data_i,
		
		raf_read_i,
		raf_block_o,
		raf_empty_no,
		raf_one_o,	// One address in FIFO
		raf_owner_o,
		raf_addr_o,
		
		waf_read_i,	// Address and data are seperate FIFOs
		waf_empty_no,
		waf_addr_o,
		
		wdf_read_i,
		wdf_bytes_o,
		wdf_data_o
	);
	
	parameter	WIDTH	= 32;
	parameter	ADDRESS	= 23;
	parameter	OWNERS	= 2;
	parameter	BYTES	= WIDTH / 8;
	
	input	clock_i;
	input	reset_i;
	
	// User interface
	input	rd_req_i;
	input	rd_block_i;	// Read a whole row then precharge?
	output	rd_busy_o;
	input	[OWNERS-1:0]	rd_owner_i;
	input	[ADDRESS-1:0]	rd_addr_i;
	
	input	wr_req_i;
	output	wr_busy_o;
	input	[ADDRESS-1:0]	wr_addr_i;
	input	[BYTES-1:0]	wr_bytes_i;
	input	[WIDTH-1:0]	wr_data_i;
	
	// Read Address FIFO
	input	raf_read_i;
	output	raf_empty_no;
	output	raf_one_o;
	output	raf_block_o;
	output	[OWNERS-1:0]	raf_owner_o;
	output	[ADDRESS-1:0]	raf_addr_o;
	
	// Write FIFOs
	input	waf_read_i;
	output	waf_empty_no;
	output	[ADDRESS-1:0]	waf_addr_o;
	
	input	wdf_read_i;
	output	[BYTES-1:0]	wdf_bytes_o;
	output	[WIDTH-1:0]	wdf_data_o;
	
	
	wire	waf_full;
	wire	wdf_full;
	assign	wr_busy_o	= waf_full | wdf_full;
	
	wire	raf_write_w	= rd_req_i & ~rd_busy_o;
	wire	waf_write_w	= wr_req_i & ~wr_busy_o;
	wire	wdf_write_w	= wr_req_i & ~wr_busy_o;
	
	// Each read command can come from multiple sources, so each source
	// has an owner tag which is placed on the bus when the data has been
	// read.
	defparam	RAF0.WIDTH	= ADDRESS + OWNERS + 1;
	fifo16s	RAF0 (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		
		.read_i		(raf_read_i & raf_empty_no),
		.write_i	(raf_write_w),
		.data_i		({rd_block_i, rd_owner_i, rd_addr_i}),
		.data_o		({raf_block_o, raf_owner_o, raf_addr_o}),
		
		.full_o		(rd_busy_o),
		.empty_no	(raf_empty_no)
	);
	
	
/*	
`ifndef __use_LFSR
	
	defparam	RAF0.WIDTH	= ADDRESS + OWNERS + 1;
	fifo15l RAF0 (
		.reset_i	(reset_i),
		
		.wr_clk_i	(clock_i),
		.wr_en_i	(raf_write_w),
		.wr_data_i	({rd_block_i, rd_owner_i, rd_addr_i}),
		
		.rd_clk_i	(clock_i),
		.rd_en_i	(raf_read_i & raf_empty_no),
		.rd_data_o	({raf_block_o, raf_owner_o, raf_addr_o}),
`ifdef __use_reg_FIFOs		
		.full_ro	(rd_busy_o),
		.one_ro		(raf_one_o),
		.empty_nro	(raf_empty_no)
`else		
		.full_o		(rd_busy_o),
		.one_o		(raf_one_o),
		.empty_no	(raf_empty_no)
`endif
	);
	*/
	
	
	// Write data and addresses are seperate since the data delay is
	// different to the address delay.
	defparam	WAF0.WIDTH	= ADDRESS;
	fifo16s	WAF0 (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		
		.read_i		(waf_read_i),
		.write_i	(waf_write_w),
		.data_i		(wr_addr_i),
		.data_o		(waf_addr_o),
		
		.full_o		(waf_full),
		.empty_no	(waf_empty_no)
	);
	
	
/*	defparam	WAF0.WIDTH	= ADDRESS;
	fifo15l WAF0 (
		.reset_i	(reset_i),
		
		.wr_clk_i	(clock_i),
		.wr_en_i	(waf_write_w),
		.wr_data_i	(wr_addr_i),
		
		.rd_clk_i	(clock_i),
		.rd_en_i	(waf_read_i),
		.rd_data_o		(waf_addr_o),
`ifdef __use_reg_FIFOs		
		.full_ro	(waf_full),
		.empty_nro	(waf_empty_no)
`else		
		.full_o		(waf_full),
		.empty_no	(waf_empty_no)
`endif
	);
	*/
	
	defparam	WDF0.WIDTH	= WIDTH + BYTES;
	fifo16s	WDF0 (
		.clock_i	(clock_i),
		.reset_i	(reset_i),
		
		.read_i		(wdf_read_i),
		.write_i	(wdf_write_w),
		.data_i		({wr_bytes_i, wr_data_i}),
		.data_o		({wdf_bytes_o, wdf_data_o}),
		
		.full_o		(wdf_full),
		.empty_no	(wdf_empty_no)
	);
	
	
/*	defparam	WDF0.WIDTH	= WIDTH + BYTES;
	fifo15l WDF0 (
		.reset_i	(reset_i),
		
		.wr_clk_i	(clock_i),
		.wr_en_i	(wdf_write_w),
		.wr_data_i	({wr_bytes_i, wr_data_i}),
		
		.rd_clk_i	(clock_i),
		.rd_en_i	(wdf_read_i),
		.rd_data_o	({wdf_bytes_o, wdf_data_o}),
`ifdef __use_reg_FIFOs		
		.full_ro	(wdf_full),
		.empty_nro	(wdf_empty_no)
`else		
		.full_o		(wdf_full),
		.empty_no	(wdf_empty_no)
`endif
	);
	*/
	
/*`else
	// FIXME: I think the FIFO full signal will go high when 15 items are
	// in the FIFO, but low when the 16 is added.
	defparam	RAF0.FIFO_WIDTH	= ADDRESS + OWNERS + 1;
	fifo16 RAF0 (
		.write_clock_i	(clock_i),
		.write_i	(raf_write_w),
		.data_i		({rd_block_i, rd_owner_i, rd_addr_i}),
		
		.read_clock_i	(clock_i),
		.read_i		(raf_read_i & raf_empty_no),
		.data_o		({raf_block_o, raf_owner_o, raf_addr_o}),
		
		.almost_full_o	(rd_busy_o),
		.not_empty_o	(raf_empty_no)
	);
	
	
	// Write data and addresses are seperate since the data delay is
	// different to the address delay.
	defparam	WAF0.FIFO_WIDTH	= ADDRESS;
	fifo16 WAF0 (
		.write_clock_i	(clock_i),
		.write_i	(waf_write_w),
		.data_i		(wr_addr_i),
		
		.read_clock_i	(clock_i),
		.read_i		(waf_read_i),
		.data_o		(waf_addr_o),
		
		.almost_full_o	(waf_full),
		.not_empty_o	(waf_empty_no)
	);
	
	defparam	WDF0.FIFO_WIDTH	= WIDTH + BYTES;
	fifo16 WDF0 (
		.write_clock_i	(clock_i),
		.write_i	(wdf_write_w),
		.data_i		({wr_bytes_i, wr_data_i}),
		
		.read_clock_i	(clock_i),
		.read_i		(wdf_read_i),
		.data_o		({wdf_bytes_o, wdf_data_o}),
		
		.almost_full_o	(wdf_full),
		.not_empty_o	(wdf_empty_no)
	);
`endif
	*/
endmodule	// user_fifos
