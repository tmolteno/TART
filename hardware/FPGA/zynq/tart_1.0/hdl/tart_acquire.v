`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/tart_acquire.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * 
 * This file is part of TART.
 * 
 * TART is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * TART is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser Public License along with
 * TART.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * 
 * Description:
 * TART's raw-data acquisition control subcircuit, connected via a Wishbone
 * (SPEC B4) interconnect.
 * 
 * This module handles DRAM prefetch of raw acquisition data, for the SPI
 * interface, and contains registers for:
 *  a) controlling raw-data acquisition;
 *  b) the Memory Controller Block (MCB); and
 *  c) streaming back raw acquisition-data.
 * 
 * 
 * REGISTERS:
 *  Reg#   7         6       5       4       3       2      1      0
 *      -------------------------------------------------------------------
 *   00 ||                 RAW-DATA STREAM REGISTER                      ||
 *      ||                           (RO)                                ||
 *      -------------------------------------------------------------------
 *   01 ||                         RESERVED                              ||
 *      ||                                                               ||
 *      -------------------------------------------------------------------
 *   10 ||                         RESERVED                              ||
 *      ||                                                               ||
 *      -------------------------------------------------------------------
 *   11 || ENABLED | ERROR | READY | 512Mb | OFLOW |        STATE        ||
 *      ||  (R/W)  | (RO)  | (RO)  | (RO)  | (RO)  |        (RO)         ||
 *      -------------------------------------------------------------------
 * 
 * By default, the raw-data acquisition unit has address 7'b010_00xx.
 * 
 * 
 * NOTE:
 *  + supports both classic and pipelined transfers;
 * 
 * Changelog:
 *  + 23/08/2016  --  initial file (refactored from elsewhere);
 *  + 27/10/2016  --  upgraded to Wishbone SPEC B4;
 *  + 23/11/2016  --  refactored to just contain acquisition functionality;
 * 
 * TODO:
 *  + the block-access mechanism is currently not very flexible -- ideally,
 *    the block-counter would increment once all visibilities have been read
 *    back from the current block?
 *  + more testing for the upgraded to Wishbone (SPEC B4) interface;
 *  + checksums;
 * 
 */

 `include "tartcfg.v"

//----------------------------------------------------------------------------
//  TART DATA-ACQUISITION UNIT REGISTERS
//  TODO: Move into the above configuration file?
//----------------------------------------------------------------------------
// Raw antenna-data, read-back registers:
`define AQ_STREAM 2'h0
`define AQ_SYSTEM 2'h3


module tart_acquire
#(// Antenna-signal data bit-widths:
  parameter AXNUM = 24,
  parameter MSB   = AXNUM-1,

    // Memory-Controller Block (MSB) settings:
    parameter ABITS = 21,
    parameter ASB   = ABITS-1,

    // Wishbone bus bit-widths:
    parameter BBITS = 8,        // WB bus data-width
    parameter BSB   = BBITS-1,
    parameter KBITS = BBITS*3,  // by default raw data is 24-bit
    parameter KSB   = KBITS-1,
    parameter XBITS = 4,
    parameter XSB   = XBITS-1,

    // Wishbone bus mode parameters:
    parameter PIPED = 1,     // pipelined (SPEC B4) transfers (0/1)?
    parameter CHECK = 1,     // TODO: extra sanity-checking (0/1)?
    parameter RESET = 0,     // fast-reset compatible (0/1)?

    // Simulation-only parameters:
    parameter DELAY = 3,

    parameter integer C_M_AXIS_TDATA_WIDTH    = 32
  )
  (
    input          clock_i, // system & Wishbone bus clocks
    input          reset_i,

    // Raw-data inputs:
    input          locked_i, // oversampled (6x, DDR), acquisition-data
    input          strobe_i,
    input          middle_i,
    input [MSB:0]  signal_i,

    // Wishbone-like bus interface:
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output         ack_o,
    output         wat_o,
    output         rty_o,
    output         err_o,
    input [1:0]    adr_i,
    input [BSB:0]  dat_i,
    output [BSB:0] dat_o,

    input wire  M_AXIS_ACLK,
    input wire  M_AXIS_ARESETN,
    // Master Stream Ports. TVALID indicates that the master 
    // is driving a valid transfer, A transfer takes place when 
    // both TVALID and TREADY are asserted. 
    output wire  M_AXIS_TVALID,
    // TDATA is the primary payload that is used to provide the 
    // data that is passing across the interface from the master.
    output wire [C_M_AXIS_TDATA_WIDTH-1 : 0] M_AXIS_TDATA,
    // TSTRB is the byte qualifier that indicates whether the 
    // content of the associated byte of TDATA is processed as 
    // a data byte or a position byte.
    output wire [(C_M_AXIS_TDATA_WIDTH/8)-1 : 0] M_AXIS_TSTRB,
    // TLAST indicates the boundary of a packet.
    output wire  M_AXIS_TLAST,
    // TREADY indicates that the slave can accept a transfer in the 
    // current cycle.
    input wire  M_AXIS_TREADY,

    //  Debug/status outputs:
    output         enabled_o,
    output         oflow_o,
    output [2:0]   state_o
  );



   //-------------------------------------------------------------------------
   //  Wishbone-to-SPI signals.
   wire            cyc_w, stb_w, ack_w;
   reg             ack = 1'b0;
   reg [BSB:0]     dat = {BBITS{1'b0}};
   wire [BSB:0]    dat_w;
   wire            fetch, store;

   //-------------------------------------------------------------------------
   //  Acquisition unit signals and variables.
   wire [BSB:0]    aq_stream, aq_system;
   reg             en_acquire = 1'b0;

   //-------------------------------------------------------------------------
   //  Additional MCB signals.
   wire [MSB:0]    data_in;
   wire            request;
   reg             mcb_err = 1'b0, mcb_wat = 1'b0, active = 1'b0;
   wire [2:0]      cap_state;

   //-------------------------------------------------------------------------
   //  Data-streaming registers & signals.
   reg             data_sent = 1'b0;
   reg [1:0]       index = 2'h0;
   wire            send, wrap_index, oflow;
   wire [2:0]      next_index;


   //-------------------------------------------------------------------------
   //  Debug-signal assignments.
   //-------------------------------------------------------------------------
   assign enabled_o  = en_acquire;
   assign oflow_o    = oflow;
   assign state_o    = cap_state;


   //-------------------------------------------------------------------------
   //  Group signals into registers.
   //-------------------------------------------------------------------------
   //  Assign the current byte (of the raw-data stream) to the streaming read-
   //  back register.
   assign aq_stream  = index == 2'b00 ? data_in[23:16] :
   index == 2'b01 ? data_in[15: 8] :
   data_in[7:0];

   //  Data acquisition status, and control register.
   assign aq_system  = {en_acquire, axis_active, 0, 0, oflow, cap_state};

   //-------------------------------------------------------------------------
   //  Map the output signals to the system WishBone bus.
   //-------------------------------------------------------------------------
   assign ack_o      = ack;
   assign wat_o      = 1'b0;     // not used/needed by this module
   assign rty_o      = 1'b0;
   assign err_o      = 1'b0;     // TODO: pass out any errors?
   assign dat_o      = dat;

   //  Drive the data bus with either visibilities, or system registers.
   assign cyc_w      = CHECK ? cyc_i : 1'b1;
   assign stb_w      = cyc_w && stb_i;
   assign ack_w      = PIPED ? stb_w : stb_w && !ack;
   assign dat_w      = dat;

   //  These ensure just a single 'fetch'/'store' per transfer.
   assign fetch      = PIPED ? stb_w && !we_i : stb_w && !we_i &&  ack;
   assign store      = PIPED ? stb_w &&  we_i : stb_w &&  we_i && !ack;


   //-------------------------------------------------------------------------
   //  Data-streaming assignments.
   //-------------------------------------------------------------------------
   assign send       = fetch && adr_i == `AQ_STREAM && active;
   assign wrap_index = index == 2'b10;
   assign next_index = wrap_index ? 3'h0 : index + 1;



   //-------------------------------------------------------------------------
   //
   //  WISHBONE (SLAVE, SPEC B4) BUS INTERFACE.
   //
   //-------------------------------------------------------------------------
   //  Generate acknowledges for incoming requests.
   always @(posedge clock_i)
   begin
     if (reset_i && RESET)
       ack <= #DELAY 1'b0;
     else
       ack <= #DELAY ack_w;
   end


   always @(posedge clock_i)
   begin
     if (stb_w)
     begin
       case (adr_i)
	 `AQ_STREAM: dat <= #DELAY 0;
	 `AQ_SYSTEM: dat <= #DELAY aq_system;
	 default:    dat <= #DELAY 8'bx;
       endcase
     end
   end

   always @(posedge clock_i)
   begin
     if (reset_i)
     begin
       en_acquire <= #DELAY 1'b0;
     end
     else if (store)
     begin
       case (adr_i)
	 `AQ_SYSTEM: en_acquire <= #DELAY dat_i[BSB];
       endcase
     end
   end

   wire fifo_empty;
   wire [MSB:0] fifo_data_o;

   afifo_gray 
   #( 
     .WIDTH(AXNUM),
     .ABITS(8),
     .DELAY(DELAY)
   ) fifo ( 
     .rst_i(reset_i),

     .wr_clk_i(clock_i),
     .wr_en_i(en_acquire && locked_i),
     .wr_data_i(signal_i),
     .wfull_o(oflow),

     .rd_clk_i(M_AXIS_ACLK),
     .rd_en_i(axis_tvalid && M_AXIS_TREADY),
     .rd_data_o(fifo_data_o),
     .rempty_o(fifo_empty)
   );

   localparam NUMBER_OF_OUTPUT_WORDS = 31;

   reg [7 : 0]  read_pointer;

   wire  	axis_tvalid;
   reg  	axis_tlast;
   reg 	        axis_active;

   assign axis_tvalid   = axis_active && !fifo_empty;

   assign M_AXIS_TVALID	= axis_tvalid;
   assign M_AXIS_TDATA	= fifo_data_o;
   assign M_AXIS_TLAST	= axis_tlast;
   assign M_AXIS_TSTRB	= {(C_M_AXIS_TDATA_WIDTH/8){1'b1}};

   always@(posedge M_AXIS_ACLK)
   begin
     if(!M_AXIS_ARESETN)
     begin
       axis_active <= 0;
       read_pointer <= 0;
       axis_tlast <= 0;
     end
     else
     begin
       if (!axis_active)
       begin
	 axis_active <= 1;
       end
       else if (axis_active && read_pointer < NUMBER_OF_OUTPUT_WORDS)
       begin
	 if (axis_tvalid && M_AXIS_TREADY)
	 begin
	   if (read_pointer == NUMBER_OF_OUTPUT_WORDS-1)
	     axis_tlast <= 1;

	   read_pointer <= read_pointer + 1;
	 end
       end
       else 
       begin
	 axis_active <= 0;
	 axis_tlast <= 0;
	 read_pointer <= 0;
       end
     end
   end

 endmodule
