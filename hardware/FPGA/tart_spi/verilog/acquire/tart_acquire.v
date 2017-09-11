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
    parameter DELAY = 3)
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

    // Flag for when the I/O (SPI by default) interace is active:
    input          io_busy_i,

    //  Memory Controller Block (MCB) signals:
    output         mcb_ce_o,
    output         mcb_wr_o,
    input          mcb_rdy_i, // MCB ready for commands
    input          mcb_ack_i, // read acknowledged and ready
    output [ASB:0] mcb_adr_o,
    input [31:0]   mcb_dat_i,
    output [31:0]  mcb_dat_o,

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
   wire            request, mcb_512mb;
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
   assign aq_system  = {en_acquire, mcb_err, mcb_rdy_i, mcb_512mb, oflow, cap_state};

   //  Extended-memory device?
   assign mcb_512mb  = ABITS >= 25;


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
     if (reset_i && RESET)
       ack <= #DELAY 1'b0;
     else
       ack <= #DELAY ack_w;


   //-------------------------------------------------------------------------
   //  Manage system flags.
   //-------------------------------------------------------------------------
   //  Acquisition-unit register reads.
   always @(posedge clock_i)
     if (stb_w)
       case (adr_i)
         `AQ_STREAM: dat <= #DELAY aq_stream;
         `AQ_SYSTEM: dat <= #DELAY aq_system;
         default:    dat <= #DELAY 8'bx;
       endcase // case (adr_i)

   //-------------------------------------------------------------------------
   //  Acquisition-unit register writes.
   always @(posedge clock_i)
     if (reset_i)
       en_acquire <= #DELAY 1'b0;
     else if (store)
       case (adr_i)
         //  Acquisition control register:
         `AQ_SYSTEM: en_acquire <= #DELAY dat_i[BSB];
       endcase // case (adr_i)



   //-------------------------------------------------------------------------
   //  
   //  PREFETCH RAW ACQUISITION-DATA FROM THE SYSTEM DRAM.
   //  
   //-------------------------------------------------------------------------
   //  Increment the current antenna-data index, and prefetch more data as
   //  needed.
   always @(posedge clock_i)
     if (reset_i && RESET)
       data_sent <= #DELAY 1'b0;
     else
       data_sent <= #DELAY wrap_index && send;

   //-------------------------------------------------------------------------
   //  Increment the byte-index after each transfer, and then reset to zero at
   //  the end of a SPI transaction.
   always @(posedge clock_i)
     if (!io_busy_i)
       index <= #DELAY 2'h0;
     else if (send)
       index <= #DELAY next_index[1:0];

   //-------------------------------------------------------------------------
   //  The MCB is only active once the raw-data buffer has been filled.
   always @(posedge clock_i)
     if (reset_i && RESET)
       active <= #DELAY 1'b0;
     else if (cap_state > 2)
       active <= #DELAY 1'b1;


   //-------------------------------------------------------------------------
   //  Check that SDRAM data has arrived before a new request.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i)
       mcb_wat <= #DELAY 1'b0;
     else if (wrap_index && send)
       mcb_wat <= #DELAY 1'b1;
     else if (mcb_ack_i)
       mcb_wat <= #DELAY 1'b0;

   always @(posedge clock_i)
     if (reset_i)
       mcb_err <= #DELAY 1'b0;
     else if (mcb_wat && send) // SPI req before MCD ready?
       mcb_err <= #DELAY 1'b1;


   //-------------------------------------------------------------------------
   //  DRAM prefetch logic core.
   //-------------------------------------------------------------------------
   dram_prefetch
     #( .WIDTH(AXNUM)
       ) PREFETCH
       (
        .clock_i     (clock_i),
        .reset_i     (reset_i),

        .dram_ready  (mcb_ack_i),
        .dram_request(request),
        .dram_data   (mcb_dat_i[MSB:0]),
        .data_sent   (data_sent),
        .fetched_data(data_in)
        );



   //-------------------------------------------------------------------------
   //
   //  RAW-DATA ACQUISITION BLOCK.
   //
   //-------------------------------------------------------------------------
   //  Streams signal raw-data to an off-chip SDRAM, and also streaming it
   //  back.
   //  NOTE: This is mostly just a wrapper around the original raw-data
   //    acquisition functionality from 2013 , but with more attention paid to
   //    CDC issues.
   raw_acquire
     #( .AXNUM(AXNUM),          // number of antennae?
        .ABITS(ABITS),          // MCB address bit-width?
        .RESET(RESET),          // reset-to-zero enable (0/1)?
        .DELAY(DELAY)
        ) RAWDATA
       (
        .clock_i  (clock_i),
        .reset_i  (reset_i),

        //  External antenna data:
        .locked_i (locked_i),
        .strobe_i (strobe_i),
        .middle_i (middle_i),
        .signal_i (signal_i),

        //  Module control-signals:
        .capture_i(en_acquire),
        .request_i(request),

        //  Memory controller signals (bus-domain):
        .mcb_ce_o (mcb_ce_o),
        .mcb_wr_o (mcb_wr_o),
        .mcb_rdy_i(mcb_rdy_i),
        .mcb_adr_o(mcb_adr_o),
        .mcb_dat_o(mcb_dat_o),

        //  Debug signals:
        .oflow_o  (oflow),
        .state_o  (cap_state)
        );



   //-------------------------------------------------------------------------
   //
   //  SANITY-CHECK THE INITIALISATION PARAMETERS.
   //
   //-------------------------------------------------------------------------
   initial begin
      if (KBITS < AXNUM) begin : INIT_ERROR
         $write("Invalid initialisation parameters:\n");
         $write("  AXNUM = %1d\n", AXNUM);
         $write("  KBITS = %1d\n", KBITS);
         $write("  BBITS = %1d\n", BBITS);
         $error;
      end
   end



endmodule // tart_acquire
