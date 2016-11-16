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
 * TART's data-acquisition control subcircuit, connected via a Wishbone-like
 * interconnect.
 * 
 * This module handles DRAM prefetch of raw acquisition data, for the SPI
 * interface, and controls the settings for both the:
 *  a) visibilities calculation unit; and
 *  b) raw acquisition-data buffering unit.
 * 
 * Has system registers for:
 *   0000  --  antenna data stream;
 *   0001  --  antenna data[23:16];
 *   0010  --  antenna data[15: 8];
 *   0011  --  antenna data[ 7: 0];
 *   0100  --  acquisition status;
 *   0110  --  acquisition debug mode;
 *   0111  --  acquisition control;
 * 
 *   1000  --  visibilities data stream;
 *   1001  --  visibilities status; and
 *   1010  --  visibilities control.
 * 
 * NOTE:
 *  + supports both classic and pipelined transfers;
 * 
 * Changelog:
 *  + 23/08/2016  --  initial file (refactored from elsewhere);
 *  + 27/10/2016  --  upgraded to Wishbone SPEC B4;
 * 
 * TODO:
 *  + the block-access mechanism is currently not very flexible -- ideally,
 *    the block-counter would increment once all visibilities have been read
 *    back from the current block?
 *  + more testing for the upgraded to Wishbone (SPEC B4) interface;
 * 
 */

`include "tartcfg.v"

//----------------------------------------------------------------------------
//  TART DATA-ACQUISITION UNIT REGISTERS
//  TODO: Move into the above configuration file?
//----------------------------------------------------------------------------
// Raw antenna-data, read-back registers:
`define AX_STREAM 4'h0
`define AX_DATA1  4'h1
`define AX_DATA2  4'h2
`define AX_DATA3  4'h3

// Data-acquisition status, and control:
`define AQ_STATUS 4'h4
`define AQ_DEBUG  4'h6
`define AQ_SYSTEM 4'h7

// Visibilities access, status, and control:
`define VX_STREAM 4'h8
`define VX_STATUS 4'h9
`define VX_SYSTEM 4'ha

module tart_acquire
  #(// Antenna-source & correlator data bit-widths:
    parameter ACCUM = 24,       // #bits of the viz accumulators
    parameter MSB   = ACCUM-1,
    parameter AXNUM = 24,
    parameter NSB   = AXNUM-1,

    // Wishbone bus bit-widths:
    parameter BBITS = 8,        // WB-like bus data-width
    parameter BSB   = BBITS-1,
    parameter KBITS = BBITS*3,  // By default raw data is 24-bit
    parameter KSB   = KBITS-1,
    parameter XBITS = 4,
    parameter XSB   = XBITS-1,

    // Wishbone bus mode parameters:
    parameter ASYNC = 1,     // combinational control signals (0/1)?
    parameter PIPED = 1,     // pipelined (SPEC B4) transfers (0/1)?
    parameter CHECK = 1,     // TODO: extra sanity-checking (0/1)?
    parameter RESET = 0,     // fast-reset compatible (0/1)?
    parameter VIZWR = 0,     // write-thru mode for visibilities (0/1)?

    // Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_i, // System & Wishbone bus clocks
    input          rst_i,

    // Wishbone-like bus interface:
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output         ack_o,
    output         wat_o,
    output         rty_o,
    output         err_o,
    input [3:0]    adr_i,
    input [BSB:0]  dat_i,
    output [BSB:0] dat_o,

    // SPI status flags:
    input          spi_busy_i,

    //------------------------------------------------------------------------
    //  Memory Controller Block (MCB) signals:
    output         mcb_ce_o,
    output         mcb_wr_o,
    input          mcb_rdy_i,   // MCB ready for commands
    input          mcb_ack_i,   // read acknowledged and ready
    output [ASB:0] mcb_adr_o,
    input [31:0]   mcb_dat_i,
    output [31:0]  mcb_dat_o,

    //  Debug/status outputs:
    output [2:0]   state_o,

    output reg     aq_enabled_o = 1'b0,
    input          aq_valid_i,
    output reg     aq_debug_o = 1'b0,
    output reg     aq_shift_o = 1'b0,
    output reg     aq_count_o = 1'b0,
	  output [2:0]   aq_delay_o,
    input [24:0]   aq_adr_i
    );


   //-------------------------------------------------------------------------
   //  Wishbone-to-SPI signals.
   wire            cyc_w, stb_w, ack_w, ack_c;
   reg             ack = 1'b0;
   reg [BSB:0]     dat = {BBITS{1'b0}};
   wire [BSB:0]    dat_w;

   //  Acquisition unit signals and variables.
   wire [BSB:0]    ax_stream, aq_status, aq_debug, aq_system;
   wire [KSB:0]    ax_data;
	 reg [2:0]       aq_delay = 3'h0;

   //-------------------------------------------------------------------------
   //  Additional MCB signals.
   wire [NSB:0]    data_in = mcd_dat_i[NSB:0];
   wire            request;


   //-------------------------------------------------------------------------
   //  Map the output signals to the system WishBone bus.
   //-------------------------------------------------------------------------
   assign ack_o     = ASYNC ? ack_w : ack;
   assign wat_o     = 1'b0;     // not used/needed by this module
   assign rty_o     = 1'b0;
   assign err_o     = 1'b0;     // TODO: pass out any errors?
   assign dat_o     = ASYNC ? dat_w : dat;

   //  Drive the data bus with either visibilities, or system registers.
   assign cyc_w     = CHECK ? cyc_i : 1'b1;
   assign stb_w     = cyc_w && stb_i;
   assign ack_w     = ack;
   assign dat_w     = dat;


   //-------------------------------------------------------------------------
   //  Group signals into registers.
   //-------------------------------------------------------------------------
   //  Data acquisition streaming, status, and control registers.
   assign aq_debug  = {aq_debug_o, {(BBITS-3){1'b0}}, aq_count_o, aq_shift_o};
   assign aq_status = {aq_adr_i[7:0]};
   assign aq_system = {aq_enabled_o, aq_valid_i, {(BBITS-5){1'b0}}, aq_delay};

   assign aq_delay_o = aq_delay;


   //-------------------------------------------------------------------------
   //
   //  WISHBONE (SLAVE, SPEC B4) BUS INTERFACE.
   //
   //-------------------------------------------------------------------------
   //  Classic, pipelined Wishbone bus cycles can require at least one wait-
   //  state between each transfer, which is achieved here by preventing ACK
   //  being asserted for two consecutive cycles.
   assign ack_c = PIPED ? 1'b0 : ack;


   //-------------------------------------------------------------------------
   //  Generate acknowledges for incoming requests.
   always @(posedge clk_i)
     if (rst_i && RESET)
       ack <= #DELAY 1'b0;
     else if (stb_w && !ack_c)
       ack <= #DELAY 1'b1;
     else if (!ASYNC)
       ack <= #DELAY 1'b1;
     else
       ack <= #DELAY cyc_w;

   //-------------------------------------------------------------------------
   //  Aqusition & visibilities register reads.
   always @(posedge clk_i)
     if (stb_w || x_ack && !ASYNC)
       case (adr_i)
         //  Antenna-data access registers:
         `AX_STREAM: dat <= #DELAY ax_stream;
         `AX_DATA1:  dat <= #DELAY ax_data[23:16];
         `AX_DATA2:  dat <= #DELAY ax_data[15: 8];
         `AX_DATA3:  dat <= #DELAY ax_data[ 7: 0];

         //  Acquisition status, and control, registers:
         `AQ_STATUS: dat <= #DELAY aq_status;
         `AQ_DEBUG:  dat <= #DELAY aq_debug;
         `AQ_SYSTEM: dat <= #DELAY aq_system;

         default:    dat <= #DELAY 8'bx;
       endcase // case (adr_i)


   
   //-------------------------------------------------------------------------
   //
   //  MANAGE SYSTEM FLAGS.
   //
   //-------------------------------------------------------------------------
   //  Aqusition & visibilities register writes.
   always @(posedge clk_i)
     if (rst_i) begin
        aq_enabled_o   <= #DELAY 1'b0;
        aq_delay       <= #DELAY 3'h0;
        aq_debug_o     <= #DELAY 1'b0;
        aq_shift_o     <= #DELAY 1'b0;
        aq_count_o     <= #DELAY 1'b0;
     end
     else if (cyc_w && we_i && !ack_c)
       case (adr_i)
         `AQ_DEBUG:  begin
            aq_shift_o <= #DELAY dat_i[0];
            aq_count_o <= #DELAY dat_i[1];
            aq_debug_o <= #DELAY dat_i[BSB];
         end
         //  Acquisition control register:
         `AQ_SYSTEM: begin
            aq_delay     <= #DELAY dat_i[2:0];
            aq_enabled_o <= #DELAY dat_i[BSB];
         end
       endcase // case (adr_i)



   //-------------------------------------------------------------------------
   //  
   //  PREFETCH RAW ACQUISITION DATA FROM THE SYSTEM DRAM.
   //  
   //-------------------------------------------------------------------------
   reg                 data_sent = 1'b0;
   reg [1:0]           index = 2'h0;
   wire                send, wrap_index;
   wire [2:0]          next_index = wrap_index ? 3'h0 : index + 1;

   assign ax_stream  = index == 2'b00 ? ax_data[23:16] :
                       index == 2'b01 ? ax_data[15: 8] :
                       ax_data[7:0];

   assign send       = stb_w && !we_i && !ack_c && adr_i == `AX_STREAM;
   assign wrap_index = index == 2'b10;

   //-------------------------------------------------------------------------
   //  Increment the current antenna-data index, and prefetch more data as
   //  needed.
   always @(posedge clk_i)
     if (rst_i)
       data_sent <= #DELAY 1'b0;
     else
       data_sent <= #DELAY wrap_index && send;

   always @(posedge clk_i)
     if (!spi_busy_i)
       index <= #DELAY 2'h0;
     else if (send)
       index <= #DELAY next_index[1:0];


   //-------------------------------------------------------------------------
   //  DRAM prefetch logic core.
   //-------------------------------------------------------------------------
   dram_prefetch #( .WIDTH(24) ) DRAM_PREFETCH0
     ( .clock_i(clk_i),
       .reset_i(rst_i),
       .dram_ready(mcb_ack_i),
       .dram_request(request),
       .dram_data(data_in),
       .data_sent(data_sent),
       .fetched_data(ax_data)
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
   raw_capture
     #( .AXNUM(AXNUM),          // number of antennae?
        .ABITS(ABITS),          // MCB address bit-width?
        .RESET(RESET),          // reset-to-zero enable (0/1)?
        .DELAY(DELAY)
        ) RAWDATA
       (
        .clock_i   (clock_i),
        .reset_i   (reset_i),
        .clock_x   (clock_x),
        .reset_x   (reset_i),

        //  Module control-signals:
        .capture_i (en_capture),
        .request_i (request),

        //  External antenna data:
        .strobe_x_i(write_x),
        .signal_x_i(signal_x),

        //  Memory controller signals (bus-domain):
        .mcb_ce_o  (mcb_ce_o),
        .mcb_wr_o  (mcb_wr_o),
        .mcb_rdy_i (mcb_rdy_i),
        .mcb_adr_o (mcb_adr_o),
        .mcb_dat_o (mcb_dat_o),

        //  Debug signals:
        .state_o   (state_o)
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
