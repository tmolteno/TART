`timescale 1ns/100ps
/*
 * Module      : verilog/tart_acquire.v
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

    // Streaming data interface:
    // NOTE: Doesn't need to initiate transfers, but data is valid whenever
    //   `data_ready` is asserted.
    input          data_ready,
    output         data_request,
    input [NSB:0]  data_in,

    // SPI status flags:
    input          spi_busy_i,

    // Visibilities status-flags, and settings:
    output         vx_cyc_o, // Wishbone-like (master) bus interface,
    output         vx_stb_o, // for reading back visibilities
    output         vx_we_o,
    input          vx_ack_i,
    input          vx_wat_i,
    input          vx_rty_i,
    input          vx_err_i,
    output [XSB:0] vx_adr_o, // Visibilities block to access
    input [BSB:0]  vx_dat_i,
    output [BSB:0] vx_dat_o,

    input          overflow_i,
    input          newblock_i,
    input          streamed_i,
    input [MSB:0]  checksum_i, // TODO:
    output         accessed_o,
    output         available_o,

    output [MSB:0] blocksize_o,
    output reg     vx_enabled_o = 1'b0,
    output reg     vx_overwrite_o = 1'b0,
    input          vx_stuck_i,
    input          vx_limp_i,

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
   wire            cyc_w, stb_w, ack_w, ack_c, x_ack;
   reg             ack = 1'b0;
   reg [BSB:0]     dat = {BBITS{1'b0}};
   wire [BSB:0]    dat_w;

   //  Acquisition unit signals and variables.
   wire [BSB:0]    ax_stream, aq_status, aq_debug, aq_system;
   wire [KSB:0]    ax_data;
	 reg [2:0]       aq_delay = 3'h0;

   //  Visibilities/correlator unit settings and signals.
   reg             vx_set = 1'b0, vx_clr = 1'b1;
   wire [BSB:0]    vx_stream, vx_status, vx_system;
   reg [4:0]       log_block = 5'h0; // = log2(#viz/block);
   reg [MSB:0]     blocksize = {ACCUM{1'b0}};
   reg             accessed = 1'b0, available = 1'b0;

   //  Bank address counter signals.
   reg [XSB:0]     new_blk = {XBITS{1'b0}};
   reg             upd_blk = 1'b0;
   wire            bs_new_w;
   reg             bs_upd = 1'b0;

   //  Wishbone signals to the (visibilities) streaming read-back unit.
   reg [XSB:0]     adr = {XBITS{1'b0}};
   reg             viz = 1'b0;
   wire [XBITS:0]  nxt;


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
   assign ack_w     = ack || x_ack;
   assign dat_w     = viz ? vx_dat_i : dat;


   //-------------------------------------------------------------------------
   //  Wishbone <--> vizibilities signals.
   //-------------------------------------------------------------------------
   //  After every SPI transaction the visibilities streaming module is reset,
   //  so unless reading back raw acquisition data, shouldn't cause banks to
   //  be lost (and the bank index can be set via the SPI bus anyway).
   assign vx_cyc_o  = cyc_i;
   assign vx_stb_o  = stb_i && (adr_i == `VX_STREAM || !CHECK);
   assign vx_we_o   = VIZWR ? we_i  : 1'b0;
   assign vx_adr_o  = adr;
   assign vx_dat_o  = VIZWR ? dat_i : {BBITS{1'b0}};

   //-------------------------------------------------------------------------
   //  Group signals into registers.
   //-------------------------------------------------------------------------
   //  Data acquisition streaming, status, and control registers.
   assign aq_debug  = {aq_debug_o, vx_stuck_i, vx_limp_i, {(BBITS-5){1'b0}}, aq_count_o, aq_shift_o};
   assign aq_status = {aq_adr_i[7:0]};
   assign aq_system = {aq_enabled_o, aq_valid_i, {(BBITS-5){1'b0}}, aq_delay};

   assign aq_delay_o = aq_delay;

   //  Visibilities registers.
   assign vx_status = {available, accessed, overflow_i, {(5-XBITS){1'b0}}, adr};
   assign vx_system = {vx_enabled_o, vx_overwrite_o, {(BBITS-7){1'b0}}, log_block};

   assign accessed_o  = accessed;
   assign available_o = available;
   assign blocksize_o = blocksize;



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



   //-------------------------------------------------------------------------
   //
   //  WISHBONE (SLAVE, SPEC B4) BUS INTERFACE.
   //
   //-------------------------------------------------------------------------
   //  Classic, pipelined Wishbone bus cycles can require at least one wait-
   //  state between each transfer, which is achieved here by preventing ACK
   //  being asserted for two consecutive cycles.
   assign ack_c = PIPED ? 1'b0 : ack;

   //  Visibilities data acknowledge?
   assign x_ack = PIPED ? viz && vx_ack_i : stb_w && vx_ack_i;

   assign nxt = adr + 1;


   //-------------------------------------------------------------------------
   //  Generate acknowledges for incoming requests.
   always @(posedge clk_i)
     if (rst_i && RESET)
       ack <= #DELAY 1'b0;
     else if (stb_w && !ack_c && adr_i != `VX_STREAM)
       ack <= #DELAY 1'b1;
     else if (!ASYNC && x_ack)
       ack <= #DELAY 1'b1;
     else
       ack <= #DELAY cyc_w && x_ack;

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

         //  Visibilities registers:
         `VX_STREAM: dat <= #DELAY vx_dat_i;
         `VX_STATUS: dat <= #DELAY vx_status;
         `VX_SYSTEM: dat <= #DELAY vx_system;

         default:    dat <= #DELAY 8'bx;
       endcase // case (adr_i)


   //-------------------------------------------------------------------------
   //  Wishbone (SPEC B4) bus for reading back visibilities.
   //-------------------------------------------------------------------------
   //  At the beginning of each bus cycle, see if this is a register-access,
   //  or a visibilities read-back.
   always @(posedge clk_i)
     if (rst_i && RESET || !spi_busy_i)
       viz <= #DELAY 1'b0;
     else if (stb_w)
       viz <= #DELAY adr_i == `VX_STREAM;
     else
       viz <= #DELAY viz;

   //-------------------------------------------------------------------------
   //  When using correlators in SDP-mode, there are `2^4 == 16` banks of
   //  stored visibilities that can be read back. Every time a bank has been
   //  streamed out of this device (via the SPI interface), the bank-counter
   //  is incremented.
   always @(posedge clk_i)
     if (rst_i)
       adr <= #DELAY {XBITS{1'b0}};
`ifdef __USE_SETTABLE_BLOCK_COUNTER
     else if (upd_blk)
       adr <= #DELAY new_blk;
`endif
     else
       adr <= #DELAY streamed_i ? nxt[XSB:0] : adr;


   
   //-------------------------------------------------------------------------
   //
   //  MANAGE SYSTEM FLAGS.
   //
   //-------------------------------------------------------------------------
   //  Aqusition & visibilities register writes.
   always @(posedge clk_i)
     if (rst_i) begin
        vx_set         <= #DELAY 1'b0;
        vx_clr         <= #DELAY 1'b1;
        aq_enabled_o   <= #DELAY 1'b0;
        aq_delay       <= #DELAY 3'h0;
        upd_blk        <= #DELAY 1'b0;
        aq_debug_o     <= #DELAY 1'b0;
        aq_shift_o     <= #DELAY 1'b0;
        aq_count_o     <= #DELAY 1'b0;
        vx_overwrite_o <= #DELAY 1'b0;
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
         //  Visibilities status register:
         //  NOTE: Set elsewhere.
         `VX_STATUS: begin
            new_blk <= #DELAY dat_i[XSB:0];
            upd_blk <= #DELAY 1'b1;
         end
         //  Visibilities control register:
         `VX_SYSTEM: begin
            vx_set         <= #DELAY dat_i[BSB];
            vx_clr         <= #DELAY ~dat_i[BSB];
            vx_overwrite_o <= #DELAY dat_i[BSB-1];
            log_block      <= #DELAY dat_i[4:0];
         end
       endcase // case (adr_i)
     else begin
        upd_blk <= #DELAY 1'b0;
        vx_set  <= #DELAY 1'b0;
        vx_clr  <= #DELAY 1'b0;
     end


   //-------------------------------------------------------------------------
   //  Visibilities access and control circuit.
   //-------------------------------------------------------------------------
   //  Enable the visibilities unit when a write is performed to the control-
   //  register, and disable it upon overflow, if overwrite mode is disabled.
   always @(posedge clk_i)
     if (rst_i && RESET || overflow_i && !vx_overwrite_o || vx_clr)
       vx_enabled_o <= #DELAY 1'b0;
     else if (vx_set)
       vx_enabled_o <= #DELAY 1'b1;
     else
       vx_enabled_o <= #DELAY vx_enabled_o;

   //-------------------------------------------------------------------------
   //  Set the blocksize when a write is performed to the control-register.
   assign bs_new_w = stb_w && we_i && !ack_c && adr_i == `VX_SYSTEM;

   always @(posedge clk_i) begin
      bs_upd <= #DELAY bs_new_w && !bs_upd;
      if (bs_upd)
`ifdef  __LOOKUP_BLOCKSIZE
        case (log_block)
          0:  blocksize <= #DELAY        0;
          1:  blocksize <= #DELAY        1;
          2:  blocksize <= #DELAY        3;
          3:  blocksize <= #DELAY        7;
          4:  blocksize <= #DELAY       15;
          5:  blocksize <= #DELAY       31;
          6:  blocksize <= #DELAY       63;
          7:  blocksize <= #DELAY      127;
          8:  blocksize <= #DELAY      255;
          9:  blocksize <= #DELAY      511;
          10: blocksize <= #DELAY     1023;
          11: blocksize <= #DELAY     2047;
          12: blocksize <= #DELAY     4095;
          13: blocksize <= #DELAY     8191;
          14: blocksize <= #DELAY    16383;
          15: blocksize <= #DELAY    32767;
          16: blocksize <= #DELAY    65535;
          17: blocksize <= #DELAY   131071;
          18: blocksize <= #DELAY   262143;
          19: blocksize <= #DELAY   524287;
          20: blocksize <= #DELAY  1048575;
          21: blocksize <= #DELAY  2097151;
          22: blocksize <= #DELAY  4194303;
          23: blocksize <= #DELAY  8388607;
          default:
            blocksize   <= #DELAY 16777215;
        endcase // case (log_block)
`else
      //  TODO: How slow is computing the new block-size?
      blocksize <= #DELAY (1 << dat_i[4:0]) - 1;
`endif
   end


   //-------------------------------------------------------------------------
   //  Monitor access to each new bank.
   //  TODO: Reading an old bank would still assert this flag, and is this
   //    sensible behaviour?
   always @(posedge clk_i)
     if (rst_i && RESET || newblock_i)
       accessed <= #DELAY 1'b0;
     else
       accessed <= #DELAY accessed || x_ack;

   //  Monitor when a new bank becomes available.
   always @(posedge clk_i)
     if (rst_i)
       available <= #DELAY 1'b0;
     else if (newblock_i)
       available <= #DELAY 1'b1;
     else if (x_ack)
       available <= #DELAY 1'b0;



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
       .dram_ready(data_ready),
       .dram_request(data_request),
       .dram_data(data_in),
       .data_sent(data_sent),
       .fetched_data(ax_data)
       );


endmodule // tart_acquire
