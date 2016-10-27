`timescale 1ns/100ps
/*
 * Module      : verilog/tart_acquire_old.v
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
 * TODO:
 *  + the block-access mechanism is currently not very flexible -- ideally,
 *    the block-counter would increment once all visibilities have been read
 *    back from the current block?
 *  + upgrade to WB SPEC B4;
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

module tart_acquire_old
  #(parameter WIDTH = 8,        // WB-like bus data-width
    parameter MSB   = WIDTH-1,
    parameter ACCUM = 24,       // #bits of the viz accumulators
    parameter XSB   = ACCUM-1,
    parameter BBITS = 4,
    parameter BSB   = BBITS-1,
    parameter AXNUM = 24,
    parameter NSB   = AXNUM-1,
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input              clk_i,
    input              rst_i,
    input              cyc_i,
    input              stb_i,
    input              we_i,
    output reg         ack_o = 1'b0,
    output             wat_o,
    output             rty_o,
    output             err_o,
    input [3:0]        adr_i,
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o = {WIDTH{1'b0}},

    // Streaming data interface:
    // NOTE: Doesn't need to initiate transfers, but data is valid whenever
    //   `data_ready` is asserted.
    input              data_ready,
    output             data_request,
    input [NSB:0]      data_in,

    // SPI status flags:
    input              spi_busy,

    // Visibilities status-flags, and settings:
    output             vx_cyc_o, // Wishbone-like (master) bus interface,
    output             vx_stb_o, // for reading back visibilities
    output             vx_we_o,
    input              vx_ack_i,
    input              vx_wat_i,
    input              vx_rty_i,
    input              vx_err_i,
    output reg [BSB:0] vx_adr_o = {BBITS{1'b0}}, // Visibilities block to access
    input [MSB:0]      vx_dat_i,
    input              overflow,
    input              newblock,
    input              streamed,
    input [XSB:0]      checksum, // TODO:
    output reg         accessed = 1'b0,
    output reg         available = 1'b0,

    (* ASYNC_REG = "TRUE" *)
    output reg [XSB:0] blocksize = 1'b0, // = #viz/block - 1;
    output reg         vx_enabled = 1'b0,
    output reg         vx_overwrite = 1'b0,
    input              vx_stuck_i,
    input              vx_limp_i,

    (* ASYNC_REG = "TRUE" *)
    output reg         aq_enabled = 1'b0,
    output reg         aq_debug_mode = 1'b0,
	  output reg [2:0]   aq_sample_delay = 3'h0,
    input [24:0]       aq_adr_i
    );


   //-------------------------------------------------------------------------
   //  Signals for the WishBone-like bus, for streaming back visibilities.
   //-------------------------------------------------------------------------
   assign wat_o    = 1'b0;  // these aren't used/needed by this module
   assign rty_o    = 1'b0;
   assign err_o    = 1'b0;     // TODO: pass out any submodule errors?

   assign vx_cyc_o = cyc_i;
   assign vx_stb_o = stb_i && adr_i == `VX_STREAM;
   assign vx_we_o  = 1'b0;


   //-------------------------------------------------------------------------
   //  Group signals into registers.
   //-------------------------------------------------------------------------
   //  Data acquisition streaming, status, and control registers.
   wire [MSB:0]        ax_stream, aq_status, aq_debug, aq_system;
   wire [WIDTH*3-1:0]  ax_data;

   assign aq_debug  = {aq_debug_mode, vx_stuck_i, vx_limp_i, {(WIDTH-3){1'b0}}};
   assign aq_status = {aq_adr_i[7:0]};
   assign aq_system = {aq_enabled, {(WIDTH-4){1'b0}}, aq_sample_delay};

   //  Visibilities registers.
   wire [MSB:0]        vx_stream, vx_status, vx_system;
   reg [4:0]           log_block = 0; // = log2(#viz/block);

   assign vx_status = {available, accessed, overflow, {(5-BBITS){1'b0}}, vx_adr_o};
   assign vx_system = {vx_enabled, vx_overwrite, {(WIDTH-7){1'b0}}, log_block};


   //-------------------------------------------------------------------------
   //  Manage system flags.
   //-------------------------------------------------------------------------
   wire                x_ack;

   assign x_ack = cyc_i && stb_i && !we_i && !ack_o &&
                  adr_i == `VX_STREAM && vx_ack_i;

   always @(posedge clk_i)
     if (newblock) accessed <= #DELAY 1'b0;
     else          accessed <= #DELAY accessed || x_ack;

   always @(posedge clk_i)
     if (rst_i)         available <= #DELAY 1'b0;
     else if (newblock) available <= #DELAY 1'b1;
     else if (x_ack)    available <= #DELAY 1'b0;


   //-------------------------------------------------------------------------
   //  Wishbone-like (slave) bus interface logic.
   //-------------------------------------------------------------------------
   //  Classic, pipelined Wishbone bus cycles can require at least one wait-
   //  state between each transfer, which is achieved here by preventing ACK
   //  being asserted for two consecutive cycles.
`ifdef __WB_CLASSIC
   wire                ack_c = ack_o;
`else
   wire                ack_c = 1'b0;
`endif

   //  Generate acknowledges for incoming requests.
   always @(posedge clk_i)
     if (rst_i)
       ack_o <= #DELAY 1'b0;
     else if (cyc_i && stb_i && !ack_c) begin
        if (adr_i == `VX_STREAM)
          ack_o <= #DELAY vx_ack_i;
        else
          ack_o <= #DELAY 1'b1;
     end
     else
       ack_o <= #DELAY 1'b0;

   //-------------------------------------------------------------------------
   //  Aqusition & visibilities register reads.
   always @(posedge clk_i)
     if (cyc_i && stb_i && !we_i && !ack_c)
       case (adr_i)
         //  Antenna-data access registers:
         `AX_STREAM: dat_o <= #DELAY ax_stream;
         `AX_DATA1:  dat_o <= #DELAY ax_data[23:16];
         `AX_DATA2:  dat_o <= #DELAY ax_data[15: 8];
         `AX_DATA3:  dat_o <= #DELAY ax_data[ 7: 0];

         //  Acquisition status, and control, registers:
         `AQ_STATUS: dat_o <= #DELAY aq_status;
         `AQ_DEBUG:  dat_o <= #DELAY aq_debug;
         `AQ_SYSTEM: dat_o <= #DELAY aq_system;

         //  Visibilities registers:
         `VX_STREAM: dat_o <= #DELAY vx_dat_i;
         `VX_STATUS: dat_o <= #DELAY vx_status;
         `VX_SYSTEM: dat_o <= #DELAY vx_system;

         default:    dat_o <= #DELAY 8'bx;
       endcase // case (adr_i)

   //-------------------------------------------------------------------------
   //  Aqusition & visibilities register writes.
   reg [BSB:0] new_blk = {BBITS{1'b0}};
   reg         upd_blk = 1'b0;
   reg         vx_enable = 1'b0, vx_disable = 1'b0;

   always @(posedge clk_i)
     if (rst_i) begin
        vx_enable       <= #DELAY 1'b0;
        aq_enabled      <= #DELAY 1'b0;
        aq_debug_mode   <= #DELAY 1'b0;
        aq_sample_delay <= #DELAY 3'h0;
        upd_blk         <= #DELAY 1'b0;
     end
     else if (cyc_i && stb_i && we_i && !ack_c)
       case (adr_i)
         `AQ_DEBUG:  begin
            aq_debug_mode   <= #DELAY dat_i[MSB];
         end
         //  Acquisition control register:
         `AQ_SYSTEM: begin
            aq_sample_delay <= #DELAY dat_i[2:0];
            aq_enabled      <= #DELAY dat_i[MSB];
         end
         //  Visibilities status register:
         //  NOTE: Set elsewhere.
         `VX_STATUS: begin
            new_blk <= #DELAY dat_i[BSB:0];
            upd_blk <= #DELAY 1'b1;
         end
         //  Visibilities control register:
         `VX_SYSTEM: begin
            vx_enable    <= #DELAY dat_i[MSB];
            vx_disable   <= #DELAY ~dat_i[MSB];
            vx_overwrite <= #DELAY dat_i[MSB-1];
            log_block    <= #DELAY dat_i[4:0];
         end
       endcase // case (adr_i)
     else begin
        upd_blk    <= #DELAY 1'b0;
        vx_enable  <= #DELAY 1'b0;
        vx_disable <= #DELAY 1'b0;
     end


   //-------------------------------------------------------------------------
   //  Visibilities access and control circuit.
   //-------------------------------------------------------------------------
   wire                bs_new_w;
   reg                 bs_upd = 1'b0;

   assign bs_new_w = cyc_i && stb_i && we_i && !ack_o && adr_i == `VX_SYSTEM;

   //-------------------------------------------------------------------------
   //  Enable the visibilities unit when a write is performed to the control-
   //  register, and disable it upon overflow, if overwrite mode is disabled.
   always @(posedge clk_i)
     if (rst_i)
       vx_enabled <= #DELAY 1'b0;
     else if (vx_enable)
       vx_enabled <= #DELAY 1'b1;
     else if (vx_disable || overflow && !vx_overwrite)
       vx_enabled <= #DELAY 1'b0;

   //-------------------------------------------------------------------------
   //  Set the blocksize when a write is performed to the control-register.
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
   //  When using correlators in SDP-mode, there are `2^5 == 32` blocks of
   //  stored visibilities that can be read back. Every time a new block
   //  becomes available, the block-counter is incremented.
   //  TODO: Should there be some mechanism for controlling which block is to
   //    be accessed?
   wire [BBITS:0] vx_next = vx_adr_o + 1;

   always @(posedge clk_i)
     if (rst_i)
       vx_adr_o <= #DELAY {BBITS{1'b0}};
`ifdef __USE_SETTABLE_BLOCK_COUNTER
     else if (upd_blk)
       vx_adr_o <= #DELAY new_blk;
`endif
     else
       vx_adr_o <= #DELAY streamed ? vx_next[BSB:0] : vx_adr_o;


   //-------------------------------------------------------------------------
   //  DRAM prefetcher-control logic.
   //-------------------------------------------------------------------------
   reg                 data_sent = 0;
   reg [1:0]           index = 0;
   wire                send, wrap_index;
   wire [1:0]          next_index = wrap_index ? 0 : index + 1;

   assign ax_stream  = index == 0 ? ax_data[23:16] :
                       index == 1 ? ax_data[15: 8] :
                       ax_data[7:0];

   assign send       = cyc_i && stb_i && !we_i && !ack_o && adr_i == `AX_STREAM;
   assign wrap_index = index == 2;

   //-------------------------------------------------------------------------
   //  Increment the current antenna-data index, and prefetch more data as
   //  needed.
   always @(posedge clk_i)
     if (rst_i) data_sent <= #DELAY 0;
     else       data_sent <= #DELAY wrap_index && send;

   always @(posedge clk_i)
     if (!spi_busy) index <= #DELAY 0;
     else if (send) index <= #DELAY next_index;


   //-------------------------------------------------------------------------
   //  DRAM prefetch logic core.
   //-------------------------------------------------------------------------
   dram_prefetch #( .WIDTH(24) ) DRAM_PREFETCH0
     ( .clk(clk_i),
       .rst(rst_i),
       .dram_ready(data_ready),
       .dram_request(data_request),
       .dram_data(data_in),
       .data_sent(data_sent),
       .fetched_data(ax_data)
       );


endmodule // tart_acquire_old
