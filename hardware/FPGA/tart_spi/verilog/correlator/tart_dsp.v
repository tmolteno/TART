`timescale 1ns/100ps
/*
 * Module      : verilog/correlator/tart_dsp.v
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
 * Top-level of the TART DSP units, which contain the time-multiplexed block
 * of correlator-blocks.
 * 
 * The visibilities are computed by 24 correlators, each with 12x time-
 * multiplexing, so that 576 correlations are performed for each antenna
 * sample (with default settings).
 * 
 * The rough schematic/flow-diagram of this module:
 * 
 *                    24
 * sig  o-------------/-----------+      (Bus clock domain, 98.208 MHz)
 *                                |
 *                +-----------+   |   +-------------+
 *                |           |   +-> |             |
 *  WB  o===<>==> |  Control  |       | Correlators | <=== WB ==<>
 *          ||    |           | <-+-> |             |           ||
 *          ||    +-----------+   |   +-------------+           ||
 *          ||                    |                             ||
 *          ||                    +-------------------+         \/
 *          ||                                        |   +--------------+
 *          ||    +------------+       +----------+   +-> |              |
 *          ||    |            |       |  Port A  | <===> | Visibilities |
 *          <>==> | Vis-stream |       + -  --  - +       |   Prefetch   |
 *                |            | <===> |  Port B  |       |              |
 *                +------------+       +----------+       +--------------+
 *                                       (SRAM's)
 * 
 * where just the data-paths are shown; i.e., the flags are not shown.
 * 
 * 
 * REGISTERS:
 *  Reg#   7           6           5          4      3     2     1     0
 *      -------------------------------------------------------------------
 *   00 ||                 VISIBILITIES STREAM DATA                      ||
 *      ||                            (RO)                               ||
 *      -------------------------------------------------------------------
 *   01 || AVAILABLE | ACCESSED  | OVERFLOW | 1'b0 |         BANK        ||
 *      ||    (RO)   |   (RO)    |   (RO)   |      |        (R/W)        ||
 *      -------------------------------------------------------------------
 *   10 ||   STUCK   |   LIMP    |                  6'h00                ||
 *      ||    (RO)   |   (RO)    |                                       ||
 *      -------------------------------------------------------------------
 *   11 ||  ENABLED  | OVERWRITE |   1'b0   |         BLOCK SIZE         ||
 *      ||   (R/W)   |   (R/W)   |          |            (R/W)           ||
 *      -------------------------------------------------------------------
 * 
 * By default, the DSP/visibilities unit has address 7'b100_00xx.
 * 
 * NOTE:
 *  + currently specialised for a 24 antenna setup, so your mileage will
 *    vary if you change 'AXNUM' to any other value;
 *  + the correlators are set to use 12:1 time-multiplexing, so `clk_x` must
 *    be 12x the frequency of the sampling clock;
 *  + the number of bits in a block's counter corresponds to the maximum size
 *    of the accumulator, because the visibilities are monotone increasing;
 * 
 * TODO:
 *  + better tagging of the signals so that they show their clock-domains;
 *  + compute the exponent of the count-size;
 *  + status registers, and correctly handle overflows;
 *  + disable the correlators to prevent overwriting data, if that mode has
 *    been selected (TEST);
 * 
 */

`include "tartcfg.v"


//----------------------------------------------------------------------------
// DSP registers for visibilities-data, status, and control.
//----------------------------------------------------------------------------
`define DSP_STREAM 2'b00
`define DSP_STATUS 2'b01
`define DSP_DEBUG  2'b10
`define DSP_SYSTEM 2'b11


module tart_dsp
  #(// Raw-data parameters:
    parameter AXNUM = 24,      // Number of antennae
    parameter NSB   = AXNUM-1, // MSB of the antenna signal

    //  Visibilities/correlator settings:
    parameter ACCUM = 24,      // Bit-width of the accumulators
    parameter BLOCK = ACCUM,   // Maximum #bits of the block-size
    parameter MSB   = BLOCK-1, // Data transfer MSB
    parameter TRATE = 12,      // Time-multiplexing rate
    parameter TBITS = 4,       // Number of bits for TMUX counter

    //  Visibilities-bus settings:
    parameter NREAD = 576,     // Number of visibilities to read back
    parameter RBITS = 10,      // = ceiling{log2(NREAD)}
    parameter RSB   = RBITS-1, // MSB of read-back address

    //  Parameters for the bank addresses:
    parameter XBITS = 4,       // Bit-width of the block-counter
    parameter XSB   = XBITS-1, // MSB of the block-counter

    //  Correlator-bus settings:
    parameter CBITS = XBITS+RBITS,
    parameter CSB   = CBITS-1,

    //  Wishbone bit-width parameters:
    parameter BBITS = 8,       // Bit-width of the SoC Wishbone bus
    parameter BSB   = BBITS-1, // Bus data MSB
    parameter WSB   = BBITS-2, // SPI -> WB bus address-width
    parameter ABITS = 12,      // WB bus address bit-width
    parameter ASB   = ABITS-1, // Address MSB

    //  Wishbone mode parameters:
    parameter PIPED = 1,       // Wishbone SPEC B4 mode (0/1)?
    parameter CHECK = 1,       // Sanity checking when on a bus (0/1)?
    parameter ASYNC = 1,       // synchronous cycles (0/1/2)?
    parameter RESET = 1,        // enable fast-resets (0/1)?
    parameter VIZWR = 0,       // allow visibilities-buffer writes (0/1)?

     //  Simulation-only parameters:
    parameter NOISY = 0,       // display extra debug info?
    parameter DELAY = 3)       // simulated combinational delay (ns)
   (
    input          clk_x, // correlator clock
    input          clk_i, // bus clock
    input          rst_i, // global reset

    //  Oversampled signal inputs (bus domain).
    input          vld_i,
    input          new_i,
    input [NSB:0]  sig_i,

    //  Wishbone-like bus interface for reading visibilities.
    input          cyc_i,
    input          stb_i,
    input          we_i, // writes only work for system registers
    output         ack_o,
    output         wat_o,
    output         rty_o,
    output         err_o,
    input [1:0]    adr_i, // selects the DSP register to access
    input [7:0]    dat_i,
    output [7:0]   dat_o,

    //  Streaming read-back unit signals.
    input          sce_i, // Stream CE (0/1)?

    //  Status output signals.
    output         enabled_o,
    output         pending_o,
    output         switched_o,
    output         newblock_o,
    output         streamed_o,
    output [31:0]  checksum_o,

    //  Debugging signals.
    output         overflow_o,
    output [XSB:0] bank_o,
    output         stuck_o,
    output         limp_o
    );


   //-------------------------------------------------------------------------
   //  Wishbone (external) interface helper-signals.
   //-------------------------------------------------------------------------
   reg             ack = 1'b0, viz = 1'b0;
   reg [BSB:0]     dat;
   wire            cyc_w, stb_w, ack_w, fetch, store;


   //-------------------------------------------------------------------------
   //  WB signals to access the prefetched visibilities (that have been
   //  stored within a block SRAM).
   //-------------------------------------------------------------------------
   wire [MSB:0]    v_drx, v_dtx;
   wire [RSB:0]    v_adr;          // read-back address
   wire [CSB:0]    c_adr;          // bank + read-back address
   wire            v_cyc, v_stb, v_we; // Master WB control signals
   wire            v_ack, v_wat, v_rty, v_err; // Slave WB response signals

   //-------------------------------------------------------------------------
   //  Signals for the dual-port SRAM's.
   wire            sram_a_ce, sram_a_we; // Port used to store visibilities
   wire [RSB:0]    sram_a_ad;
   wire [3:0]      sram_a_be;
   wire [31:0]     sram_a_di, sram_a_do;

   wire            sram_b_ce, sram_b_we; // Port to read back visibilities
   wire [ASB:0]    sram_b_ad;
   wire            sram_b_be;
   wire [7:0]      sram_b_di, sram_b_do;


   //-------------------------------------------------------------------------
   //  DSP state registers, and status signals.
   //-------------------------------------------------------------------------
   reg [4:0]       log_block = 5'h0;
   reg             enabled = 1'b0, accessed = 1'b0, available = 1'b0;
   reg             overwrite = 1'b0, overflow = 1'b0;
   wire            switched, newblock, wrapped;
   wire [31:0]     checksum;
   reg [MSB:0]     blocksize = {ACCUM{1'b0}};

   // DSP banks.
   reg [XSB:0]     bank = 4'h0;
   wire [XBITS:0]  next;

   //  DSP registers.
   wire [BSB:0]    dsp_stream, dsp_status, dsp_debug, dsp_system;
   reg             dsp_set = 1'b0, dsp_clr = 1'b0;
   reg             new_bnk = 1'b0, upd_bnk = 1'b0, bs_upd = 1'b0;
   wire            bs_new_w;

   //  Correlator-domain signals.
   wire            swp_x;


   //-------------------------------------------------------------------------
   //  Assignments for the DSP's status-flags.
   //-------------------------------------------------------------------------
   assign enabled_o  = enabled;
   assign pending_o  = available;
   assign newblock_o = newblock;
   assign streamed_o = wrapped;
   assign switched_o = switched;
   assign checksum_o = checksum;

   //  Debug signal assignments.
   assign overflow_o = overflow;
   assign bank_o     = bank;
   assign stuck_o    = stuck;
   assign limp_o     = limp;

   //  Signal-update assignments.
   assign next       = bank + 1;


   //-------------------------------------------------------------------------
   //  Map the output signals to the system WishBone bus.
   //-------------------------------------------------------------------------
   assign ack_o      = ack;
   assign wat_o      = 1'b0;     // never needs to wait
   assign rty_o      = 1'b0;     // accesses always succeed, so no retries
   assign err_o      = 1'b0;     // TODO: assert for invalid correlator access?
   assign dat_o      = dat;

   //  Drive the data bus with either visibilities, or system registers.
   assign cyc_w      = CHECK ? cyc_i : 1'b1;
   assign stb_w      = cyc_w && stb_i;
   assign ack_w      = stb_w && adr_i != `DSP_STREAM || viz;
   assign fetch      = stb_w && !we_i;
   assign store      = stb_w &&  we_i;


   //-------------------------------------------------------------------------
   //  Group signals into registers.
   //-------------------------------------------------------------------------
   //  Visibilities registers.
   assign dsp_stream = sram_b_do;
   assign dsp_status = {available,  accessed, overflow, 1'b0, bank};
   assign dsp_debug  = {    stuck,      limp,                 6'h0};
   assign dsp_system = {  enabled, overwrite,     1'b0,  log_block};



   //-------------------------------------------------------------------------
   //     
   //  DSP SETTINGS (SET USING THE WISHBONE INTERCONNECT).
   //     
   //-------------------------------------------------------------------------
   //  Generate acknowledges for incoming requests.
   always @(posedge clk_i)
     if (rst_i && RESET)
       ack <= #DELAY 1'b0;
     else
       ack <= #DELAY ack_w;


   //-------------------------------------------------------------------------
   //  Acquisition & visibilities register reads.
   always @(posedge clk_i)
     if (stb_w)
       case (adr_i)
         `DSP_STREAM: dat <= #DELAY dsp_stream;
         `DSP_STATUS: dat <= #DELAY dsp_status;
         `DSP_DEBUG:  dat <= #DELAY dsp_debug;
         `DSP_SYSTEM: dat <= #DELAY dsp_system;
         default:     dat <= #DELAY 8'bx;
       endcase // case (adr_i)


   //-------------------------------------------------------------------------
   //  Wishbone (SPEC B4) bus for reading back visibilities.
   //-------------------------------------------------------------------------
   //  At the beginning of each bus cycle, see if this is a register-access,
   //  or a visibilities read-back.
   always @(posedge clk_i)
     if (rst_i && RESET)
       viz <= #DELAY 1'b0;
     else
       viz <= #DELAY stb_w && adr_i == `DSP_STREAM;


   
   //-------------------------------------------------------------------------
   //
   //  MANAGE SYSTEM FLAGS.
   //
   //-------------------------------------------------------------------------
   //  Acquisition & visibilities register writes.
   always @(posedge clk_i)
     if (rst_i) begin
        if (RESET) begin
           dsp_set   <= #DELAY 1'b0;
           dsp_clr   <= #DELAY 1'b1;
           upd_bnk   <= #DELAY 1'b0;
        end
        overwrite <= #DELAY 1'b0;
     end
     else if (store)
       case (adr_i)
         //  Visibilities status register:
         //  NOTE: Set elsewhere.
         `DSP_STATUS: begin
            new_bnk <= #DELAY dat_i[XSB:0];
            upd_bnk <= #DELAY 1'b1;
         end
         //  Visibilities control register:
         `DSP_SYSTEM: begin
            dsp_set   <= #DELAY dat_i[BSB];
            dsp_clr   <= #DELAY ~dat_i[BSB];
            overwrite <= #DELAY dat_i[BSB-1];
            log_block <= #DELAY dat_i[4:0];
         end
       endcase // case (adr_i)
     else begin
        upd_bnk <= #DELAY 1'b0;
        dsp_set <= #DELAY dsp_set ? !enabled : 1'b0;
        dsp_clr <= #DELAY 1'b0;
     end


   //-------------------------------------------------------------------------
   //  Visibilities-unit control-signals.
   //-------------------------------------------------------------------------
   //  Once the 'enable' flag (of the visibilities unit) is set, then wait
   //  for valid & new data before enabling the DSP pipeline.
   //  NOTE: Disable it upon overflow, if overwrite mode is disabled.
   always @(posedge clk_i)
     if (rst_i || overflow && !overwrite || dsp_clr)
       enabled <= #DELAY 1'b0;
     else if (dsp_set && vld_i && new_i)
       enabled <= #DELAY 1'b1;
     else
       enabled <= #DELAY enabled;

   //-------------------------------------------------------------------------
   //  Set the blocksize when a write is performed to the control-register.
   assign bs_new_w = store && adr_i == `DSP_SYSTEM;

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
   //  When using correlators in SDP-mode, there are `2^4 == 16` banks of
   //  stored visibilities that can be read back. Every time a bank has been
   //  streamed out of this device (via the SPI interface), the bank-counter
   //  is incremented.
   always @(posedge clk_i)
     if (rst_i)
       bank <= #DELAY {XBITS{1'b0}};
`ifdef __USE_SETTABLE_BLOCK_COUNTER
     else if (upd_bnk)
       bank <= #DELAY new_bnk;
`endif
     else
       bank <= #DELAY wrapped ? next[XSB:0] : bank;

   //-------------------------------------------------------------------------
   //  Monitor access to each new bank.
   //  TODO: Reading an old bank would still assert this flag, and is this
   //    sensible behaviour?
   always @(posedge clk_i)
     if (rst_i && RESET || newblock)
       accessed <= #DELAY 1'b0;
     else
       accessed <= #DELAY accessed || viz;

   //-------------------------------------------------------------------------
   //  Monitor when a new bank becomes available.
   always @(posedge clk_i)
     if (rst_i && RESET)
       available <= #DELAY 1'b0;
     else if (newblock)
       available <= #DELAY 1'b1;
     else if (viz)
       available <= #DELAY 1'b0;



   //-------------------------------------------------------------------------
   //     
   //  SYNCHRONISE THE SIGNALS FROM THE WISHBONE CLOCK DOMAIN.
   //     
   //-------------------------------------------------------------------------
   //  Synchroniser registers for correlator control-signals.
   (* NOMERGE = "TRUE" *)
   reg [MSB:0]  block_x = {ACCUM{1'b0}}, block_s = {ACCUM{1'b0}};

   always @(posedge clk_x) begin
      block_s  <= #DELAY blocksize;
      block_x  <= #DELAY block_s;
   end



   //-------------------------------------------------------------------------
   //     
   //  BANK-OVERWRITE DETECTION LOGIC.
   //     
   //-------------------------------------------------------------------------
`ifdef  __USE_OVERFLOW_DETECTION
   //  Makes sure that a synchroniser is synthesised, not a shift-register.
   (* NOMERGE = "TRUE" *)
   wire               overflow_x;

   (* NOMERGE = "TRUE" *)
   reg                overflow_s;

   //  Assert overflow whenever a bank-switch causes new data to be written
   //  to the bank currently being read back.
   always @(posedge clk_i) begin
      overflow_s <= #DELAY overflow_x;
      overflow   <= #DELAY overflow_s;
   end
`endif



   //-------------------------------------------------------------------------
   //     
   //     CORRELATOR AND VISIBILITIES BLOCKS.
   //     
   //-------------------------------------------------------------------------
   //  When the SPI (or any other off-board I/O) interface is inactive, hold
   //  the streaming read-back module in the reset state.
   //  NOTE: This is required for the SPI module as it prefetches ahead, and
   //    then discards any unused data, causing some visibilities to be lost.
`ifdef __USE_ASYNC_FETCH
   wire               stream_reset = ~sce_i;
`else   
   reg                stream_reset = 1'b1;

   always @(posedge clk_i)
     stream_reset <= #DELAY ~sce_i;
`endif



   //-------------------------------------------------------------------------
   //
   //  STREAMING, VISIBILITIES-READ-BACK LOGIC-CORE.
   //
   //-------------------------------------------------------------------------
   //  NOTE: After every SPI transaction the visibilities streaming module is
   //    reset, so unless reading back raw acquisition data, shouldn't cause
   //    banks to be lost (and the bank index can be set via the SPI bus
   //    anyway).
   wire               s_stb;
   wire               s_ack, s_wat, s_rty, s_err;
   wire [BSB:0]       s_dat;

   assign s_stb = CHECK ? stb_w && adr_i == `DSP_STREAM : stb_w;

   wb_sram_stream
     #(  .WIDTH(BBITS),         // Wishbone to SPI data bit-width
         .WORDS(NREAD << 2),    // number of bytes to stream
         .WBITS(ABITS),         // word-counter bit-width
         .RESET(1),             // need to reset after each SPI transfer
         .START(0),             // start address
         .TICKS(1),             // read latency of the attached SRAM's
         .READ (1),             // stream from the SRAM's
         .WRITE(VIZWR),         // typically only need read access
         .USEBE(0),             // single byte data
         .PIPED(PIPED),         // Pipelined (SPEC B4) transfers
         .ASYNC(1),             // registered outputs
         .CHECK(CHECK),         // bus is shared by several slaves
         .DELAY(DELAY)
         ) STREAM
       ( .clk_i(clk_i),
         .rst_i(stream_reset),

         //  Wishbone (SPEC B4) interface to external I/O (SPI) device:
         .cyc_i(cyc_w),
         .stb_i(s_stb),
         .we_i (we_i),
         .ack_o(s_ack),
         .wat_o(s_wat),
         .rty_o(s_rty),
         .err_o(s_err),
         .sel_i(1'b0),
         .dat_i(dat_i),
         .dat_o(s_dat),

         //  SRAM (8-bit) read interface, for the prefetched visibilities:
         .sram_ce_o(sram_b_ce),
         .sram_we_o(sram_b_we),
         .sram_be_o(sram_b_be),
         .sram_ad_o(sram_b_ad),
         .sram_do_i(sram_b_do),
         .sram_di_o(sram_b_di),
 
         .wrapped_o(wrapped)   // strobes when block has been streamed
         );


   //-------------------------------------------------------------------------
   //
   //  VISIBILITIES READ-BACK UNIT.
   //
   //-------------------------------------------------------------------------
   //  Local synchronous reset for the visibilities read-back core.
   (* NOMERGE *) reg rst_viz = 1'b1;

   always @(posedge clk_i)
     rst_viz <= #DELAY rst_i;

   //-------------------------------------------------------------------------
   parameter BSIZE = TRATE*2;   // sine & cosine components in the SRAM's
   parameter DBITS = TBITS+1;   // distributed SRAM address bits
   parameter COUNT = 24;        // correlator number/count
   parameter NBITS = 5;

   wire [31:0] v_dat;

   assign v_dtx    = v_dat[MSB:0];

   tart_visibilities
     #(  .WIDTH(32),            // SRAM data bit-width
         .XBITS(XBITS),         // number of banks of visibilities
         .COUNT(COUNT),         // prefetch from `COUNT` correlators
         .CBITS(NBITS),         // correlator-count bit-width
         .BSIZE(BSIZE),         // number of words/correlator
         .BBITS(DBITS),         // bit-width of word address
         .CHECK(CHECK),         // use extra-checking of WB signals (0/1)?
         .DELAY(DELAY)
         ) VIZ
       ( .clk_i(clk_i),
         .rst_i(rst_viz),

         //  Wishbone interconnect between correlators and prefetch SRAM:
         .cyc_o(v_cyc),
         .stb_o(v_stb),
         .we_o (v_we), 
         .ack_i(v_ack),
         .wat_i(v_wat),
         .rty_i(v_rty),
         .err_i(v_err),
         .adr_o(v_adr),
         .dat_i({{32-ACCUM{1'b0}}, v_drx}),
         .dat_o(v_dat),

         //  SRAM (32-bit) write interface, for storing the visibilities:
         .sram_ce_o(sram_a_ce),
         .sram_we_o(sram_a_we),
         .sram_be_o(sram_a_be),
         .sram_ad_o(sram_a_ad),
         .sram_do_i(sram_a_do),
         .sram_di_o(sram_a_di),

         //  Control and status signals (Wishbone domain).
         .streamed_i(wrapped),  // signals that a bank has been sent
         .newblock_o(newblock), // strobes when new block is available
         .checksum_o(checksum), // computed checksum of block

         //  Correlator-domain signals:
         .clk_x     (clk_x),
         .switch_x  (swp_x),
         .overflow_x(overflow_x)
         );



   //-------------------------------------------------------------------------
   //
   //  TOP-LEVEL CORRELATORS-BLOCK FUNCTIONAL UNIT.
   //
   //-------------------------------------------------------------------------
   //  Local synchronous reset for the correlators.
   (* NOMERGE *) reg rst_cor = 1'b1;

   always @(posedge clk_i)
     rst_cor <= #DELAY rst_i;

   //-------------------------------------------------------------------------
   //  Combined bank-index and visibility-address.
   assign c_adr = {bank, v_adr};

   tart_correlator
     #(  .ACCUM(BLOCK),         // accumulator bits
         .IBITS(AXNUM),         // data-input bits
         .ABITS(CBITS),         // correlator address bits
         .TRATE(TRATE),         // time-multiplexing rate
         .TBITS(TBITS),         // TMUX bits
         .XBITS(XBITS),         // bank-address bits
         .SEQRD(1),             // prefetch is sequential
         .DSLOW(0),             // use fast-clock settings for DSP
         .SSLOW(0),             //   and SDP correlators
         .DDUPS(0),             // no DSP duplicate address regs
         .SDUPS(1),             // use SDP duplicate address regs
         .PIPED(PIPED),         // use parent Wishbone mode
         .CHECK(CHECK),         // on bus, so do checking
         .DELAY(DELAY)
         ) COR
       ( .clk_x(clk_x),         // 12x data-rate sampling clock
         .clk_i(clk_i),         // bus and SRAM-read clock
         .rst_i(rst_cor),

         .cyc_i(v_cyc),         // the correlator connects to the read-back
         .stb_i(v_stb),         // unit for the visibilities, via this bus
         .we_i (v_we ),
         .ack_o(v_ack),
         .wat_o(v_wat),
         .rty_o(v_rty),
         .err_o(v_err),
         .adr_i(c_adr),
         .dat_i(v_dtx),
         .dat_o(v_drx),

         .vld_i(enabled),       // bus clock-domain source & framing
         .new_i(new_i),         // signal inputs
         .sig_i(sig_i),

         .switching_o(switched), // asserts on bank-switch (bus domain)

         .sums_x_i(block_x),   // number of samples per visibility sum
         .swap_x_o(swp_x),     // bank-switch strobe
         .bank_x_o()           // bank address
         );



   //-------------------------------------------------------------------------
   //
   //  PREFETCH SRAM'S.
   //
   //-------------------------------------------------------------------------
   //  Reorder the data so that it can be read out byte-by-byte, and without
   //  any reordering (as long as little endian is desired).
   wire [15:0] sram_a_hi, sram_a_lo;

   //  Swizzle the nibbles.
   assign sram_a_hi = {sram_a_di[31:28], sram_a_di[23:20],
                       sram_a_di[15:12], sram_a_di[7:4]};
   assign sram_a_lo = {sram_a_di[27:24], sram_a_di[19:16],
                       sram_a_di[11:8] , sram_a_di[3:0]};

   //  Stores the lower nibbles of the visibilities.
   RAMB16X16X4_TDP
     #( .DELAY(DELAY)
        ) SRAM0
       (.CLKA (clk_i),          // 16-bit write-only port
        .ENA  (sram_a_ce),
        .WEA  (sram_a_be[1:0]),
        .ADDRA(sram_a_ad),
        .DIA  (sram_a_lo),
        .DOA  (sram_a_do[15:0]),

        .CLKB (clk_i),          // 4-bit read-only port
        .ENB  (sram_b_ce),
        .WEB  (sram_b_we),
        .ADDRB(sram_b_ad),
        .DIB  (sram_b_di[3:0]),
        .DOB  (sram_b_do[3:0])
        );

   //  Stores the upper nibbles of the visibilities.
   RAMB16X16X4_TDP
     #( .DELAY(DELAY)
        ) SRAM1
       (.CLKA (clk_i),          // 16-bit write-only port
        .ENA  (sram_a_ce),
        .WEA  (sram_a_be[3:2]),
        .ADDRA(sram_a_ad),
        .DIA  (sram_a_hi),
        .DOA  (sram_a_do[31:16]),

        .CLKB (clk_i),          // 4-bit read-only port
        .ENB  (sram_b_ce),
        .WEB  (sram_b_we),
        .ADDRB(sram_b_ad),
        .DIB  (sram_b_di[7:4]),
        .DOB  (sram_b_do[7:4])
        );



   //-------------------------------------------------------------------------
   //     
   //     DEBUGGING STUFF.
   //     
   //-------------------------------------------------------------------------
`ifdef __RELEASE_BUILD
   wire               stuck = 1'b0;
   wire               limp  = 1'b0;

`else
   reg                stuck = 1'b0;
   reg                limp  = 1'b1;
   reg [5:0]          count = 6'h0;
   wire [5:0]         cnext = count[4:0] + 1'b1;

   //  Assert 'stuck` if transfers are taking too long.
   always @(posedge clk_i)
     if (rst_i)
       {count, stuck} <= #DELAY 7'b0;
     else if (v_cyc && v_ack) begin
        if (!count[5])
          {count, stuck} <= #DELAY {cnext, stuck};
        else
          {count, stuck} <= #DELAY {count, 1'b1};
     end
     else if (!v_cyc)
       {count, stuck} <= #DELAY {6'h0, stuck};

   //  Clear `limp` if the device is prefetching visibilities.
   always @(posedge clk_i)
     if (rst_i)
       limp <= #DELAY 1'b1;
     else if (v_cyc && v_ack && enabled)
       limp <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (stuck) begin
        #10 $display("%12t: WB stuck.", $time);
        #80 $finish;
     end
`endif // !`ifdef __RELEASE_BUILD


endmodule // tart_dsp
