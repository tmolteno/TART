`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/tart_capture.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
 * 
 * The external antennae typically operate at 16.368 MHz, driven by an off-
 * chip crystal oscillator, and the resulting raw-data signals need to be
 * captured (and, optionally aligned using a clock-recovery circuit).
 * 
 * Alternatively, for testing & configuring TART fake-data can also be
 * generated, and this fake-data can be of several types.
 * 
 * NOTE:
 *  + the `e` suffix is used to tag signals from the external (signal) clock-
 *    domain, and the `x` suffix for the (12x by default) sampling- and
 *    correlator- domain clock;
 *  + even though the sample-clock's frequency is an integer multiple of the
 *    external clock, the phase relationship is unknown (due to the quirky
 *    Spartan 6 DCM's), thus asynchronous domain-crossing techniques must be
 *    used;
 *  + the `ALIGN` parameter selects whether to oversample the antenna
 *    signals, and perform clock-recovery, if enabled (1) -- or instead to
 *    cross the clock-domain (to the correlator domain) using an asynchronous
 *    FIFO (0);
 * 
 * TODO:
 *  + setup a bunch of FROM/TO constraints for CDC;
 *  + the handling of asynchronous signals is still a mess;
 *  + floorplan into the upper-right corner?
 *  + AUTO-mode, which continually monitors (and adjusts) the phase?
 * 
 */

`include "tartcfg.v"

module tart_capture
  #(//  Bit-width parameters:
    parameter AXNUM = 24,       // number of antennae
    parameter MSB   = AXNUM-1,  // MSB of antenna signal
    parameter SBITS = 5,        // number of MUX's required select bits
    parameter SSB   = SBITS-1,  // MSB of the antenna-selects
    parameter ABITS = 20,       // DRAM controller address bit-width
    parameter ASB   = ABITS-2,  // MSB of DRAM address

    //  Fake antenna-data settings:
    parameter DEBUG = 1,        // allow debug builds & options?
    parameter MULTI = 1,        // runtime fake-data options?
    parameter RNG   = 1,        // random data?
    parameter CONST = 0,        // constant data?
    parameter CDATA = 24'h0,    // constant data value

    //  Data-alignment options:
    parameter ALIGN = 0,        // (re-)align captured data?
    parameter FSIZE = 2,        // 2^FSIZE asynchronous FIFO entries
    parameter RATIO = 12,       // oversampling ratio?
    parameter RMAX  = RATIO-1,  // maximum clock-counter value
    parameter RBITS = 4,        // bit-width of clock-counter
    parameter RSB   = RBITS-1,  // MSB of clock-counter

    //  Wisbone mode/settings:
    parameter RESET = 1,        // enable fast-resets (0/1)?
    parameter CHECK = 1,        // bus-signal sanity-checking (0/1)?
    parameter PIPED = 1,        // WB pipelined transfers (0/1)?

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clk_i, // bus clock
    input          clk_x, // capture clock
    input          clk_e, // external clock
    input          rst_i, // bus-domain reset

    //  Wishbone (SPEC B4) bus:
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output         ack_o,
    output         wat_o,
    output         rty_o,
    output         err_o,
    input [1:0]    adr_i,       // four WB-mapped registers
    input [7:0]    dat_i,       // byte-wide R/W register access
    output [7:0]   dat_o,
    
    //  Raw signal data input:
    input [MSB:0]  signal_e_i, // NOTE: external (xtal) clock domain

    //  Memory Controller Block (MCB) signals:
    output         mcb_ce_o,
    output         mcb_wr_o,
    input          mcb_rdy_i,
    output [ASB:0] mcb_adr_o,
    output [31:0]  mcb_dat_o,

    //  Fake/real acquistion data:
    output         ax_vld_x_o, // asserted when data is valid
    output         ax_new_x_o, // strobes when new data is available
    output [MSB:0] ax_dat_x_o, // present the captured data

    //  Request for acquired data (stored in the DRAM):
    input          rd_req_i,
    output [2:0]   tart_state_o // debug info
    );


   //-------------------------------------------------------------------------
   //  Antenna-data routing signals:
   //  TODO: Make sure that both are placed within the IOB's.
   (* IOB = "TRUE", NOMERGE = "TRUE" *)
   reg [MSB:0]     sig_iob0, sig_iob1;

   //-------------------------------------------------------------------------
   //  Cross-domain, capture control-signals:
   wire            rst_x, rst_e;

   //-------------------------------------------------------------------------
   //  Phase-measurement unit signals:
   reg             write_x = 1'b0;
   wire            strobe_x, locked_x, invalid_x;
   wire [MSB:0]    signal_w;

   //  NOTE: 'NOMERGE' constraint prevents these signals from being pulled
   //    into SRL primitives.
   //  TODO: Check.
   (* NOMERGE = "TRUE" *)
   reg [MSB:0]     source_x, signal_x;

   //-------------------------------------------------------------------------
   //  Acquistion-block signals:
   //  TODO: Which domains do these belong to?
   wire [MSB:0]    aq_read_data;
   wire [8:0]      aq_bb_rd_address;
   wire [8:0]      aq_bb_wr_address;
   reg [8:0]       bb_wr_ad_x = 9'h0;
   wire            bb_wr_ad_x_next;

   //-------------------------------------------------------------------------
   //  Fake/debug antenna-data signals:
   wire            debug_w;     // bus domain
   reg             en_fake_s = 1'b0, go_fake_s = 1'b0;
   reg             aq_debug_e, aq_shift_e, aq_count_e;
   wire [MSB:0]    ax_fake_e;
   reg             en_fake_e = 1'b0;
   wire            go_fake_e, fifo_ce_e, debug_e, ax_ce_e;

   //-------------------------------------------------------------------------
   //  Sample-/correlator- domain signals.
   //   (Generated or used by the data-capture and alignment unit)
   //-------------------------------------------------------------------------
   wire [MSB:0]    aq_clean_x;

   //  Synchroniser bus-domain signals:
   reg             aq_align_s = 1'b0, aq_clear_s = 1'b0, aq_retry_s = 1'b0;
   reg             aq_valid_s = 1'b0, aq_error_s = 1'b0;

   //  Correlator-domain signals:
   reg             aq_align_x = 1'b0, aq_clear_x = 1'b0, aq_retry_x = 1'b0;
   reg             aq_valid_r = 1'b0, aq_error_r = 1'b0;
   wire            aq_ready_x, aq_valid_x, aq_error_x;

   //-------------------------------------------------------------------------
   //  CDC signals for the unaligned data:
   reg [MSB:0]     dat_x;
   reg [RSB:0]     cnt_x = {RBITS{1'b0}};
   reg             new_x = 1'b0, vld_x = 1'b0;
   wire            max_x;
   wire [MSB:0]    dat_w;
   wire [RBITS:0]  nxt_x;
   reg             aq_valid_t;


   //-------------------------------------------------------------------------
   //  Wishbone bus-mapped read-back registers.
   //-------------------------------------------------------------------------
   wire [7:0]      aq_capture, aq_centre, aq_debug, aq_status; // WB regs
   reg             en_centre = 1'b0, en_capture = 1'b0;
   reg             aq_select = {SBITS{1'b0}}; // source/antenna to centre

   //-------------------------------------------------------------------------
   //  Internal signals used for the Wishbone interface:
   reg [7:0]       dat;
   reg             ack = 1'b0;
   wire            cyc_w, stb_w, write;


   //-------------------------------------------------------------------------
   //  Wishbone-related signal assignments.
   //-------------------------------------------------------------------------
   //  Wishbone-mapped register assignments:
   assign aq_capture = {en_capture, tart_state, aq_delay};
   assign aq_centre  = {aq_align, 2'b0, aq_source};
   assign aq_debug   = {aq_fake, 5'h0, aq_count, aq_shift};
   assign aq_status  = {aq_invalid, aq_locked, 2'b0, aq_phase};

   //  Internal, Wishbone, combinational signals:
   assign cyc_w = CHECK ? cyc_i : 1'b1;
   assign stb_w = cyc_w && stb_i;
   assign write = stb_w && we_i;

   //  Wishbone output assignments:
   assign ack_o = ack;
   assign wat_o = 1'b0;
   assign rty_o = 1'b0;
   assign err_o = 1'b0;
   assign dat_o = dat;


   //-------------------------------------------------------------------------
   //  Data-capture assignments.
   //-------------------------------------------------------------------------
   assign aq_valid_o = ALIGN ? aq_valid_s : aq_valid_t;
   assign aq_error_o = ALIGN ? aq_error_s : 1'b0;

   //-------------------------------------------------------------------------
   // Antenna sources MUX, for choosing real data or fake data.
   assign fifo_ce_e  = debug_e ? go_fake_e : ax_en_e;
   assign ax_en_w    = debug_w ? go_fake_s : ax_ce_w;

   assign ax_ce_w    = aq_store_i || aq_align_i;
   assign debug_w    = DEBUG && aq_debug_i;
   assign debug_e    = DEBUG && aq_debug_e;

   assign bb_wr_ad_x_next = bb_wr_ad_x + 1;


   //-------------------------------------------------------------------------
   //  Correlator-domain signal assignments.
   //-------------------------------------------------------------------------
   assign ax_vld_x_o = ALIGN ? aq_valid_x : vld_x;
   assign ax_new_x_o = ALIGN ? aq_ready_x : new_x;
   assign ax_dat_x_o = ALIGN ? aq_clean_x : dat_x;

   //-------------------------------------------------------------------------
   //  CDC signal-assignments for unaligned data.
   assign nxt_x  = cnt_x + 1;
   assign max_x  = cnt_x == RMAX;



   //-------------------------------------------------------------------------
   //
   //  WISHBONE (SPEC B4) INTERFACE.
   //
   //-------------------------------------------------------------------------
   //  Acknowledge all requests.
   always @(posedge clk_i)
     if (rst_i && RESET)
       ack <= #DELAY 1'b0;
     else
       ack <= #DELAY stb_w;

   //-------------------------------------------------------------------------
   //  Capture-register read-backs.
   always @(posedge clk_i)
     if (stb_w)
       case (adr_i)
         2'b00: dat <= #DELAY aq_capture;
         2'b01: dat <= #DELAY aq_centre;
         2'b10: dat <= #DELAY aq_debug;
         2'b11: dat <= #DELAY aq_status;
       endcase // case (adr_i)

   //-------------------------------------------------------------------------
   //  Control the raw-data capture unit.
   always @(posedge clk_i)
     if (rst_i && RESET)
       {en_capture, aq_delay} <= #DELAY {1'b0, {RBITS{1'b0}}};
     else if (write && adr_i == 2'b00)
       {en_capture, aq_delay} <= #DELAY {dat_i[7], dat_i[RSB:0]};
     else
       {en_capture, aq_delay} <= #DELAY {en_capture, aq_delay};

   //-------------------------------------------------------------------------
   //  Enable/restart the signal-capture, centring unit, and select the
   //  requested antenna/source.
   //  TODO: AUTO-mode, which continually monitors (and adjusts) the phase?
   always @(posedge clk_i)
     if (rst_i && RESET)
       {en_centre, aq_select} <= #DELAY {1'b0, {SBITS{1'b0}}};
     else if (write && adr_i == 2'b01)
       {en_centre, aq_select} <= #DELAY {dat_i[7], dat_i[SSB:0]};
     else
       {en_centre, aq_select} <= #DELAY {1'b0, aq_select};

   //-------------------------------------------------------------------------
   //  Control the debug unit, and its modes/settings.
   always @(posedge clk_i)
     if (rst_i && RESET)
       {en_debug, aq_count, aq_shift} <= #DELAY 3'h0;
     else if (write && adr_i == 2'b10)
       {en_debug, aq_count, aq_shift} <= #DELAY {dat_i[7], dat_i[1:0]};
     else
       {en_debug, aq_count, aq_shift} <= #DELAY {en_debug, aq_count, aq_shift};



   //-------------------------------------------------------------------------
   //
   //  DATA CAPTURE.
   //
   //-------------------------------------------------------------------------
   //  Capture (and supersample) the input signal, and use both IOB registers
   //  to reduce the amount of metastability.
   //  TODO: Check post PAR, to see the actual placement & routing.
   always @(posedge clk_x)
     {sig_iob1, sig_iob0} <= #DELAY signal_e_i;


   //-------------------------------------------------------------------------
   //  Sample-domain data-flow.
   //-------------------------------------------------------------------------
   always @(posedge clk_x)
     begin
        source_x <= #DELAY debug_x ? fake_x : sig_iob1;
        signal_x <= #DELAY signal_w;
        write_x  <= #DELAY strobe_x;
     end


`ifndef __RELEASE_BUILD   
   //-------------------------------------------------------------------------
   //  Fake data generation circuit, for testing & debugging.
   //-------------------------------------------------------------------------
   fake_telescope
     #(  .WIDTH(AXNUM),
         .MULTI(MULTI),
         .RNG  (RNG),
         .CONST(CONST),
         .CDATA(CDATA),
         .DELAY(DELAY)
         ) FAKE_TART0
       ( .clock_i (clk_e),
         .reset_i (rst_e),
         .enable_i(debug_e),
         .shift_i (shift_e),
         .count_i (count_e),
         .valid_o (valid_e),
         .data_o  (fake_e)
         );
`endif


   //-------------------------------------------------------------------------
   //  Measure the signal phase.
   //-------------------------------------------------------------------------
   //  NOTE: Computes the required phase-shift, to determine the relative
   //    clock-phases, so that data can be captured mid-period (of the slower
   //    clock).
   signal_centre
     #( .WIDTH(AXNUM),
        .SBITS(SBITS),
        .RATIO(RATIO),
        .RBITS(RBITS),
        .DELAY(DELAY)
        ) CENTRE
     (  .clock_i  (clk_x),
        .reset_i  (rst_x),
        .align_i  (centre_x),   // compute the alignment shift?
        .signal_i (source_x),   // from external/fake data MUX
        .select_i (select_x),   // select antenna to measure phase of
        .strobe_o (strobe_x),   // mark the arrival of a new value
        .locked_o (locked_x),   // signal is stable & locked
        .phase_o  (phase_x),    // relative phase for stable signal
        .invalid_o(invalid_x),  // signal can't lock, or lock lost
        .restart_i(restart_x)   // acknowledge, and retry to lock
        );


   //-------------------------------------------------------------------------
   //  Programmable delay.
   //-------------------------------------------------------------------------
   shift_reg
     #(  .DEPTH(16),            // should synthesise to SRL16E primitives
         .ABITS(4),
         .DELAY(DELAY)
         ) SHREG [MSB:0]
       ( .clk(clk_x),
         .ce (ce_x),
         .a  (delay_x),
         .d  (source_x),
         .q  (signal_x)
         );



   //-------------------------------------------------------------------------
   //
   //  CONTROL-SIGNAL CLOCK DOMAIN CROSSING (CDC) UNIT.
   //
   //-------------------------------------------------------------------------
   //  Performs (nearly) all CDC within one module, to make it easier to spot
   //  violations of CDC rules.
   capture_control
     #( .WIDTH(AXNUM),
        .DELAY(DELAY)
        ) CTRL0
       ( .clk_b_i(clk_i),       // bus-domain clock & reset signals
         .rst_b_i(rst_i),

         .clk_e_i(clk_e),       // external-domain clock & reset
         .rst_e_o(rst_e),

         .clk_x_i(clk_x),       // correlator-/sample- domain clock & reset
         .rst_x_o(rst_x),

         //  CDC for the data-capture signals:
         .aq_capture_b_i(en_capture), // enable raw-data capture?
         .aq_capture_e_o(capture_e),
         .aq_capture_x_o(capture_x),

         .aq_delay_b_i(aq_delay), // set the phase-delay for the input signals:
         .aq_delay_x_o(delay_x),

         //  CDC for the phase-alignment signals:
         .aq_centre_b_i(en_centre), // enable the centering unit?
         .aq_centre_x_o(centre_x),

         .aq_select_b_i(aq_select), // select an antenna/source to centre
         .aq_select_x_o(select_x),

         .aq_locked_b_o(aq_locked), // asserted once signal is locked
         .aq_locked_x_i(locked_x),

         .aq_strobe_b_o(aq_strobe), // strobes for each new sample
         .aq_strobe_x_i(strobe_x),

         .aq_phase_b_o(aq_phase), // measured phase-delay
         .aq_phase_x_i(phase_x),

         .aq_invalid_b_o(aq_invalid), // signal lost?
         .aq_invalid_x_i(invalid_x),

         .aq_restart_b_i(aq_restart), // restart if lost tracking
         .aq_restart_x_o(restart_x),
         
         //  CDC for the fake-data unit signals:
         .aq_debug_b_i(en_debug), // enable fake-data unit?
         .aq_debug_e_o(debug_e),

         .aq_shift_b_i(aq_shift), // use a shift-register for fake data?
         .aq_shift_e_o(shift_e),

         .aq_count_b_i(aq_count), // use an up-counter for fake data?
         .aq_count_e_o(count_e),

         .aq_fake_b_o (fake_s), // fake/debug data
         .aq_fake_x_o (fake_x),
         .aq_fake_e_i (fake_i)
         );



   //-------------------------------------------------------------------------
   //
   //  ACQUISITION BLOCK
   //
   //-------------------------------------------------------------------------
`ifdef __USE_ACQUISITION
   always @(posedge clk_x)
     if (rst_x && RESET)
       bb_wr_ad_x <= #DEBUG 9'h0;
     else if (strobe_x)
       bb_wr_ad_x <= #DELAY bb_wr_ad_x_next[8:0];
     else
       bb_wr_ad_x <= #DELAY bb_wr_ad_x;

       
   //-------------------------------------------------------------------------
   //  FIFO for temporary buffering.
   //-------------------------------------------------------------------------
   block_buffer AQ_BB
     ( // read port:
       .read_clk_i     (clk_i),
       .read_address_i (aq_bb_rd_address),
       .read_data_o    (aq_read_data),
       // write port:
       .write_clk_i    (clk_x),
       .write_enable_i (write_x),
//        .write_address_i(aq_bb_wr_address),
       .write_address_i(bb_wr_ad_x),
       .write_data_i   (signal_x)
       );


   //-------------------------------------------------------------------------
   //      Storage block controller.
   //-------------------------------------------------------------------------
   //  NOTE: Bus-clock domain.
   fifo_sdram_fifo_scheduler
     #(.SDRAM_ADDRESS_WIDTH(ABITS))
   SCHEDULER0
     ( .clk  (clk_d),           // phase-shifted clock
       .clk6x(clk_i),           // bus-clock
       .rst  (rst_i),

//        .aq_bb_wr_address(aq_bb_wr_address),
       .aq_bb_rd_address(aq_bb_rd_address),
       .aq_read_data    (aq_read_data),

       .spi_start_aq(en_capture),
       .spi_buffer_read_complete(rd_req_i),

       .cmd_enable (mcb_ce_o),
       .cmd_wr     (mcb_wr_o),
       .cmd_ready  (mcb_rdy_i),
       .cmd_address(mcb_adr_o),
       .cmd_data_in(mcb_dat_o),

       .tart_state (tart_state)
       );


`else // !`ifdef __USE_ACQUISITION
   //  Only other use is to drive a test-pin.
   assign clk_d     = 1'b0;

   //  Drive zeroes onto the unused pins:
   assign mcb_ce_o  = 1'b0;
   assign mcb_wr_o  = 1'b0;
   assign mcb_adr_o = {(ABITS-1){1'b0}};
   assign mcb_dat_o = {32{1'b0}};

`endif // !`ifdef __USE_ACQUISITION


endmodule // tart_capture
