`timescale 1ns/100ps
/*
 * Module      : verilog/capture/tart_capture.v
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
 * 
 * REGISTERS:
 *  Reg#   7         6        5       4        3     2      1       0
 *      --------------------------------------------------------------------
 *   00 ||  CENTRE | DRIFT  |     2'b00      |           DELAY            ||
 *      --------------------------------------------------------------------
 *   01 || INVALID | LOCKED |     2'b00      |           PHASE            ||
 *      --------------------------------------------------------------------
 *   10 ||  DEBUG  | COUNT  | SHIFT |              #ANTENNA               ||
 *      --------------------------------------------------------------------
 *   11 || CAPTURE |      2'b00     |               SELECT                ||
 *      --------------------------------------------------------------------
 * 
 * By default, the capture unit has address 7'b000_00xx.
 * 
 * NOTE:
 *  + the `e` suffix is used to tag signals from the external (signal) clock-
 *    domain, and the `x` suffix for the (12x by default) sampling/correlator
 *    clock-domain;
 *  + even though the sample-clock's frequency is an integer multiple of the
 *    external clock, the phase relationship is unknown (due to the quirky
 *    Spartan 6 DCM's), thus asynchronous domain-crossing techniques must be
 *    used;
 *  + the 'ALIGN' parameter selects whether to oversample the antenna
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


//----------------------------------------------------------------------------
//  Register addresses.
//----------------------------------------------------------------------------
`define CAP_CENTRE 2'b00
`define CAP_STATUS 2'b01
`define CAP_DEBUG  2'b10
`define CAP_SYSTEM 2'b11


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
    parameter ALIGN = 1,        // (re-)align captured data?
    parameter DRIFT = 1,        // incrementally change the phase (0/1)?
    parameter CYCLE = 1,        // auto-strobe when not centring
    parameter RATIO = 12,       // oversampling ratio?
    parameter RMAX  = RATIO-1,  // maximum clock-counter value
    parameter RBITS = 4,        // bit-width of clock-counter
    parameter RSB   = RBITS-1,  // MSB of clock-counter

    //  Wisbone mode/settings:
    parameter RESET = 1,        // enable fast-resets (0/1)?
    parameter CHECK = 1,        // bus-signal sanity-checking (0/1)?
    parameter PIPED = 1,        // WB pipelined transfers (0/1)?

    //  Simulation-only parameters:
    parameter NOISY = 0,        // display extra debug info?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input          clock_e, // external clock
    input          clock_x, // capture clock
    output         reset_x, // capture-domain reset signal
    input          clock_i, // bus clock
    input          reset_i, // bus-domain reset

    //  Wishbone (SPEC B4) bus:
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output         ack_o,
    output         wat_o,
    output         rty_o,
    output         err_o,
    input [1:0]    adr_i, // four WB-mapped registers
    input [7:0]    dat_i, // byte-wide R/W register access
    output [7:0]   dat_o,
    
    //  Raw signal data input:
    input [MSB:0]  signal_e_i, // NOTE: external (xtal) clock domain

    //  Supersampled, aligned antenna signals, for the correlators:
    output         enable_x_o, // asserted when data is valid
    output         strobe_x_o, // strobes when new data is available
    output [MSB:0] signal_x_o, // present the captured data

    //  Debug/info outputs:
    output         enabled_o, // data-capture enabled?
    output         centred_o, // clock-recovery has succeded?
    output         debug_o    // debug (fake-data) mode?
    );


   //-------------------------------------------------------------------------
   //
   //  ANTENNA-DATA ROUTING SIGNALS.
   //
   //-------------------------------------------------------------------------
`ifdef __FORCE_SIGNAL_IOBS
   //  TODO: Make sure that the first set of registers are placed within the
   //    IOB's.
   (* IOB = "TRUE", NOMERGE = "TRUE" *)
   reg [MSB:0]     sig_x_iob;

`else
   //  Just prevent these registers from being converted to Xilinx shift-
   //  register primitives.
   (* NOMERGE = "TRUE" *)
   reg [MSB:0]     sig_x_iob;
`endif // !`ifdef __FORCE_SIGNAL_IOBS

   //-------------------------------------------------------------------------
   //  An additional layer of registers, used as synchronisers.
   (* NOMERGE = "TRUE" *)
   reg [MSB:0]     sig_x_fd0, sig_x_fd1;

   //-------------------------------------------------------------------------
   //  Fake-data signal registers.
   (* NOMERGE = "TRUE" *)
   reg [MSB:0]     sig_x_dbg0, sig_x_dbg1;



   //-------------------------------------------------------------------------
   //
   //  CROSS-DOMAIN, CAPTURE CONTROL-SIGNALS.
   //
   //-------------------------------------------------------------------------
   wire            reset_e;

   //-------------------------------------------------------------------------
   //  Phase-measurement unit signals:
   (* NOMERGE = "TRUE" *)
   reg             valid_x = 1'b0;
   wire            capture_x, centre_x, drift_x;
   wire            strobe_x, locked_x, invalid_x, restart_x;
   wire [MSB:0]    signal_w;
   wire [SSB:0]    select_x;

   //  NOTE: 'NOMERGE' constraint prevents these signals from being pulled
   //    into SRL primitives.
   //  TODO: Check.
   (* NOMERGE = "TRUE" *)
   reg [MSB:0]     source_x, signal_x;
   wire [RSB:0]    phase_x, delay_x;


   //-------------------------------------------------------------------------
   //  Fake/debug antenna-data signals.
   //-------------------------------------------------------------------------
   wire            debug_e, shift_e, count_e, valid_e;
   wire            debug_x;
   wire [MSB:0]    fake_e;



   //-------------------------------------------------------------------------
   //
   //  WISHBONE BUS-MAPPED REGISTERS.
   //
   //-------------------------------------------------------------------------
   wire [7:0]      tc_system, tc_centre, tc_debug, tc_status; // WB regs
   wire [4:0]      tc_number = AXNUM;
   reg             en_capture = 1'b0, en_centre = 1'b0, en_debug = 1'b0;
   reg [SSB:0]     tc_select = {SBITS{1'b0}}; // source/antenna to centre
   wire            tc_invalid, tc_locked;
   wire [RSB:0]    tc_phase;
   reg [RSB:0]     tc_delay = {RBITS{1'b0}};
   reg             tc_drift, tc_count, tc_shift, tc_restart = 1'b0;

   //-------------------------------------------------------------------------
   //  Internal signals used for the Wishbone interface:
   reg [7:0]       dat;
   reg             ack = 1'b0;
   wire            cyc_w, stb_w, fetch, store;



   //-------------------------------------------------------------------------
   //
   //  WISHBONE-RELATED SIGNAL ASSIGNMENTS.
   //
   //-------------------------------------------------------------------------
   //  Wishbone-mapped register assignments:
   assign tc_centre = {en_centre, tc_drift, 2'b0, tc_delay[3:0]};
   assign tc_status = {tc_invalid, tc_locked, 2'b0, tc_phase[3:0]};
   assign tc_debug  = {en_debug, tc_count, tc_shift, tc_number};
   assign tc_system = {en_capture, 2'b00, tc_select};

   //-------------------------------------------------------------------------
   //  Internal, Wishbone, combinational signals:
   assign cyc_w = CHECK ? cyc_i : 1'b1;
   assign stb_w = cyc_w && stb_i;

   assign fetch = PIPED ? stb_w && !we_i : stb_w && !we_i &&  ack;
   assign store = PIPED ? stb_w &&  we_i : stb_w &&  we_i && !ack;

   //  Wishbone output assignments:
   assign ack_o = ack;
   assign wat_o = 1'b0;
   assign rty_o = 1'b0;
   assign err_o = 1'b0;
   assign dat_o = dat;


   //-------------------------------------------------------------------------
   //  Correlator-domain signal assignments.
   //-------------------------------------------------------------------------
   assign enable_x_o = valid_x;
   assign strobe_x_o = strobe_x;
   assign signal_x_o = signal_x;


   //-------------------------------------------------------------------------
   //  Debug/info signal assignments.
   //-------------------------------------------------------------------------
   assign enabled_o = en_capture;
   assign centred_o = tc_locked;
   assign debug_o   = en_debug;



   //-------------------------------------------------------------------------
   //
   //  WISHBONE (SPEC B4) INTERFACE.
   //
   //-------------------------------------------------------------------------
   //  Acknowledge all requests.
   always @(posedge clock_i)
     if (reset_i && RESET)
       ack <= #DELAY 1'b0;
     else
       ack <= #DELAY stb_w;

   //-------------------------------------------------------------------------
   //  Capture-register read-backs.
   always @(posedge clock_i)
     if (stb_w)
       case (adr_i)
         `CAP_CENTRE: dat <= #DELAY tc_centre;
         `CAP_STATUS: dat <= #DELAY tc_status;
         `CAP_DEBUG:  dat <= #DELAY tc_debug;
         `CAP_SYSTEM: dat <= #DELAY tc_system;
         default:     dat <= #DELAY 8'bx;
       endcase // case (adr_i)


   //-------------------------------------------------------------------------
   //  Enable/restart the signal-capture, centring unit, and set the antenna/
   //  source phase-delay.
   //-------------------------------------------------------------------------
   //  TODO: AUTO-mode, which continually monitors (and adjusts) the phase?
   always @(posedge clock_i)
     if (reset_i)
       en_centre <= #DELAY 1'b0;
     else if (store && adr_i == `CAP_CENTRE)
       en_centre <= #DELAY dat_i[7];
     else
       en_centre <= #DELAY en_centre;

   always @(posedge clock_i)
     if (reset_i && RESET)
       {tc_drift, tc_delay} <= #DELAY {1'b0, {RBITS{1'b0}}};
     else if (store && adr_i == `CAP_CENTRE)
       {tc_drift, tc_delay} <= #DELAY {dat_i[6], dat_i[RSB:0]};
     else
       {tc_drift, tc_delay} <= #DELAY {tc_drift, tc_delay};

   //  Pulse the restart if enable of an active unit is requested.
   always @(posedge clock_i)
     if (reset_i && RESET)
       tc_restart <= #DELAY 1'b0;
     else if (store && adr_i == `CAP_CENTRE && en_centre)
       tc_restart <= #DELAY 1'b1;
     else
       tc_restart <= #DELAY 1'b0;

   //-------------------------------------------------------------------------
   //  Control the debug unit, and its modes/settings.
   always @(posedge clock_i)
     if (reset_i)
       {en_debug, tc_count, tc_shift} <= #DELAY 3'h0;
     else if (store && adr_i == `CAP_DEBUG)
       {en_debug, tc_count, tc_shift} <= #DELAY dat_i[7:5];
     else
       {en_debug, tc_count, tc_shift} <= #DELAY {en_debug, tc_count, tc_shift};

   //-------------------------------------------------------------------------
   //  Control the raw-data capture unit.
   always @(posedge clock_i)
     if (reset_i)
       en_capture <= #DELAY 1'b0;
     else if (store && adr_i == `CAP_SYSTEM)
       en_capture <= #DELAY dat_i[7];
     else
       en_capture <= #DELAY en_capture;

   always @(posedge clock_i)
     if (reset_i && RESET)
       tc_select <= #DELAY {SBITS{1'b0}};
     else if (store && adr_i == `CAP_SYSTEM)
       tc_select <= #DELAY dat_i[SSB:0];
     else
       tc_select <= #DELAY tc_select;



   //-------------------------------------------------------------------------
   //
   //  DATA CAPTURE.
   //
   //-------------------------------------------------------------------------
   //  Synchronisers to capture (and supersample) the input (and debug/
   //  generated) signals.
   //  NOTE: Both IOB registers are used to reduce the synchroniser path-
   //    length, therefore the probability of metastability.
   //  TODO: Setup the FROM/TO constraints.
   //  TODO: Check post PAR, to see the actual placement & routing.
   always @(posedge clock_x)
     begin
        // capture the antennae signal:
        sig_x_iob <= #DELAY signal_e_i;

        // synchronise the antennae signal:
        {sig_x_fd1, sig_x_fd0} <= #DELAY {sig_x_fd0, sig_x_iob};

        // synchronise the fake/debug signal:
        {sig_x_dbg1, sig_x_dbg0} <= #DELAY {sig_x_dbg0, fake_e};
     end


   //-------------------------------------------------------------------------
   //  Sample-domain data-flow.
   //-------------------------------------------------------------------------
   //  NOTE: The phase-shifter ('shift_reg') outputs are registered because
   //    the 'SRL16E' primitive has about 1ns of combinational delay, and this
   //    signal then feeds into a block SRAM, which has another ~1ns delay.
   //    Therefore, to more easily satisfy the timing-constraints (~200 MHz),
   //    additional pipelining is used.
   always @(posedge clock_x)
     begin
        // select the signal source (fake or real), to be fed into the phase-
        // shifter:
        source_x <= #DELAY debug_x ? sig_x_dbg1 : sig_x_fd1;

        // register the phase-shifter output, and this then feeds into the
        // block SRAM for the raw-data acquisition core:
        if (strobe_x) begin
           signal_x <= #DELAY signal_w;
           valid_x  <= #DELAY locked_x;
        end
     end


`ifndef __RELEASE_BUILD   
   //-------------------------------------------------------------------------
   //  Fake data generation circuit, for testing & debugging.
   //-------------------------------------------------------------------------
   (* AREA_GROUP = "fake" *)
   fake_telescope
     #(  .WIDTH(AXNUM),
         .MULTI(MULTI),
         .RNG  (RNG),
         .CONST(CONST),
         .CDATA(CDATA),
         .DELAY(DELAY)
         ) FAKE
       ( .clock_i (clock_e),
         .reset_i (reset_e),
         .enable_i(debug_e),
         .shift_i (shift_e),
         .count_i (count_e),
         .valid_o (valid_e),
         .data_o  (fake_e)
         );
`endif


   //-------------------------------------------------------------------------
   //  Measure the phase-shift required so that latching occurs at the centre
   //  of each signal sample.
   //-------------------------------------------------------------------------
   //  NOTE: Computes the required phase-shift, to determine the relative
   //    clock-phases, so that data can be captured mid-period (of the slower
   //    clock).
   (* AREA_GROUP = "centre" *)
   signal_centre
     #( .WIDTH(AXNUM),
        .SBITS(SBITS),
        .RATIO(RATIO),
        .RBITS(RBITS),
        .RESET(RESET),
        .DRIFT(DRIFT),
        .CYCLE(CYCLE),
        .IOB  (0),              // IOB's already allocated for synchros
        .NOISY(NOISY),
        .DELAY(DELAY)
        ) CENTRE
     (  .clock_i  (clock_x),    // 12x oversampling clock
        .reset_i  (reset_x),
        .align_i  (centre_x),   // compute the alignment shift?
        .cyclic_i (1'b1),       // cycle continually (if 'CYCLE')
        .drift_i  (drift_x),    // incrementally change the phase?
        .signal_i (source_x),   // from external/fake data MUX
        .select_i (select_x),   // select antenna to measure phase of
        .strobe_o (strobe_x),   // mark the arrival of a new value
        .locked_o (locked_x),   // signal is stable & locked
        .phase_o  (phase_x),    // relative phase for stable signal
        .invalid_o(invalid_x),  // signal can't lock, or lock lost
        .restart_i(restart_x)   // acknowledge and retry
        );


   //-------------------------------------------------------------------------
   //  Programmable delay that is used to phase-shift the incoming signal.
   //-------------------------------------------------------------------------
   (* AREA_GROUP = "shreg" *)
   shift_reg
     #(  .DEPTH(16),            // should synthesise to SRL16E primitives
         .ABITS(4),
         .DELAY(DELAY)
         ) SHREG [MSB:0]
       ( .clk(clock_x),
         .ce (capture_x),       // TODO: use `locked_x`?
         .a  (delay_x),
         .d  (source_x),
         .q  (signal_w)
         );



   //-------------------------------------------------------------------------
   //
   //  CLOCK DOMAIN CROSSING (CDC) UNIT FOR THE CONTROL-SIGNALS.
   //
   //-------------------------------------------------------------------------
   //  Performs (nearly) all CDC within one module, to make it easier to spot
   //  violations of CDC rules.
   (* AREA_GROUP = "ctrl" *)
   capture_control
     #( .PBITS(RBITS),          // phase-delay bit-width
        .SBITS(SBITS),          // antenna-select bit-with
        .DELAY(DELAY)           // simulation combinational delay (ns)
        ) CTRL
       ( .clock_b_i(clock_i),       // bus-domain clock & reset signals
         .reset_b_i(reset_i),

         .clock_e_i(clock_e),       // external-domain clock & reset
         .reset_e_o(reset_e),

         .clock_x_i(clock_x),       // correlator-/sample- domain clock & reset
         .reset_x_o(reset_x),

         //  CDC for the data-capture signals:
         .capture_b_i(en_capture), // enable raw-data capture?
         .capture_x_o(capture_x),

         .delay_b_i(tc_delay),  // phase-delay for the input signals
         .delay_x_o(delay_x),

         //  CDC for the phase-alignment signals:
         .centre_b_i(en_centre), // enable the centring unit?
         .centre_x_o(centre_x),

         .drift_b_i(tc_drift), // incrementally change the phase?
         .drift_x_o(drift_x),

         .select_b_i(tc_select), // select an antenna/source to centre
         .select_x_o(select_x),

         .locked_b_o(tc_locked), // asserted once signal is locked
         .locked_x_i(locked_x),

         .phase_b_o(tc_phase), // measured phase-delay
         .phase_x_i(phase_x),

         .invalid_b_o(tc_invalid), // signal lost?
         .invalid_x_i(invalid_x),

         .restart_b_i(tc_restart), // restart if lost tracking
         .restart_x_o(restart_x),
         
         //  CDC for the fake-data unit signals:
         .debug_b_i(en_debug), // enable fake-data unit?
         .debug_e_o(debug_e),
         .debug_x_o(debug_x),

         .shift_b_i(tc_shift), // use a shift-register for fake data?
         .shift_e_o(shift_e),

         .count_b_i(tc_count), // use an up-counter for fake data?
         .count_e_o(count_e)
         );



endmodule // tart_capture
