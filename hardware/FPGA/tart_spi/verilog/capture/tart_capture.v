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
 *  Reg#   7        6       5        4       3       2       1       0
 *      --------------------------------------------------------------------
 *   00 || CENTRE | DRIFT | INVERT | 1'b0  |             DELAY            ||
 *      --------------------------------------------------------------------
 *   01 ||               DELTA             |             PHASE            ||
 *      --------------------------------------------------------------------
 *   10 || DEBUG  | COUNT | SHIFT  |               #ANTENNA               ||
 *      --------------------------------------------------------------------
 *   11 || ENABLE | ERROR | LOCKED |                SELECT                ||
 *      --------------------------------------------------------------------
 * 
 * By default, the capture unit has address 7'b000_00xx.
 * 
 * NOTE:
 *  + the `e` suffix is used to tag signals from the external (signal) clock-
 *    domain, and the `x` suffix for the (12x by default) sampling/correlator
 *    clock-domain;
 *  + the 'strobe_x_o' signal asserts the *cycle before* the sample/data is
 *    valid, as this can be used to coordinate flow-control logic;
 *  + the 'ALIGN' parameter selects whether to oversample the antenna
 *    signals, and perform clock-recovery, if enabled (1) -- or instead to
 *    cross the clock-domain (to the correlator domain) using an asynchronous
 *    FIFO (0);
 *  + DDR registers (via the 'IDDR2' primitives) are used to capture the
 *    signal inputs, and by using the 6x clock, the sources are effectively
 *    oversampled at 12x the sources' clock-rate;
 * 
 * TODO:
 *  + even though the sample-clock's frequency is an integer multiple of the
 *    external clock, the phase relationship is unknown (due to the quirky
 *    Spartan 6 DCM's), thus asynchronous domain-crossing techniques must be
 *    used?
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
    parameter TICKS = 3,        // additional pipeline delays

    //  Wisbone mode/settings:
    parameter RESET = 1,        // enable fast-resets (0/1)?
    parameter CHECK = 1,        // bus-signal sanity-checking (0/1)?
    parameter PIPED = 1,        // WB pipelined transfers (0/1)?

    //  Simulation-only parameters:
    parameter NOISY = 0,        // display extra debug info?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input          clock_e, // post-DCM external clock
    input          clock_x, // capture clock
    output         reset_x, // capture-domain reset signal
    input          clock_n, // negated bus clock
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
    output         strobe_o,  // bus-domain strobe
    output         middle_o,  // bus-domain mid-sample strobe
    output         centred_o, // clock-recovery has succeded?
    output         debug_o,   // debug (fake-data) mode?
    output [MSB:0] signal_po, // posedge of the raw DDR-sampled signal,
    output [MSB:0] signal_no  // and negedge too
    );



   //-------------------------------------------------------------------------
   //  Cross-domain, capture control-signals.
   //-------------------------------------------------------------------------
   wire            reset_e;
   wire [MSB:0]    signal_p, signal_n;

   //-------------------------------------------------------------------------
   //  Phase-measurement unit signals:
   (* NOMERGE = "TRUE" *)
   reg             enable_x = 1'b0, locked_x = 1'b0;
   wire            capture_x, centre_x, drift_x;
   wire            strobe_x, middle_x, invalid_x, restart_x;
   wire [MSB:0]    signal_w;
   wire [SSB:0]    select_x;

   //-------------------------------------------------------------------------
   //  NOTE: 'NOMERGE' constraint prevents these signals from being pulled
   //    into SRL primitives.
   //  TODO: Check.
   (* NOMERGE = "TRUE" *)
   reg [MSB:0]     source_x, signal_x;
   wire [RSB:0]    phase_x, delay_x;
   wire [MSB:0]    sig_x_ddr;

   //-------------------------------------------------------------------------
   //  Data-alignment signals.
   wire            source_p, source_n;
   wire            ref_p, ref_n;
   wire            fake_p, fake_n;


   //-------------------------------------------------------------------------
   //  Fake/debug antenna-data signals.
   //-------------------------------------------------------------------------
   wire            debug_e, shift_e, count_e;
   wire            debug_x;
   wire [MSB:0]    fake_e;
   wire [MSB:0]    sig_x_dbg;
   (* NOMERGE = "TRUE" *)
   reg [MSB:0]     fake_b_p, fake_b_n, fake_n_n;



   //-------------------------------------------------------------------------
   //
   //  WISHBONE BUS-MAPPED REGISTERS.
   //
   //-------------------------------------------------------------------------
   wire [7:0]      tc_system, tc_centre, tc_debug, tc_status; // WB regs
   wire [4:0]      tc_number = AXNUM;
   reg             en_capture = 1'b0, en_centre = 1'b0, en_debug = 1'b0;
   reg [SSB:0]     tc_select = {SBITS{1'b0}}; // source/antenna to centre
   wire            tc_invalid, tc_locked, tc_strobe, tc_middle;
   wire [RSB:0]    tc_phase, tc_delta;
   reg [RSB:0]     tc_delay = {RBITS{1'b0}};
   reg             tc_invert = 1'b0, tc_restart = 1'b0;
   reg             tc_drift, tc_count, tc_shift;

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
   assign tc_centre = {en_centre, tc_drift, tc_invert, 1'b0, tc_delay[3:0]};
   assign tc_status = {tc_delta[3:0], tc_phase[3:0]};
   assign tc_debug  = {en_debug, tc_count, tc_shift, tc_number};
   assign tc_system = {en_capture, tc_invalid, tc_locked, tc_select};

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
   assign enable_x_o = enable_x;
   assign strobe_x_o = strobe_x;
   assign signal_x_o = signal_x;


   //-------------------------------------------------------------------------
   //  Debug/info signal assignments.
   //-------------------------------------------------------------------------
   assign enabled_o = en_capture;
   assign strobe_o  = tc_strobe;
//    assign strobe_o  = strobe_b; // TODO:
   assign middle_o  = tc_middle;
   assign centred_o = tc_locked;
   assign debug_o   = en_debug;
   assign signal_po = signal_p;
   assign signal_no = signal_n;

   //-------------------------------------------------------------------------
   //  Use a single bit of the fake signal to feed into the phase-measurement
   //  unit.
   assign fake_p    = fake_b_p[0];
   assign fake_n    = fake_b_n[0];



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
       {tc_drift, tc_invert, tc_delay} <= #DELAY {RBITS+2{1'b0}};
     else if (store && adr_i == `CAP_CENTRE)
       {tc_drift, tc_invert, tc_delay} <= #DELAY {dat_i[6:5], dat_i[RSB:0]};
     else
       {tc_drift, tc_invert, tc_delay} <= #DELAY {tc_drift, tc_invert, tc_delay};

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


   /*
   //  TODO: Make sure that the first sample is valid.
   //-------------------------------------------------------------------------
   //  Bus-domain strobe signal needs to be delayed by one strobe, to ensure
   //  that the first sample is actual data.
   //-------------------------------------------------------------------------
   reg             b_strobe = 1'b0;

   always @(posedge clock_i)
     if (reset_i)
       b_strobe <= #DELAY 1'b0;
     else if (en_capture)
       b_strobe <= #DELAY tc_strobe;
    */



   //-------------------------------------------------------------------------
   //
   //  DATA CAPTURE.
   //
   //-------------------------------------------------------------------------
   //  DDR signal-capture for the fake signal.
   //-------------------------------------------------------------------------
   //  Bring both components into the same clock-domain (in a way that mimicks
   //  the 'IDDR2' primitive with 'DDR_ALIGNMENT = "C0"').
   always @(posedge clock_i) begin
     fake_b_p <= #DELAY fake_e;
     fake_b_n <= #DELAY fake_n_n;
   end

   //  Capture the "negedge" component using the DCM's 180 degree phase-
   //  shifted clock output.
   always @(posedge clock_n)
     fake_n_n <= #DELAY fake_e;



   //-------------------------------------------------------------------------
   //
   //  (12X) OVERSAMPLING-DOMAIN DATA-CAPTURE.
   //
   //-------------------------------------------------------------------------
   //  Construct a new signal, using the DDR signal, for use in the 12x clock-
   //  domain (i.e., twice the frequency), by using the LSB of the delay to
   //  select the positive- or negative- edge signal.
   assign sig_x_ddr = delay_x[0] ? signal_n : signal_p;
   assign sig_x_dbg = delay_x[0] ? fake_b_n : fake_b_p;


   //-------------------------------------------------------------------------
   //  Source-select MUX (fake or external), to be fed into the phase-
   //  shifter.
   //  NOTE: The two domains are synchronous, so synchronisers don't need to
   //    be used. (Though they are used elsewhere...)
   //  TODO: Check the timing report.
   always @(posedge clock_x)
     source_x <= #DELAY debug_x ? sig_x_dbg : sig_x_ddr;

   //-------------------------------------------------------------------------
   //  Register the phase-shifter output, and this then feeds into the block
   //  SRAM for the raw-data acquisition core.
   //  NOTE: The phase-shifter ('shift_reg') outputs are registered because
   //    the 'SRL16E' primitive has about 1ns of combinational delay, and this
   //    signal then feeds into a block SRAM, which has another ~1ns delay.
   //    Therefore, to more easily satisfy the timing-constraints (~200 MHz),
   //    additional pipelining is used.
   always @(posedge clock_x)
     signal_x <= #DELAY strobe_x ? signal_w : signal_x;


   //-------------------------------------------------------------------------
   //  Generate the enable/valid signal, and make sure that it has the correct
   //  phase relationship with the 'strobe_x' signal.
   //-------------------------------------------------------------------------
   always @(posedge clock_x)
     if (reset_x && RESET || !capture_x)
       enable_x <= #DELAY 1'b0;
     else if (capture_x && strobe_x)
       enable_x <= #DELAY 1'b1;
     else
       enable_x <= #DELAY enable_x;

   //-------------------------------------------------------------------------
   //  Consider the signal locked after the first framing signal assertion,
   //  once enabled.
   //  TODO: Replace with something better?
   always @(posedge clock_x)
     if (reset_x && RESET || !capture_x)
       locked_x <= #DELAY 1'b0;
     else if (strobe_x)
       locked_x <= #DELAY 1'b1;



`ifdef __RELEASE_BUILD
   assign fake_e = {AXNUM{1'b0}};

`else
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
       ( .clock_i (clock_e),    // XTAL-domain clock
         .reset_i (reset_e),
         .enable_i(debug_e),    // enable by using DEBUG-mode
         .shift_i (shift_e),    // gen. data using shift-register
         .count_i (count_e),    // gen. data using up-counter?
         .valid_o (),
         .data_o  (fake_e)
         );
`endif


   //-------------------------------------------------------------------------
   //  Select a source to measure the phase of.
   //-------------------------------------------------------------------------
   (* AREA_GROUP = "source" *)
   signal_source
     #( .WIDTH(AXNUM+2),
        .SBITS(SBITS),
        .RESET(0),
        .DELAY(DELAY)
        ) SOURCE
       (
        .clock_i (clock_i),
        .reset_i (reset_i),
        .enable_i(en_centre),
        .select_i(tc_select),
        .sig_p_i ({fake_p, ref_p, signal_p}),
        .sig_n_i ({fake_n, ref_n, signal_n}),
        .valid_o (tc_locked),   // TODO: not a useful signal to monitor?
        .sig_p_o (source_p),
        .sig_n_o (source_n)
        );


   //-------------------------------------------------------------------------
   //  Measure the phase-shift required so that latching occurs at the centre
   //  of each signal sample.
   //-------------------------------------------------------------------------
   //  NOTE: Computes the required phase-shift, to determine the relative
   //    clock-phases, so that data can be captured mid-period (of the slower
   //    clock).
   (* AREA_GROUP = "phase" *)
   signal_phase_DDR
     #( .RATIO(RATIO >> 1),
        .RBITS(RBITS - 1),
        .RESET(1),
        .TICKS(TICKS),
        .POLAR(1),
        .NOISY(NOISY),
        .DELAY(DELAY)
        ) PHASE
       (
        .clk_e_i (clock_e),     // external (XTAL) reference & clock
        .clk_n_i (clock_n),     // 6x (negedge) oversampling clock
        .clk_s_i (clock_i),     // 6x (posedge) oversampling clock
        .reset_i (reset_i),     // 6x clock-domain reset

        .enable_i(tc_locked),   // compute the alignment shift?
        .invert_i(tc_invert),   // TODO: use negative edge to compute phase?
        .strobe_o(tc_strobe),   // mark the arrival of a new value
        .middle_o(tc_middle),   // mark the arrival of a new value
        .sig_p_i (source_p),    // posedge DDR signal input
        .sig_n_i (source_n),    // negedge DDR signal input

        .phase_o (tc_phase),    // relative phase for stable signal
        .delta_o (tc_delta),    // phase error vs. previous sample
        .locked_o(),            // TODO: signal is stable & locked
        .error_o (tc_invalid),  // signal can't lock, or lock lost
        .retry_i (tc_restart),  // acknowledge and retry

        //  Debug/info outputs:
        .ref_p_o (ref_p),
        .ref_n_o (ref_n)
        );


   //-------------------------------------------------------------------------
   //  Programmable delay that is used to phase-shift the incoming signal.
   //-------------------------------------------------------------------------
   (* AREA_GROUP = "shreg" *)
   shift_reg
     #(  .DEPTH(8),          // should synthesise to SRL16E primitives?
         .ABITS(3),
         .DELAY(DELAY)
         ) SHREG [MSB:0]
       ( .clk(clock_x),
         .ce (capture_x),       // TODO: use `locked_x`?
         .a  (delay_x[3:1]),
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
   //  TODO: This is overkill, since the 'tart_dcm' module is configured to
   //    operate in a synchronous mode?
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

`ifdef __USE_SIGNAL_CENTRE
         // TODO: OBSOLETE
         //  CDC for the phase-alignment signals:
         .centre_b_i(en_centre), // enable the centring unit?
         .centre_x_o(centre_x),

         .drift_b_i(tc_drift), // incrementally change the phase?
         .drift_x_o(drift_x),

         .select_b_i(tc_select), // select an antenna/source to centre
         .select_x_o(select_x),
         .select_e_o(select_e),

         .locked_b_o(tc_locked), // asserted once signal is locked
         .locked_x_i(locked_x),

         .phase_b_o(tc_phase), // measured phase-delay
         .phase_x_i(phase_x),

         .invalid_b_o(tc_invalid), // signal lost?
         .invalid_x_i(invalid_x),

         .restart_b_i(tc_restart), // restart if lost tracking
         .restart_x_o(restart_x),

`else // !`ifdef __USE_SIGNAL_CENTRE
         .strobe_b_i (tc_strobe && en_capture),
         .strobe_x_o (strobe_x),

         .middle_b_i (tc_middle),
         .middle_x_o (middle_x),

         //  Not needed, as relevant operations performed in bus domain.
         .centre_b_i (1'b0),
         .drift_b_i  (1'b0),
         .select_b_i ({SBITS{1'b0}}),
         .locked_x_i (1'b0),
         .phase_x_i  ({RBITS{1'b0}}),
         .invalid_x_i(1'b0),
         .restart_b_i(1'b0),

`endif // !`ifdef __USE_SIGNAL_CENTRE
         //  CDC for the fake-data unit signals:
         .debug_b_i(en_debug), // enable fake-data unit?
         .debug_e_o(debug_e),
         .debug_x_o(debug_x),

         .shift_b_i(tc_shift), // use a shift-register for fake data?
         .shift_e_o(shift_e),

         .count_b_i(tc_count), // use an up-counter for fake data?
         .count_e_o(count_e)
         );



   //-------------------------------------------------------------------------
   //
   //  DDR IOB REGISTERS.
   //
   //-------------------------------------------------------------------------
   //  NOTE: This allow half-rate sampling, but at the expense of twice the
   //    usage of routing-resources.
   IDDR2
     #( .DDR_ALIGNMENT("C0"),
        .SRTYPE("SYNC")
        ) IOBS [MSB:0]
     ( .C0(clock_i),
       .C1(clock_n),
       .R (reset_i && RESET),
       .CE(en_capture),
       .D (signal_e_i),
       .Q0(signal_p),
       .Q1(signal_n)            // lags by 180 degrees
       );



endmodule // tart_capture
