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
 * The raw antenna signal is captured (12x oversampling), and aligned, using
 * either a predetermined phase-shift, or the value determined by the phase-
 * detection circuit.
 * 
 * The external antennae typically operate at 16.368 MHz, driven by an off-
 * chip crystal oscillator, and the resulting raw-data signals need to be
 * captured (and, optionally aligned using a clock-recovery circuit).
 * 
 * Alternatively, for testing & configuring TART fake-data can also be
 * generated, and this fake-data can be of several types.
 * 
 * The schematic of this module, for the antenna signals:
 * 
 *   \|/                                                                     
 *    |                                                                      
 *    |        +---------+                                                      
 *    |        |  Fake   |          (6x Clock Domain, 98.208 MHz)               
 *    | WB o=> | Antenna |--+                                                  
 *    |        |         |  |                                                  
 *    |        +---------+  |            +-----------+                      
 *    |                     o--> |\      |   Phase   |    4                 
 *    | reference o------------> | |---> | Detectors |----/---o phase
 *    |                  oo====> |/      |           |                        
 *    |                  || |            +-----------+                        
 *    |                  || |                                              
 *    |     +---------+  || |            +-----------+              
 *    |     |  IOBS   |  || +-> |\       |   Shift   |    24       
 *    +---> |  (DDR)  |==oo===> | |----> | Registers |----/---o signal
 * 16.368   |         |         |/       |           |
 *  (MHz)   +---------+          |       +-----------+
 *             4                 |    3        |   
 *  delay o----/-----------------o----/--------+        
 * 
 * 
 * REGISTERS:
 *  Reg#   7        6       5        4       3       2       1       0
 *      --------------------------------------------------------------------
 *   00 || CENTRE | DRIFT | INVERT | 1'b0  |             DELAY            ||
 *      || (R/W)  | (R/W) | (R/W)  |       |             (R/W)            ||
 *      --------------------------------------------------------------------
 *   01 ||               DELTA             |             PHASE            ||
 *      ||               (RO)              |             (RO)             ||
 *      --------------------------------------------------------------------
 *   10 || DEBUG  | COUNT | SHIFT  |               #ANTENNA               ||
 *      || (R/W)  | (R/W) | (R/W)  |                 (RO)                 ||
 *      --------------------------------------------------------------------
 *   11 || ENABLE | ERROR | LOCKED |                SELECT                ||
 *      || (R/W)  | (R/W) |  (RO)  |                (R/W)                 ||
 *      --------------------------------------------------------------------
 * 
 * By default, the capture unit has address 7'b000_00xx.
 * 
 * 
 * NOTE:
 *  + the `e` suffix is used to tag signals from the external (signal) clock-
 *    domain;
 *  + DDR registers (via the 'IDDR2' primitives) are used to capture the
 *    signal inputs, and by using the 6x clock, the sources are effectively
 *    oversampled at 12x the sources' clock-rate;
 *  + the 'strobe_o' signal leads the sample by one clock-cycle -- i.e., if
 *    it is used to latch the signal data, then an additional clock-cycle of
 *    latency is required;
 * 
 * TODO:
 *  + all this signal-capture stuff can be done in the XTAL and bus clock-
 *    domains, so the correlator-domain isn't needed here?
 *  + even though the sample-clock's frequency is an integer multiple of the
 *    external clock, the phase relationship is unknown (due to the quirky
 *    Spartan 6 DCM's), thus asynchronous domain-crossing techniques must be
 *    used?
 *  + setup a bunch of FROM/TO constraints for CDC;
 *  + the handling of asynchronous signals is still a mess;
 *  + floorplan into the upper-right corner?
 *  + AUTO-mode, which continually monitors (and adjusts) the phase?
 *  + 'DRIFT' is currently ignored;
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
    parameter ALIGN = 1,        // enable the phase-delay unit?
    parameter DRIFT = 1,        // incrementally change the phase (0/1)?
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

    //  Signal and debug/info outputs:
    output         enabled_o, // data-capture enabled?
    output         strobe_o,  // bus-domain strobe
    output         middle_o,  // bus-domain mid-sample strobe
    output [MSB:0] signal_o,  // delayed, oversampled signal
    output         centred_o, // clock-recovery has succeded?
    output         debug_o    // debug (fake-data) mode?
    );



   //-------------------------------------------------------------------------
   //  Cross-domain, capture control-signals.
   //-------------------------------------------------------------------------
   reg             reset_e, reset_n;
   reg             debug_e, shift_e, count_e;


   //-------------------------------------------------------------------------
   //  Various sub-unit enable signals.
   //-------------------------------------------------------------------------
   reg             en_capture = 1'b0, en_centre = 1'b0, en_debug = 1'b0;
   wire            en_source;
   reg             enabled = 1'b0;


   //-------------------------------------------------------------------------
   //  Raw-data registers & signals.
   //-------------------------------------------------------------------------
   wire [MSB:0]    signal_p, signal_n;
   reg [MSB:0]     source, signal;
   wire [MSB:0]    signal_w, sig_ddr;

   //-------------------------------------------------------------------------
   //  Data-alignment signals.
   wire            source_p, source_n;
   wire            ref_p, ref_n;
   wire            fake_p, fake_n;


   //-------------------------------------------------------------------------
   //  Fake/debug antenna-data signals.
   //-------------------------------------------------------------------------
   wire [MSB:0]    fake_e, sig_dbg;
   (* NOMERGE = "TRUE" *)
   reg [MSB:0]     fake_b_p, fake_b_n, fake_n_n;



   //-------------------------------------------------------------------------
   //
   //  WISHBONE BUS-MAPPED REGISTERS.
   //
   //-------------------------------------------------------------------------
   wire [7:0]      tc_system, tc_centre, tc_debug, tc_status; // WB regs
   wire [4:0]      tc_number = AXNUM;
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
   //  Debug/info signal assignments.
   //-------------------------------------------------------------------------
   assign enabled_o = enabled;
   assign strobe_o  = tc_strobe;
   assign middle_o  = tc_middle;
   assign centred_o = tc_locked;
   assign debug_o   = en_debug;
   assign signal_o  = signal;

   //-------------------------------------------------------------------------
   //  Signal source selection.
   assign sig_ddr = tc_delay[0] ? signal_n : signal_p;
   assign sig_dbg = tc_delay[0] ? fake_b_n : fake_b_p;

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
   //  Allow the read-back of the capture-unit's registers.
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
       {tc_drift, tc_invert} <= #DELAY 2'b00;
     else if (store && adr_i == `CAP_CENTRE)
       {tc_drift, tc_invert} <= #DELAY dat_i[6:5];
     else
       {tc_drift, tc_invert} <= #DELAY {tc_drift, tc_invert};

   //  Sets the requested delay (if built with 'ALIGN == 1').
   always @(posedge clock_i)
     if (reset_i && RESET)
       tc_delay <= #DELAY {RBITS{1'b0}};
     else if (ALIGN && store && adr_i == `CAP_CENTRE)
       tc_delay <= #DELAY dat_i[RSB:0];
     else
       tc_delay <= #DELAY tc_delay;

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
     else if (DEBUG && store && adr_i == `CAP_DEBUG)
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
   //  Generate the enable/valid signal, and make sure that it has the correct
   //  phase relationship with the 'tc_strobe' signal.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i && RESET || !en_capture)
       enabled <= #DELAY 1'b0;
     else if (en_capture && tc_strobe)
       enabled <= #DELAY 1'b1;
     else
       enabled <= #DELAY enabled;


   //-------------------------------------------------------------------------
   //  Clock Domain Crossing (CDC) for some of the control-signals.
   //-------------------------------------------------------------------------
   //  Make sure that the reset-signal is asserted for at least one slow-
   //  clock period.
   always @(posedge clock_e or posedge reset_i)
     if (reset_i && DEBUG)
       reset_e <= #DELAY 1'b1;
     else if (reset_n || !DEBUG)
       reset_e <= #DELAY 1'b0;
     else
       reset_e <= #DELAY reset_e;

   //  The Xilinx DCM's are configured so that only synchronous techniques are
   //  required for CDC, for these signals.
   always @(posedge clock_e)
     begin
        reset_n <= #DELAY reset_e;
        debug_e <= #DELAY en_debug;
        shift_e <= #DELAY tc_shift;
        count_e <= #DELAY tc_count;
     end



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
     fake_b_p <= #DELAY DEBUG ? fake_e : {AXNUM{1'b0}};
     fake_b_n <= #DELAY fake_n_n;
   end

   //  Capture the "negedge" component using the DCM's 180 degree phase-
   //  shifted clock output.
   always @(posedge clock_n)
     fake_n_n <= #DELAY DEBUG ? fake_e : {AXNUM{1'b0}};

   //-------------------------------------------------------------------------
   //  Source-select MUX (fake or external), to be fed into the phase-
   //  shifter.
   always @(posedge clock_i)
     source <= #DELAY en_debug ? sig_dbg : sig_ddr;

   //-------------------------------------------------------------------------
   //  Output-signal 'sample-and-hold' register -- which takes the phase-
   //  shifter output as input.
   always @(posedge clock_i)
     signal <= #DELAY tc_strobe ? signal_w : signal;



   //-------------------------------------------------------------------------
   //  Fake data generation circuit, for testing & debugging.
   //-------------------------------------------------------------------------
`ifdef __RELEASE_BUILD
   assign fake_e = {AXNUM{1'b0}};
`else
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
   //  Source-select MUX for the phase-measurement unit.
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
        .valid_o (en_source),   // TODO: not a useful signal to monitor?
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
        .COUNT(7),
        .NOISY(NOISY),
        .DELAY(DELAY)
        ) PHASE
       (
        .clk_e_i (clock_e),     // external (XTAL) reference & clock
        .clk_n_i (clock_n),     // 6x (negedge) oversampling clock
        .clk_s_i (clock_i),     // 6x (posedge) oversampling clock
        .reset_i (reset_i),     // 6x clock-domain reset

        .enable_i(en_source),   // compute the alignment shift?
        .invert_i(tc_invert),   // TODO: use negative edge to compute phase?
        .strobe_o(tc_strobe),   // mark the arrival of a new value
        .middle_o(tc_middle),   // mark the arrival of a new value
        .sig_p_i (source_p),    // posedge DDR signal input
        .sig_n_i (source_n),    // negedge DDR signal input

        .phase_o (tc_phase),    // relative phase for stable signal
        .delta_o (tc_delta),    // phase error vs. previous sample
        .locked_o(tc_locked),   // TODO: signal is stable & locked
        .error_o (tc_invalid),  // signal can't lock, or lock lost
        .retry_i (tc_restart),  // acknowledge and retry

        //  Debug/info outputs:
        .ref_p_o (ref_p),
        .ref_n_o (ref_n)
        );


   //-------------------------------------------------------------------------
   //  Programmable delay that is used to phase-shift the incoming signal.
   //-------------------------------------------------------------------------
   wire [MSB:0] phased;

   assign signal_w = ALIGN ? phased : source;

   (* AREA_GROUP = "shreg" *)
   shift_reg
     #(  .DEPTH(8),         // should synthesise to SRL16E primitives?
         .ABITS(3),
         .DELAY(DELAY)
         ) SHREG [MSB:0]
       ( .clk(clock_i),
         .ce (ALIGN && en_capture),
         .a  (ALIGN ? tc_delay[3:1] : 3'h0),
         .d  (source),
         .q  (phased)
         );



   //-------------------------------------------------------------------------
   //
   //  IOB, DDR, SIGNAL-CAPTURE REGISTERS.
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
