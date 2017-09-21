`timescale 1ns/100ps
/*
 * Module      : verilog/capture/signal_phase_DDR.v
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
 * Performs clock-recovery to determine the phase of the incoming signal (as
 * the frequency is assumed to known and fixed).
 * 
 * NOTE:
 *  + only tested for the configuration with the sampling-clock having 6x the
 *    frequency of the data-clock (and due to the DDR sampling, this has the
 *    same resolution as using a 12x clock with only SDR);
 *  + DDR sampling is used so that the input signals can be captured, and then
 *    distributed, with clocks slow enough for XST to easily meet timing;
 *  + assumes that the input signals are fairly well-behaved in that they are
 *    largely free from transients, have low jitter, and that the maximum
 *    phase-difference between any pair of antennas is less than quarter of a
 *    wavelength;
 * 
 * TODO:
 *  + parameterised window size, when deciding if the edges is allowable;
 *  + hardware testing;
 *  + handle signed comparisons;
 * 
 */

module signal_phase_DDR
  #(//  Clock ratio settings:
    parameter RATIO = 6,        // (DDR) sample-clock to external-clock ratio
    parameter RBITS = 3,        // bit-width of down-sampling counter
    parameter RSB   = RBITS-1,  // MSB of counter
    parameter RMAX  = RATIO-1,  // max clock-counter value
    parameter RZERO = 3'h0,

    //  Additional options for the reference signal:
    parameter TICKS = 2,        // clock-cycle delay of the source?
    parameter TSB   = TICKS-1,  // MSB of the delay shift-register
    parameter TSC   = TSB-1,    // MSB of the retained shift-register
    parameter POLAR = 0,        // use inverted polarity for reference?

    //  Signal-locking settings:
    parameter LIMIT = 2,        // signal valid if delta doesn't exceed this
    parameter COUNT = 7,        // number of samples for a lock
    parameter CBITS = 3,        // counter bit-width
    parameter CSB   = CBITS-1,  // MSB of counter
    parameter CZERO = 0,

    //  Additional features:
    parameter RESET = 0,        // TODO: fast-resets (0/1)?

    //  Simulation-only parameters:
    parameter NOISY = 0,        // display extra debug info (0/1)?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input            clk_e_i, // external (and reference) clock
    input            clk_s_i, // half-rate (sampling) clock input
    input            clk_n_i, // inverted, half-rate (sampling) clock input
    input            reset_i, // (sample domain) reset

    input            enable_i, // (sample domain) core enable
    input            invert_i, // use negative edges for reference?
    output           strobe_o, // strobes for each new sample
    output           middle_o, // strobes mid-sample
    input            sig_p_i, // DDR signal requiring clock-recovery
    input            sig_n_i, // DDR signal requiring clock-recovery

    //  Recovered-signal info:
    output [RBITS:0] phase_o, // recovered phase
    output [RBITS:0] delta_o, // the computed error
    output           locked_o, // signal locked
    output           error_o, // lock lost
    input            retry_i, // acknowledge any invalid data

    //  Debug/info outputs:
    output           ref_p_o, // output the reference signals
    output           ref_n_o
    );


   //-------------------------------------------------------------------------
   //  Reference signals, synchronisers, and delays.
   reg               pos_e = 1'b0, neg_e = 1'b0;
   reg [TSB:0]       pos, neg;
   reg               stb_p, stb_n;
   wire              pos_w, neg_w, cycled;

   //-------------------------------------------------------------------------
   //  Input signals, synchronisers, and edge-detectors.
   (* NOMERGE = "TRUE" *)
   reg               sig_n, sig_p;
   reg [1:0]         xedge = 2'b00;
   wire              edg_0, edg_1, edg_w;

   //-------------------------------------------------------------------------
   //  Cycle-count and phase signals and registers.
   wire [RBITS:0]    cnext = cycle + 1;
   wire              cwrap = cycle == RMAX;
   reg [RSB:0]       cycle = RZERO;
   reg [RBITS:0]     phase = RZERO;
   reg signed [RBITS:0] delta;
   wire [RBITS:0]    phase_w = {cycle, xedge[0]};
   wire [RBITS:0]    delta_w = phase_w - phase;

   // TODO:
   reg               valid = 1'b0, error = 1'b0;



   //-------------------------------------------------------------------------
   //  Assignments to outputs.
   //-------------------------------------------------------------------------
   assign strobe_o = stb_p;
   assign middle_o = stb_n;
   assign phase_o  = phase;
   assign delta_o  = delta;

   assign locked_o = valid;     // TODO:
   assign error_o  = error;

   assign ref_p_o  = pos[TSB];
   assign ref_n_o  = neg[TSB];


   //-------------------------------------------------------------------------
   //  Detect the positive and negative edges of the reference signal.
   //-------------------------------------------------------------------------
   assign pos_w  = pos[TSB] ^ pos[TSC];
   assign neg_w  = neg[TSB] ^ neg[TSC];

   assign cycled = POLAR ? (pos_w && !invert_i || neg_w && invert_i) : pos_w;


   //-------------------------------------------------------------------------
   //  Source-signal edge detection.
   //-------------------------------------------------------------------------
   //  There are two (unique) possible locations for transitions, at each
   //  positive-edge of the (half-rate) sampling-clock.
   assign edg_0   = sig_p ^ sig_p_i && sig_n ^ sig_n_i;
   assign edg_1   = sig_p ^ sig_n;

   //-------------------------------------------------------------------------
   //  An edge has been found whenever any of the previous values differ from
   //  the current values.
   assign edg_w   = sig_n ^ sig_n_i;



   //-------------------------------------------------------------------------
   //
   //  ESTABLISH A REFERENCE SIGNAL.
   //
   //-------------------------------------------------------------------------
   //  Toggle a register at the external-clock rate, to simulate a signal to
   //  use as a reference for clock-recovery.
   //-------------------------------------------------------------------------
   always @(posedge clk_e_i)
     pos_e <= #DELAY ~pos_e;

   always @(negedge clk_e_i)
     neg_e <= #DELAY ~neg_e;

   //-------------------------------------------------------------------------
   //  And synchronise (plus delay) the reference signal.
   always @(posedge clk_s_i)
     if (reset_i && RESET) begin
        pos <= #DELAY {TICKS{1'b0}};
        neg <= #DELAY {TICKS{1'b0}};
     end
     else begin
        pos <= #DELAY {pos[TSC:0], pos_e};
        neg <= #DELAY {neg[TSC:0], neg_e};
     end

   //-------------------------------------------------------------------------
   //  Strobe at each new edge.
   always @(posedge clk_s_i)
     if (reset_i && RESET)
       {xedge, stb_n, stb_p} <= #DELAY 4'h0;
     else begin
        stb_p <= #DELAY pos_w;
        stb_n <= #DELAY neg_w;
        xedge <= #DELAY {edg_1, edg_0};
     end



   //-------------------------------------------------------------------------
   //
   //  COMPUTE THE RELATIVE PHASES.
   //
   //-------------------------------------------------------------------------
   //  Register the current inputs, for future edge comparisons.
   //-------------------------------------------------------------------------
   always @(posedge clk_s_i)
     if (reset_i && RESET) begin
        sig_p <= #DELAY 1'b0;
        sig_n <= #DELAY 1'b0;
     end
     else begin
        sig_p <= #DELAY sig_p_i;
        sig_n <= #DELAY sig_n_i;
     end


   //-------------------------------------------------------------------------
   //  Count the cycles/periods since the signal-clock positive-edge.
   //-------------------------------------------------------------------------
   //  NOTE: This is used to compute the relative phases, of the signal-clock
   //    vs the clock of the input-signal.
   always @(posedge clk_s_i)
     if (reset_i && RESET || cycled)
       cycle <= #DELAY RZERO;
     else
       cycle <= #DELAY cnext[RSB:0];

   //-------------------------------------------------------------------------
   //  After every positive (negative) edge of the external (XTAL) clock,
   //  count the number of fast-clock transitions until an edge is found.
   //  Once found, compute the new relative phase, and the difference vs. the
   //  previous phase value.
   always @(posedge clk_s_i)
     if (reset_i && RESET) begin
        phase <= #DELAY RZERO;
        delta <= #DELAY RZERO;
     end
     else if (xedge[0] || xedge[1]) begin
        phase <= #DELAY phase_w;
        delta <= #DELAY delta_w;
     end



   //-------------------------------------------------------------------------
   //
   //  SIGNAL LOCKING.
   //
   //-------------------------------------------------------------------------
   wire [CBITS:0] lnext;
   wire           lwrap, llost, bound;
   reg [CSB:0]    count = CZERO;
   reg            ledge = 1'b0;


   assign lnext = count + 1;
   assign lwrap = lnext == COUNT[CBITS:0];
   assign llost = valid && (delta < -LIMIT || delta > LIMIT);
   assign bound = -LIMIT <= delta && delta <= LIMIT;


   //-------------------------------------------------------------------------
   //  Detect valid edges.
   always @(posedge clk_s_i)
     ledge <= #DELAY enable_i && (|xedge) && !error;

   //-------------------------------------------------------------------------
   //  Detect out-of-bounds errors.
   always @(posedge clk_s_i)
     if (reset_i && RESET || retry_i || !enable_i)
       error <= #DELAY 1'b0;
     else if (valid && !bound)
       error <= #DELAY 1'b1;


   //-------------------------------------------------------------------------
   //  Consider the signal locked if the delta stays within some predefined
   //  bounds.
   //-------------------------------------------------------------------------
   always @(posedge clk_s_i)
     if (reset_i && RESET || error || retry_i || !enable_i) begin
        valid <= #DELAY 1'b0;
        count <= #DELAY CZERO;
     end
     else if (valid) begin
        valid <= #DELAY valid;
        count <= #DELAY count;
     end
     else if (ledge &&  bound) begin
        valid <= #DELAY lwrap;
        count <= #DELAY lnext;
     end
     else if (ledge && !bound) begin
        valid <= #DELAY 1'b0;
        count <= #DELAY CZERO;
     end



endmodule // signal_phase_DDR
