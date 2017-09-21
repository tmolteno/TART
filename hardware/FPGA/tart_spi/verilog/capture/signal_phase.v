`timescale 1ns/100ps
/*
 * Module      : verilog/capture/signal_phase.v
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
 *  + only tested for the configuration with the sampling-clock having 12x
 *    the frequency of the data-clock;
 * 
 * TODO:
 *  + parameterised window size, when deciding if the edges is allowable;
 *  + hardware testing;
 *  + handle signed comparisons;
 * 
 */

module signal_phase
  #(//  Clock ratio settings:
    parameter RATIO = 12,       // clock (sampling:data) ratio
    parameter RBITS = 4,        // bit-width of down-sampling counter
    parameter RSB   = RBITS-1,  // MSB of counter
    parameter RMAX  = RATIO-1,  // max clock-counter value
    parameter RZERO = {RBITS{1'b0}},
    parameter HALF  = RMAX>>1,  // the floor of half the count
    parameter HSB   = HALF-1,   // MSB of a half-cycle counter

    //  Signal-source settings:
    parameter WIDTH = 24,       // number of input signal-sources
    parameter MSB   = WIDTH-1,  // MSB of signal sources
    parameter SBITS = 5,        // input signal-width
    parameter SSB   = SBITS-1,  // MSB of signals

    //  Signal MUX bit-widths:
    parameter TOTAL = 1<<SBITS, // total MUX width
    parameter TSB   = TOTAL-1,  // MSB of MUX width
    parameter QBITS = SBITS-2,  // select bits for the second-stage MUX
    parameter QSB   = QBITS-1,  // MSB of second-stage selector
    parameter MUX2  = 1<<QBITS, // second-stage MUX width
    parameter JSB   = MUX2-1,   // MSB of second-stage MUX input

    //  Signal-locking settings:
    parameter COUNT = 3,        // number of samples for a lock
    parameter CBITS = 2,        // counter bit-width
    parameter CSB   = CBITS-1,  // MSB of counter

    //  Additional features:
    parameter RESET = 1,        // TODO: fast-resets (0/1)?
    parameter DRIFT = 1,        // TODO: incremental changes to phase (0/1)?
    parameter CYCLE = 1,        // TODO: auto-strobe when disabled?

    //  Simulation-only parameters:
    parameter NOISY = 0,        // display extra debug info (0/1)?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input            clk_s_i, // half-rate (sampling) clock input
    input            clk_n_i, // inverted, half-rate (sampling) clock input
    input            reset_i, // (sample domain) reset

    input            clk_e_i, // must have the same frequency as signal
    input [MSB:0]    sig_e_i, // raw signal requiring clock-recovery

    input            align_i, // (sample domain) core enable
    input [SSB:0]    select_i,
    input            cycle_i, // strobe every 'TRATE' ticks?
    input            drift_i, // not useful for statistics
    output           start_o, // strobes for each new sample
    output [RSB:0]   phase_o, // recovered phase
    output [RBITS:0] delta_o, // the computed error
    output           valid_o, // signal locked
    output           error_o, // lock lost
    input            retry_i  // acknowledge any invalid data
    );


   reg               valid = 1'b0, error = 1'b0;
   wire              pos_w, neg_w, sig_w;


   //-------------------------------------------------------------------------
   //  Assignments to outputs.
   //-------------------------------------------------------------------------
   assign start_o  = stb_p;
   assign phase_o  = phase;
   assign delta_o  = delta;

   assign valid_o  = valid;     // TODO:
   assign error_o  = error;


   //-------------------------------------------------------------------------
   //
   //  ESTABLISH A REFERENCE SIGNAL.
   //
   //-------------------------------------------------------------------------
   //  Capture & oversample a reference signal, and from the external clock
   //  domain.
   reg               pos_e = 1'b0, neg_e = 1'b0, xedge = 1'b0;

   //  Reference-signal synchronisers.
   (* NOMERGE = "TRUE" *)
   reg               pos_s, pos_x, stb_p, neg_s, neg_x, stb_n;

   //  Input-signal synchronisers.
   (* NOMERGE = "TRUE" *)
   reg               sig_s, sig_x;


   //-------------------------------------------------------------------------
   //  Detect the positive and negative edges of the signal-clock.
   //-------------------------------------------------------------------------
   assign pos_w = pos_x ^ pos_s;
   assign neg_w = neg_x ^ neg_s;
   assign sig_w = sig_x ^ sig_s;


   //-------------------------------------------------------------------------
   //  Toggle a register at the external-clock rate, to simulate a signal to
   //  use as a reference for clock-recovery.
   //-------------------------------------------------------------------------
   always @(posedge clk_e_i)
     pos_e <= #DELAY ~pos_e;

   always @(negedge clk_e_i)
     neg_e <= #DELAY ~neg_e;

   //-------------------------------------------------------------------------
   //  Synchronise the input signal.
   always @(posedge clk_s_i)
     {sig_x, sig_s} <= #DELAY {sig_s, sig_e_i};

   //  And synchronise the reference signal.
   always @(posedge clk_s_i) begin
      {pos_x, pos_s} <= #DELAY {pos_s, pos_e};
      {neg_x, neg_s} <= #DELAY {neg_s, neg_e};
   end

   //-------------------------------------------------------------------------
   //  Strobe at each new edge.
   always @(posedge clk_s_i) begin
      stb_p <= #DELAY pos_w;
      stb_n <= #DELAY neg_w;
      xedge <= #DELAY sig_w;
   end



   //-------------------------------------------------------------------------
   //
   //  SELECT THE SOURCE SIGNAL.
   //
   //-------------------------------------------------------------------------
   wire [TSB:0]    signal_w;
   reg             signal;
   reg [JSB:0]     stage2;
   reg [QSB:0]     select;
   reg [1:0]       enable;


   assign signal_w = {{TOTAL-WIDTH{1'bx}}, sig_e_i};


   //-------------------------------------------------------------------------
   //  Select the signal source.
   //-------------------------------------------------------------------------
   //  Uses a two-stage MUX.
   always @(posedge clk_s_i)
     begin
        //  align-enable signal delays:
        enable <= #DELAY {enable[0], align_i};

        //  first-stage signal MUX:
        stage2 <= #DELAY signal_w >> {select_i[SSB:QBITS], {QBITS{1'b0}}};
        select <= #DELAY select_i[QSB:0];

        //  second-stage signal MUX:
        signal <= #DELAY stage2[select];
     end


   //-------------------------------------------------------------------------
   //
   //  COMPUTE THE RELATIVE PHASES.
   //
   //-------------------------------------------------------------------------
   wire [RBITS:0] cnext = cycle + 1;
   wire           cwrap = cycle == RMAX;
   reg [RSB:0]    cycle = RZERO, phase = RZERO;
   reg [RBITS:0]  delta;


   //-------------------------------------------------------------------------
   //  Count the cycles/periods since the signal-clock positive-edge.
   //-------------------------------------------------------------------------
   //  NOTE: This is used to compute the relative phases, of the signal-clock
   //    vs the clock of the input-signal.
   always @(posedge clk_s_i)
     if (pos_w)
       cycle <= #DELAY RZERO;
     else
       cycle <= #DELAY cnext[RSB:0];

   //-------------------------------------------------------------------------
   //  At each input-signal edge-event, latch the relative phase-value.
   always @(posedge clk_s_i)
     if (xedge) begin
        phase <= #DELAY cycle;
        delta <= #DELAY cycle - phase;
     end



endmodule // signal_phase
