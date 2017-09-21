`timescale 1ns/100ps
/*
 * Module      : verilog/capture/signal_capture.v
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
 * Captures data by sampling at a higher (synchronous) clock-rate, and
 * presenting the captured data halfway through each period.
 * 
 * NOTE:
 *  + output is "sample-and-hold" and valid once `ready` asserts;
 *  + only tested for the configuration with the sampling-clock having 12x
 *    the frequency of the data-clock;
 * 
 * TODO:
 *  + parameterised window size, when deciding if the edges is allowable;
 *  + hardware testing;
 *  + clean up the combinational paths;
 *  + handle signed comparisons;
 * 
 */

module signal_capture
  #(//  Clock ratio settings:
    parameter RATIO = 12,       // clock (sampling:data) ratio
    parameter RBITS = 4,        // bit-width of down-sampling counter
    parameter RSB   = RBITS-1,  // MSB of counter
    parameter RMAX  = RATIO-1,  // max clock-counter value
    parameter RZERO = {RBITS{1'b0}},
    parameter HALF  = RMAX>>1,  // the floor of half the count
    parameter HSB   = HALF-1,   // MSB of a half-cycle counter

    //  Additional features:
    parameter RESET = 1,        // fast-resets (0/1)?
    parameter DRIFT = 1,        // incremental changes to phase (0/1)?
    parameter CYCLE = 1,        // auto-strobe when disabled?
    parameter IOB   = 0,        // use Spartan 6 IOB's?

    //  Simulation-only parameters:
    parameter NOISY = 0,        // display extra debug info (0/1)?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input          clock_i, // (sampling) clock input
    input          reset_i, // (sample domain) reset

    input          align_i, // (sample domain) core enable
    input          cycle_i, // strobe every 'TRATE' ticks?
    input          drift_i, // not useful when calculating statistics
    output         ready_o,
    output [RSB:0] phase_o,
    output         locked_o,
    output         invalid_o,
    input          retry_i, // acknowledge any invalid data

    input          signal_i, // raw data
    output         signal_o  // captured data
    );


   //  Supersample at the given rate.
   wire [HALF:0]   samples;
   reg [RSB:0]     count = RZERO;
   reg             ready = 1'b0, locked = 1'b0, invalid = 1'b0, signal;
   reg [HSB:0]     d_reg = {HALF{1'b0}};

   wire [RSB:0]    count_init, count_next;
   wire            count_wrap;

   wire            cyc_w, stb_w, vld_w, rdy_w, bad_w;
   wire            no_edges, edge_pos, edge_neg, edge_found;

   //  Place the first capture-register.
   (* IOB = "TRUE" *)
   reg             d_iob = 1'b0;
   reg             d_sig = 1'b0;
   wire            d_src;

   //  Phase-locking control-signals.
   reg [1:0]       locked_count = 2'h0;
   wire [2:0]      locked_cnext;
   wire            locked_cwrap;


   //-------------------------------------------------------------------------
   //  Assignments to outputs.
   //-------------------------------------------------------------------------
   assign ready_o   = ready;
   assign locked_o  = locked;
   assign phase_o   = phase;
   assign invalid_o = invalid;
   assign signal_o  = signal;

   //  Concatenate all registered signals values, to be indexed.
   assign samples = {d_reg, d_src};

   //  Internal, combinational conditionals.
   assign cyc_w = CYCLE && cycle_i;
   assign stb_w = cyc_w || align_i && locked;
   assign rdy_w = stb_w && count == HALF;
   assign vld_w = count >= RATIO-3 && count < RATIO+2;
   assign bad_w = locked && edge_found && !vld_w;

   //  Use IOB-based registers, or ordinary fabric-based registers?
   assign d_src = IOB ? d_iob : d_sig;

   assign no_edges = &samples || ~|samples;
//    assign edge_neg = samples == {{HALF{1'b1}}, 1'b0};
//    assign edge_pos = samples == {{HALF{1'b0}}, 1'b1};
   assign edge_pos = samples == {1'b0, {HALF{1'b1}}};
   assign edge_neg = samples == {1'b1, {HALF{1'b0}}};
   assign edge_found = edge_neg || edge_pos;

   //  Count the spacing between edges.
   assign count_next = edge_found || count_wrap ? count_init : count + 1;
   assign count_wrap = count >= RATIO-1 && no_edges;
   assign count_init = count == RATIO+1 ? 1 : count == RATIO-3 ? -1 : 0;

   //-------------------------------------------------------------------------
   //  Locked once four well-behaved edges have been found.
   assign locked_cwrap = locked_count == 3;
   assign locked_cnext = !locked_cwrap ? locked_count + 1 : locked_count;


   //-------------------------------------------------------------------------
   //  Capture within an IOB's register, and use two samples for edge
   //  detection.
   //-------------------------------------------------------------------------
   reg             enable = 1'b0;

   always @(posedge clock_i)
     if (reset_i)
       enable <= #DELAY 1'b0;
     else if (align_i && count_wrap)
       enable <= #DELAY 1'b1;
     else if (!align_i && count_wrap)
       enable <= #DELAY 1'b0;
     else
       enable <= #DELAY enable;

   always @(posedge clock_i)
     if (align_i)
       {d_reg, d_sig, d_iob} <= #DELAY {samples[HSB:0], signal_i, signal_i};


   //-------------------------------------------------------------------------
   //  Count the number of cycles between edges.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i)
       count <= #DELAY RZERO;
     else if (align_i || cyc_w)
       count <= #DELAY count_next;
     else
       count <= #DELAY count;


   //-------------------------------------------------------------------------
   //  The signal is considered locked after four clean transitions.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i) begin
        locked       <= #DELAY 1'b0;
        locked_count <= #DELAY 2'h0;
     end
     else if (align_i && edge_found && vld_w) begin
        locked       <= #DELAY locked_cwrap;
        locked_count <= #DELAY locked_cnext[1:0];
     end
     else if (align_i && edge_found) begin // Edge too far from acceptable.
        locked       <= #DELAY 1'b0;
        locked_count <= #DELAY 2'h0;
     end

   //-------------------------------------------------------------------------
   // If the signal is `locked`, and `edge_found`, but occuring to far to be
   // considered a valid count, then assert `invalid`.
   // Clear `invalid` whenever any earlier `invalid` data is acknowledged.
   always @(posedge clock_i)
     if (reset_i)
       invalid <= #DELAY 1'b0;
     else if (align_i && bad_w)
       invalid <= #DELAY 1'b1;
     else if (retry_i)
       invalid <= #DELAY 1'b0;
     else
       invalid <= #DELAY invalid;


   //-------------------------------------------------------------------------
   //  Output the captured data-samples.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (reset_i && RESET)
       ready <= #DELAY 1'b0;
     else
       begin
          ready  <= #DELAY rdy_w;
          signal <= #DELAY rdy_w ? samples[HALF] : signal;
       end


   //-------------------------------------------------------------------------
   //  Compute the relative phases.
   //-------------------------------------------------------------------------
   wire [RBITS:0] cnext = cwrap ? {1'b0, RZERO} : cycle + 1;
   wire           cwrap = cycle == RMAX;
   reg [RSB:0]    cycle = RZERO, phase = RZERO;
   reg            found = 1'b0;
   wire [RBITS:0] upper = {1'b0, phase} + 2;
   wire [RBITS:0] lower = {1'b0, phase} - 2;
   reg [RSB:0]    pprev, pnext;
   wire           drift = DRIFT && drift_i;


   //-------------------------------------------------------------------------
   //  Counts the cycles/periods since this module was enabled, so that
   //  relative phases can be tracked, for the edges of the oversampled input
   //  signal.
   always @(posedge clock_i)
     if (align_i)
       cycle <= #DELAY cnext;
     else
       cycle <= #DELAY RZERO;

   //-------------------------------------------------------------------------
   //  For the first edge that is found, load the phase register, but then
   //  only increment/decrement the phase, for subsequent edges.
   always @(posedge clock_i)
     if (!align_i)
       found <= #DELAY 1'b0;
     else if (drift && align_i && edge_found)
       found <= #DELAY 1'b1;
     else
       found <= #DELAY found;

   //-------------------------------------------------------------------------
   //  Once an initial value has been latched, only allow the phase to move
   //  slowly.
   always @(posedge clock_i)
     if (align_i && edge_found) begin
        if (drift && found && cycle > phase)
          phase <= #DELAY pnext;
        else if (drift && found && cycle < phase)
          phase <= #DELAY pprev;
        else
          phase <= #DELAY cycle;
     end
     else
       phase <= #DELAY phase;

   //  Pipeline the up/down counters.
   always @(posedge clock_i)
     if (drift) begin
        pnext <= #DELAY phase + 1;
        pprev <= #DELAY phase - 1;
     end


   //-------------------------------------------------------------------------
   //  Signal tracking.
   //-------------------------------------------------------------------------
   reg plost = 1'b0;

   always @(posedge clock_i)
     if (reset_i && RESET || !align_i)
       plost <= #DELAY 1'b0;
     else if (locked && found && edge_found && (cycle > pnext || cycle < pprev))
       plost <= #DELAY 1'b1;
     else
       plost <= #DELAY plost;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION STUFF.
   //
   //-------------------------------------------------------------------------
   reg [RSB:0]    predictor = RZERO;
   wire           expected  = predictor == RMAX;

   always @(posedge clock_i)
     if (reset_i)
       predictor <= #DELAY RZERO;
     else if (align_i) begin
        if (expected && count_next < HALF)
          predictor <= #DELAY count_next;
        else if (edge_found && predictor < HALF)
          predictor <= #DELAY count_next;
        else if (expected)
          predictor <= #DELAY RZERO;
        else
          predictor <= #DELAY predictor + 1;
     end

   //-------------------------------------------------------------------------
   //  Display the locked periods.
   integer      period;
   reg          running;

   always @(posedge clock_i)
     if (reset_i || !locked) begin
        running <= #DELAY 1'b0;
        period  <= #DELAY 'hx;
     end
     else if (running && ready) begin
        running <= #DELAY running;
        period  <= #DELAY 1;
        if (NOISY)
          $display("%12t: period \t=%4d", $time, period);
     end
     else if (running) begin
        running <= #DELAY running;
        period  <= #DELAY period + 1;
     end
     else if (ready && locked) begin
        running <= #DELAY 1;
        period  <= #DELAY 0;
     end
   

endmodule // signal_capture
