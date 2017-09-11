`timescale 1ns/100ps
/*
 * Module      : verilog/capture/signal_centre.v
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
 * Performs clock-recovery on the selected signal, and outputing the measured
 * phase-shift, which could then be used for aligning multiple signals.
 * 
 * NOTE:
 *  + 'RATIO' is the oversampling-frequency ratio;
 * 
 * TODO:
 *  + parameterised number of MUX stages;
 * 
 */

module signal_centre
  #(//  Data bit-width parameters:
    parameter WIDTH = 24,       // number of signals
    parameter MSB   = WIDTH-1,  // MSB of data signal
    parameter SBITS = 5,        // number of select bits
    parameter SSB   = SBITS-1,  // MSB of selector
    parameter TOTAL = 1<<SBITS, // total MUX width
    parameter TSB   = TOTAL-1,  // MSB of MUX width
    parameter QBITS = SBITS-2,  // select bits for the second-stage MUX
    parameter QSB   = QBITS-1,  // MSB of second-stage selector
    parameter MUX2  = 1<<QBITS, // second-stage MUX width
    parameter JSB   = MUX2-1,   // MSB of second-stage MUX input

    //  Data-alignment settings:
    parameter RATIO = 12,       // oversampling ratio?
    parameter RBITS = 4,        // bit-width of clock-counter
    parameter RSB   = RBITS-1,  // MSB of the clock-counter
    parameter RESET = 1,        // enable fast-resets (0/1)?
    parameter DRIFT = 1,        // incremental changes to the phase (0/1)?
    parameter CYCLE = 1,        // auto-strobing when not centring (0/1)?

    //  Spartan 6 specific settings:
    parameter IOB   = 0,        // use IOB-based registers (0/1)?

    //  Simulation-only options:
    parameter NOISY = 0,        // display extra debug info (0/1)?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input          clock_i, // oversampling (by 'RATIO') clock
    input          reset_i, // clears all stored timing info
    input          align_i, // align the inputs while asserted
    input          start_i, // auto-strobe once started (and 'CYCLE')
    input          drift_i, // incrementally change the phase?

    input [MSB:0]  signal_i, // raw signal
    input [SSB:0]  select_i, // selects one of the `WIDTH` signals

    output         strobe_o, // strobes for each new output-value
    output         locked_o, // valid data is being emitted
    output [RSB:0] phase_o,  // phase-shift required to centre signal
    output         invalid_o, // lost tracking of the signal
    input          restart_i  // clear the error flag
    );


   wire [TSB:0]    signal_w;
   reg             signal, cycle = 1'b0;
   reg [JSB:0]     stage2;
   reg [QSB:0]     select;
   reg [1:0]       enable;


   assign signal_w = {{TOTAL-WIDTH{1'bx}}, signal_i};


   //-------------------------------------------------------------------------
   //  Select the signal source.
   //-------------------------------------------------------------------------
   //  Uses a two-stage MUX.
   always @(posedge clock_i)
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
   //  Activate the auto-strobe.
   //-------------------------------------------------------------------------
   //  NOTE: The 'start_i' determines the reference signal phase, that all
   //    subsequent phases are measured against.
   always @(posedge clock_i)
     if (reset_i)
       cycle <= #DELAY 1'b0;
     else if (start_i && CYCLE)
       cycle <= #DELAY 1'b1;


   //-------------------------------------------------------------------------
   //  Use a signal-capture block to measure the phase of one channel of the
   //  input signals.
   //-------------------------------------------------------------------------
   //  NOTE: The aligned signal output isn't used, as it's just the phase that
   //    is needed. It's used to measure each channel, one-by-one, and then
   //    for calculating the "best" shift-amount, for capturing & aligning all
   //    individual antenna sources.
   signal_capture
     #( .RATIO(RATIO),
        .RBITS(RBITS),
        .RESET(RESET),
        .DRIFT(DRIFT),
        .CYCLE(CYCLE),
        .IOB  (IOB),
        .NOISY(NOISY),
        .DELAY(DELAY)
        ) PHASE
     (  .clock_i  (clock_i),
        .reset_i  (reset_i),
        .align_i  (enable[1]),
        .cycle_i  (cycle),
        .drift_i  (drift_i),
        .signal_i (signal),
        .signal_o (),
        .ready_o  (strobe_o),
        .phase_o  (phase_o),
        .locked_o (locked_o),
        .invalid_o(invalid_o),
        .retry_i  (restart_i)
        );


   //-------------------------------------------------------------------------
   //
   //  DEBUGGING STUFF.
   //
   //-------------------------------------------------------------------------
   //  Should produce equivalent behaviour to `signal` (vs. `sig1`).
   wire sig_w = signal_i[select_i];
   reg  sig1, sig0;

   always @(posedge clock_i)
     {sig1, sig0} <= #DELAY {sig0, sig_w};


endmodule // signal_centre
