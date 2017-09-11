`timescale 1ns/100ps
/*
 * Module      : verilog/capture/signal_source.v
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
 * Two-stage, DDR multiplexer for the input signals.
 * 
 * NOTE:
 *  + only tested for the configuration with the sampling-clock having 6x
 *    the frequency of the data-clock;
 * 
 * TODO:
 *  + additional hardware testing, and also testing different parameter
 *    values;
 *  + parameterise the number of pipeline stages?
 * 
 */

module signal_source
  #(//  Signal-source settings:
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

    //  Additional features:
    parameter RESET = 0,        // TODO: fast-resets (0/1)?

    //  Simulation-only parameters:
    parameter NOISY = 0,        // display extra debug info (0/1)?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input         clock_i, // half-rate/DDR (sampling) clock input
    input         reset_i, // (sample domain) reset

    input         enable_i, // (sample domain) core enable
    input [SSB:0] select_i, // chooses the signal-source
    input [MSB:0] sig_p_i, // DDR signal requiring clock-recovery
    input [MSB:0] sig_n_i, // DDR signal requiring clock-recovery
    output        valid_o, // signal valid
    output reg    sig_p_o, // posedge MUX output
    output reg    sig_n_o // negedge MUX output
    );


   //-------------------------------------------------------------------------
   //
   //  TWO-STAGE MUX TO SELECT THE SOURCE SIGNAL.
   //
   //-------------------------------------------------------------------------
   wire [TSB:0]    sig_p_w, sig_n_w;
   reg [JSB:0]     sig_p2, sig_n2;
   reg [QSB:0]     select;
   reg [1:0]       enable;


   assign valid_o = enable[1];

   assign sig_p_w = {{TOTAL-WIDTH{1'bx}}, sig_p_i};
   assign sig_n_w = {{TOTAL-WIDTH{1'bx}}, sig_n_i};


   //-------------------------------------------------------------------------
   //  Select the signal source.
   //-------------------------------------------------------------------------
   //  NOTE: Uses a two-stage MUX. The first stage is a 4:1 MUX (which is one
   //    (six-input LUT per selected source-bit), and the second stage is an
   //    8:1 MUX.
   always @(posedge clock_i)
     if (reset_i && RESET)
       enable <= #DELAY 2'b00;
     else begin
        //  align-enable signal delays:
        enable  <= #DELAY {enable[0], enable_i};

        //  first-stage signal MUX:
        sig_p2  <= #DELAY sig_p_w >> {select_i[SSB:QBITS], {QBITS{1'b0}}};
        sig_n2  <= #DELAY sig_n_w >> {select_i[SSB:QBITS], {QBITS{1'b0}}};
        select  <= #DELAY select_i[QSB:0];

        //  second-stage signal MUX:
        sig_p_o <= #DELAY sig_p2[select];
        sig_n_o <= #DELAY sig_n2[select];
     end



endmodule // signal_source
