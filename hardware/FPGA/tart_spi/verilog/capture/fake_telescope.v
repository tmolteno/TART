`timescale 1ns/100ps
/*
 * Module      : verilog/capture/fake_telescope.v
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
 * Generates either incrementing, pseudorandom, constant, or toggling values,
 * for purposes including:
 *  + filling the TART SDRAM with debug data;
 *  + training the clock-recovery unit; and
 *  + testing the correlators.
 * 
 * NOTE:
 *  + MFSR stands for Multiple-Feedback Shift Register;
 * 
 * TODO:
 * 
 */

module fake_telescope
  #(//  Data-bus and bit-width settings:
    parameter WIDTH = 24,
    parameter MSB   = WIDTH-1,

    //  Fake-data generation-modes:
    parameter MULTI = 1, // runtime mode setting (0/1)?
    parameter RNG   = 1, // use random (MFSR) data for testing?
    parameter CONST = 0, // use constant data for testing correlators?
    parameter CDATA = 0, // constant data value
    parameter START = 0, // starting data value

    //  Simulation-only setttings:
    parameter DELAY = 3)
   (
    input          clock_i,     // generator clock
    input          reset_i,
    input          enable_i,

    input          shift_i, // use the shift-register (MFSR)
    input          count_i, // use the (up-)counter

    output         valid_o,
    output [MSB:0] data_o
    );


   //-------------------------------------------------------------------------
   //  Feedback shift-registers can be good sources of pseudorandom data.
   wire [31:0]     mfsr_new;
   reg [31:0]      mfsr_reg = 32'h1;

   //-------------------------------------------------------------------------
   //  Toggle every cycle (which is good for training the clock-recovery
   //  unit).
   reg [MSB:0]     flip_reg = CDATA;

   //-------------------------------------------------------------------------
   //  Standard up-counters should give predictable correlator outputs.
   reg [MSB:0]     data_inc = RNG;
   wire [WIDTH:0]  next_data;

   //-------------------------------------------------------------------------
   //  Signals used to generate outputs.
   wire [MSB:0]    tick_data, fake_data;
   reg [MSB:0]     data_mux = START;
   reg             valid_r = 1'b0, valid_s = 1'b0;


   assign next_data = data_inc + 1;
   assign tick_data = RNG   == 0 ? data_inc  : mfsr_reg[MSB:0];
   assign fake_data = CONST == 0 ? tick_data : CDATA;

   assign valid_o   = MULTI ? valid_s  : valid_r;
   assign data_o    = MULTI ? data_mux : fake_data;


   //-------------------------------------------------------------------------
   //  Generate registered, fake data, when enabled.
   //-------------------------------------------------------------------------
   //  Takes two cycles for valid output to be produced.
   always @(posedge clock_i)
     if (reset_i) begin
        valid_r  <= #DELAY 1'b0;
        valid_s  <= #DELAY 1'b0;
     end
     else begin
        valid_r  <= #DELAY enable_i;
        valid_s  <= #DELAY valid_r;
     end

   //-------------------------------------------------------------------------
   //  Compute the incremented values.
   always @(posedge clock_i)
     if (reset_i) begin
        data_inc <= #DELAY START;
        mfsr_reg <= #DELAY 32'h1;
        flip_reg <= #DELAY CDATA;
     end
     else if (enable_i) begin
        data_inc <= #DELAY next_data[MSB:0];
        mfsr_reg <= #DELAY mfsr_new;
        flip_reg <= #DELAY ~flip_reg;
     end

   //-------------------------------------------------------------------------
   //  Select the data-source.
   always @(posedge clock_i)
     if (valid_r)
       case ({count_i, shift_i})
         2'b00: data_mux <= #DELAY CDATA;
         2'b01: data_mux <= #DELAY mfsr_reg;
         2'b10: data_mux <= #DELAY data_inc;
         2'b11: data_mux <= #DELAY flip_reg;
       endcase // case ({count_i, shift_i})


   //-------------------------------------------------------------------------
   //  One of Roy's MFSR's (which is similar to a LFSR, but fewer gates).
   //-------------------------------------------------------------------------
   mfsr32 MFSR32 (.count_i(mfsr_reg), .count_o(mfsr_new));


endmodule // fake_telescope
