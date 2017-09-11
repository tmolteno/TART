`timescale 1ns/100ps
/*
 * Module      : bench/acquire/mcb_dummy.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
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
 * Dummy Memory Controller Block (MCB) for TART.
 * 
 * NOTE:
 * 
 * TODO:
 *  + busy & refresh states;
 * 
 */

module mcb_dummy
  #(// Bit-width settings:
    parameter WIDTH = 32,
    parameter MSB   = WIDTH-1,
    parameter BSELS = WIDTH >> 2,
    parameter BSB   = BSELS-1,

    //  Memory-size parameters:
    parameter ABITS = 10,
    parameter ASB   = ABITS-1,
    parameter SIZE  = 1<<ABITS,

    //  Simulation-only options:
    parameter TICKS = 4,        // latency (clock-cycles)
    parameter TSB   = TICKS-1,  // MSB of the shift-register
    parameter START = 1000,     // start-up delay (ns)
    parameter DELAY = 3)        // combinational delay (ns)
   (
    input          clock_i,
    input          reset_i,

    output         active_o,
    input          request_i,
    input          write_i,
    output         ready_o,
    input [ASB:0]  address_i,
    input [BSB:0]  bytes_i,
    input [MSB:0]  data_i,
    output [MSB:0] data_o
    );


   reg [MSB:0]     mem[0:SIZE-1];
   reg [MSB:0]     dat[0:TSB];
   reg [TSB:0]     rdy = {TICKS{1'b0}};
   reg [TSB:0]     run = {TICKS{1'b1}};
   reg             active = 1'b0;
   wire            fetch, store, free;
   integer         idx = 0, ptr = TSB;


   assign fetch    = active && request_i && !write_i;
   assign store    = active && request_i &&  write_i;
   assign free     = &run;

   assign data_o   = dat[ptr];
   assign ready_o  = rdy[TSB];
   assign active_o = active && free;


   //-------------------------------------------------------------------------
   //  Pretend to initialise.
   //-------------------------------------------------------------------------
   initial begin
      #DELAY active = 1'b0;
      #START active = 1'b1;
   end

   always @(posedge clock_i)
     if (reset_i) begin
        #DELAY active = 1'b0;
        #START active = 1'b1;
     end

   always @(posedge clock_i)
     run <= #DELAY {~request_i, run[TSB:1]};


   //-------------------------------------------------------------------------
   //  Delay outgoing data, to simulate SDRAM + controller latency.
   //-------------------------------------------------------------------------
   //  Satisfy read requests, if initialised.
   always @(posedge clock_i) begin
      dat[idx] <= #DELAY fetch ? mem[address_i] : 'bz;
      rdy <= #DELAY {fetch, rdy[TSB:1]};
      idx <= #DELAY idx == TSB ? 0 : idx + 1;
      ptr <= #DELAY ptr == TSB ? 0 : ptr + 1;
   end


   //-------------------------------------------------------------------------
   //  Satisfy write requests, if initialised.
   //-------------------------------------------------------------------------
   //  TODO: Byte enables.
   always @(posedge clock_i)
     if (store)
       mem[address_i] <= #DELAY data_i;


endmodule // mcb_dummy
