`timescale 1ns/100ps
/*
 * Module      : bench/mcb_dummy.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
 * 
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
    parameter ABITS = 10,
    parameter ASB   = ABITS-1,
    parameter BSELS = WIDTH >> 2,
    parameter BSB   = BSELS-1,

    //  Memory-size parameters:
    parameter SIZE  = 1<<ABITS,

    //  Simulation-only options:
    parameter START = 1000,     // start-up delay (ns)
    parameter DELAY = 3)        // combinational delay (ns)
   (
    input              clock_i,
    input              reset_i,

    output reg         active_o = 1'b0,
    input              request_i,
    input              write_i,
    output reg         ready_o = 1'b0,
    input [ASB:0]      address_i,
    input [BSB:0]      bytes_i,
    input [MSB:0]      data_i,
    output reg [MSB:0] data_o
    );


   reg [MSB:0]         mem[0:SIZE-1];


   //-------------------------------------------------------------------------
   //  Pretend to initialise.
   //-------------------------------------------------------------------------
   initial begin
      #DELAY active_o = 1'b0;
      #START active_o = 1'b1;
   end

   always @(posedge clock_i)
     if (reset_i) begin
        #DELAY active_o = 1'b0;
        #START active_o = 1'b1;
     end


   //-------------------------------------------------------------------------
   //  Satisfy read requests, if initialised.
   //-------------------------------------------------------------------------
   always @(posedge clock_i)
     if (active_o && request_i && !write_i) begin
        data_o  <= #DELAY mem[address_i];
        ready_o <= #DELAY 1'b1;
     end
     else begin
        data_o  <= #DELAY {WIDTH{1'bz}};
        ready_o <= #DELAY 1'b0;
     end


   //-------------------------------------------------------------------------
   //  Satisfy write requests, if initialised.
   //-------------------------------------------------------------------------
   //  TODO: Byte enables.
   always @(posedge clock_i)
     if (active_o && request_i && write_i)
        mem[address_i] <= data_i;


endmodule // mcb_dummy
