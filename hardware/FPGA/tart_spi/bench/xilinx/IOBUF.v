`timescale 1ns/100ps
/*
 * Module      : bench/xilinx/IOBUF.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
 * 
 * Simulation-only module implementing the functionality of the Spartan 6
 * primitive of the same name.
 * 
 * NOTE:
 * 
 * TODO:
 *  + currently only a subset of the 'IOBUF' functionality is supported;
 * 
 */

module IOBUF
  #(parameter IOSTANDARD = "DEFAULT",
    parameter SLEW = "SLOW",
    parameter DRIVE = "12")
   (
    input  T,
    input  I,
    output O,
    inout  IO
    );

   assign IO = T ? 1'bz : I;
   assign O  = IO;

endmodule // IOBUF
