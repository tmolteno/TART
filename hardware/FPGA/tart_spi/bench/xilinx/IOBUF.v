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
 * Simulation-only module implementing the functionality of the Spartan 6
 * primitive of the same name.
 * 
 * NOTE:
 *  + WILL NOT SYNTHESISE;
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
