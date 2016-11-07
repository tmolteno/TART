`timescale 1ns/100ps
/***************************************************************************
 *                                                                         *
 *   LUT6.v - Simulates the Xilinx (Virtex5) primitive of the same name    *
 *     for use with Icarus Verilog.                                        *
 *                                                                         *
 *   WILL NOT SYNTHESISE!                                                  *
 *                                                                         *
 *   Copyright (C) 2007 by Patrick Suggate                                 *
 *   patrick@physics.otago.ac.nz                                           *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

//----------------------------------------------------------------------------
//  The most general (and recommended) Xilinx LUT6 primitive.
//----------------------------------------------------------------------------
module LUT6
  #(parameter INIT  = 64'h0000_0000_0000_0000,
    parameter DELAY = 0.5)
   (
    output reg O,
    input      I0,
    input      I1,
    input      I2,
    input      I3,
    input      I4,
    input      I5
    );

   reg         lut_rom [0:63];
   integer     i;

   initial begin : Init
	    for (i=0; i<64; i=i+1)
		    lut_rom [i]	= INIT [i];
   end	// Init

   always @(I5, I4, I3, I2, I1, I0)
	   O <= #DELAY lut_rom[{I5, I4, I3, I2, I1, I0}];

endmodule	// LUT6


//----------------------------------------------------------------------------
//  Local output version of the LUT6 primitive.
//----------------------------------------------------------------------------
module LUT6_L
  #(parameter	INIT = 64'h0000_0000_0000_0000,
    parameter DELAY = 0.5)
   (
    output reg LO,
    input      I0,
    input      I1,
    input      I2,
    input      I3,
    input      I4,
    input      I5
    );

   reg         lut_rom[0:63];
   integer     i;

   initial begin : Init
	    for (i=0; i<64; i=i+1)
		    lut_rom[i] = INIT[i];
   end // Init

   always @(I5, I4, I3, I2, I1, I0)
	   LO <= #DELAY lut_rom [{I5, I4, I3, I2, I1, I0}];

endmodule	// LUT6_L


//----------------------------------------------------------------------------
//  Local and general outputs version of the LUT6 primitive.
//----------------------------------------------------------------------------
module LUT6_D
  #(parameter INIT  = 64'h0000_0000_0000_0000,
    parameter DELAY = 0.5)
   (
    output reg LO,
    output     O,
    input      I0,
    input      I1,
    input      I2,
    input      I3,
    input      I4,
    input      I5
    );

   reg     lut_rom [0:63];
   integer i;

   assign  O = LO;

   initial begin : Init
	    for (i=0; i<64; i=i+1)
		    lut_rom[i] = INIT[i];
   end // Init

   always @(I5, I4, I3, I2, I1, I0)
	   LO <= #DELAY lut_rom [{I5, I4, I3, I2, I1, I0}];

endmodule	// LUT6_D
