`timescale 1ns/100ps
/***************************************************************************
 *                                                                         *
 *   MUXF7.v - Simulates the Xilinx primitive of the same name for use     *
 *     with Icarus Verilog.                                                *
 *                                                                         *
 *   Copyright (C) 2005 by Patrick Suggate                                 *
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
 *                                                                         *
 ***************************************************************************/

//----------------------------------------------------------------------------
//  General (unspecified) output version of the Xilinx MUXF7 primitive.
//----------------------------------------------------------------------------
module MUXF7 #(parameter DELAY = 1.5)
   (
    output O,
	  input  I0,
	  input  I1,
	  input  S
    );

   assign	#DELAY O = S ? I1 : I0;

endmodule	//	MUXF7


//----------------------------------------------------------------------------
//  General and local outputs version of the Xilinx MUXF7 primitive.
//----------------------------------------------------------------------------
module MUXF7_D #(parameter DELAY = 1.5)
   (
	  output LO,
	  output O,
	  input  I0,
	  input  I1,
	  input  S
    );

   assign	#DELAY LO = S ? I1 : I0;
   assign	O = LO;

endmodule	//	MUXF7_D


//----------------------------------------------------------------------------
//  Local-output only version of the Xilinx MUXF7 primitive.
//----------------------------------------------------------------------------
module MUXF7_L #(parameter DELAY = 1.5)
   (
	  output LO,
	  input  I0,
	  input  I1,
	  input  S
    );

   assign	#DELAY LO = S ? I1 : I0;

endmodule	//	MUXF7_L
