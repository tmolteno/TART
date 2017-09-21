/***************************************************************************
 *                                                                         *
 *   MFSR20.v - One of Roy Ward's ultra tricky Multiple Feed-back Shift    *
 *     Registers (MFSR). This one is 32-bits wide.                         *
 *                                                                         *
 *   Copyright (C) 2009 by Patrick Suggate and Roy Ward                    *
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

// Taps @: (28, 32), (21, 27)
// Taps @: (00, 01), (01, 28)        (added 20/08/2016, from ENZCon poster)

`timescale 1ns/100ps
module mfsr32
  #(parameter DELAY = 1)
   (
	  input [31:0]  count_i,
	  output [31:0] count_o
   );

   wire #DELAY tap_xor0 = count_i[0]^count_i[1];
   wire #DELAY tap_xor1 = count_i[1]^count_i[28];

   assign	count_o = {count_i[30:2], tap_xor1, tap_xor0, count_i[31]};

endmodule	// mfsr32
