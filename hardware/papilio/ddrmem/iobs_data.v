/***************************************************************************
 *                                                                         *
 *   iobs_data.v - Instantiates the DDR flip-flops for a DDR controller.   *
 *     This file is Xilinx Spartan III specific.                           *
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

`timescale 1ns/100ps
module iobs_data (
	clock_i,
	
	send_i,
	capture_i,
	data_i,
	data_o,
	
	ddr_dq_io
);

parameter	IOBDELAY	= 4.6;

input	clock_i;

input	send_i;
input	[1:0]	capture_i;
input	[31:0]	data_i;
output	[31:0]	data_o;

inout	[15:0]	ddr_dq_io;


wire	[15:0]	ddr_dq_w;



/////////////////////////////////////////////////////////////////////////////
//  Output DDR flip-flops.
//

FDDRRSE fddr_dq0 (
	.Q	(ddr_dq_w [0]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [0]),
	.D1	(data_i [16]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq1 (
	.Q	(ddr_dq_w [1]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [1]),
	.D1	(data_i [17]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq2 (
	.Q	(ddr_dq_w [2]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [2]),
	.D1	(data_i [18]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq3 (
	.Q	(ddr_dq_w [3]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [3]),
	.D1	(data_i [19]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq4 (
	.Q	(ddr_dq_w [4]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [4]),
	.D1	(data_i [20]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq5 (
	.Q	(ddr_dq_w [5]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [5]),
	.D1	(data_i [21]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq6 (
	.Q	(ddr_dq_w [6]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [6]),
	.D1	(data_i [22]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq7 (
	.Q	(ddr_dq_w [7]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [7]),
	.D1	(data_i [23]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq8 (
	.Q	(ddr_dq_w [8]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [8]),
	.D1	(data_i [24]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq9 (
	.Q	(ddr_dq_w [9]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [9]),
	.D1	(data_i [25]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq10 (
	.Q	(ddr_dq_w [10]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [10]),
	.D1	(data_i [26]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq11 (
	.Q	(ddr_dq_w [11]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [11]),
	.D1	(data_i [27]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq12 (
	.Q	(ddr_dq_w [12]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [12]),
	.D1	(data_i [28]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq13 (
	.Q	(ddr_dq_w [13]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [13]),
	.D1	(data_i [29]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq14 (
	.Q	(ddr_dq_w [14]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [14]),
	.D1	(data_i [30]),
	.R	(0),
	.S	(0)
);

FDDRRSE fddr_dq15 (
	.Q	(ddr_dq_w [15]),
	.C0	(clock_i),
	.C1	(~clock_i),
	.CE	(1'b1),
	.D0	(data_i [15]),
	.D1	(data_i [31]),
	.R	(0),
	.S	(0)
);


OBUFT obuf_dq0 ( .I (ddr_dq_w [0]), .T (send_i), .O (ddr_dq_io [0]) );
OBUFT obuf_dq1 ( .I (ddr_dq_w [1]), .T (send_i), .O (ddr_dq_io [1]) );
OBUFT obuf_dq2 ( .I (ddr_dq_w [2]), .T (send_i), .O (ddr_dq_io [2]) );
OBUFT obuf_dq3 ( .I (ddr_dq_w [3]), .T (send_i), .O (ddr_dq_io [3]) );
OBUFT obuf_dq4 ( .I (ddr_dq_w [4]), .T (send_i), .O (ddr_dq_io [4]) );
OBUFT obuf_dq5 ( .I (ddr_dq_w [5]), .T (send_i), .O (ddr_dq_io [5]) );
OBUFT obuf_dq6 ( .I (ddr_dq_w [6]), .T (send_i), .O (ddr_dq_io [6]) );
OBUFT obuf_dq7 ( .I (ddr_dq_w [7]), .T (send_i), .O (ddr_dq_io [7]) );
OBUFT obuf_dq8 ( .I (ddr_dq_w [8]), .T (send_i), .O (ddr_dq_io [8]) );
OBUFT obuf_dq9 ( .I (ddr_dq_w [9]), .T (send_i), .O (ddr_dq_io [9]) );
OBUFT obuf_dq10 ( .I (ddr_dq_w [10]), .T (send_i), .O (ddr_dq_io [10]) );
OBUFT obuf_dq11 ( .I (ddr_dq_w [11]), .T (send_i), .O (ddr_dq_io [11]) );
OBUFT obuf_dq12 ( .I (ddr_dq_w [12]), .T (send_i), .O (ddr_dq_io [12]) );
OBUFT obuf_dq13 ( .I (ddr_dq_w [13]), .T (send_i), .O (ddr_dq_io [13]) );
OBUFT obuf_dq14 ( .I (ddr_dq_w [14]), .T (send_i), .O (ddr_dq_io [14]) );
OBUFT obuf_dq15 ( .I (ddr_dq_w [15]), .T (send_i), .O (ddr_dq_io [15]) );



/////////////////////////////////////////////////////////////////////////////
//  Input DDR flip-flops.
//

IFDDRRSE fddr_in0 (
	.C0	(capture_i [0]),
	.C1	(~capture_i [0]),
	.CE	(1'b1),
	.D	(ddr_dq_io [0]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [0]),
	.Q1	(data_o [16])
);

IFDDRRSE fddr_in1 (
	.C0	(capture_i [0]),
	.C1	(~capture_i [0]),
	.CE	(1'b1),
	.D	(ddr_dq_io [1]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [1]),
	.Q1	(data_o [17])
);

IFDDRRSE fddr_in2 (
	.C0	(capture_i [0]),
	.C1	(~capture_i [0]),
	.CE	(1'b1),
	.D	(ddr_dq_io [2]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [2]),
	.Q1	(data_o [18])
);

IFDDRRSE fddr_in3 (
	.C0	(capture_i [0]),
	.C1	(~capture_i [0]),
	.CE	(1'b1),
	.D	(ddr_dq_io [3]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [3]),
	.Q1	(data_o [19])
);

IFDDRRSE fddr_in4 (
	.C0	(capture_i [0]),
	.C1	(~capture_i [0]),
	.CE	(1'b1),
	.D	(ddr_dq_io [4]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [4]),
	.Q1	(data_o [20])
);

IFDDRRSE fddr_in5 (
	.C0	(capture_i [0]),
	.C1	(~capture_i [0]),
	.CE	(1'b1),
	.D	(ddr_dq_io [5]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [5]),
	.Q1	(data_o [21])
);

IFDDRRSE fddr_in6 (
	.C0	(capture_i [0]),
	.C1	(~capture_i [0]),
	.CE	(1'b1),
	.D	(ddr_dq_io [6]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [6]),
	.Q1	(data_o [22])
);

IFDDRRSE fddr_in7 (
	.C0	(capture_i [0]),
	.C1	(~capture_i [0]),
	.CE	(1'b1),
	.D	(ddr_dq_io [7]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [7]),
	.Q1	(data_o [23])
);

IFDDRRSE fddr_in8 (
	.C0	(capture_i [1]),
	.C1	(~capture_i [1]),
	.CE	(1'b1),
	.D	(ddr_dq_io [8]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [8]),
	.Q1	(data_o [24])
);

IFDDRRSE fddr_in9 (
	.C0	(capture_i [1]),
	.C1	(~capture_i [1]),
	.CE	(1'b1),
	.D	(ddr_dq_io [9]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [9]),
	.Q1	(data_o [25])
);

IFDDRRSE fddr_in10 (
	.C0	(capture_i [1]),
	.C1	(~capture_i [1]),
	.CE	(1'b1),
	.D	(ddr_dq_io [10]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [10]),
	.Q1	(data_o [26])
);

IFDDRRSE fddr_in11 (
	.C0	(capture_i [1]),
	.C1	(~capture_i [1]),
	.CE	(1'b1),
	.D	(ddr_dq_io [11]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [11]),
	.Q1	(data_o [27])
);

IFDDRRSE fddr_in12 (
	.C0	(capture_i [1]),
	.C1	(~capture_i [1]),
	.CE	(1'b1),
	.D	(ddr_dq_io [12]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [12]),
	.Q1	(data_o [28])
);

IFDDRRSE fddr_in13 (
	.C0	(capture_i [1]),
	.C1	(~capture_i [1]),
	.CE	(1'b1),
	.D	(ddr_dq_io [13]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [13]),
	.Q1	(data_o [29])
);

IFDDRRSE fddr_in14 (
	.C0	(capture_i [1]),
	.C1	(~capture_i [1]),
	.CE	(1'b1),
	.D	(ddr_dq_io [14]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [14]),
	.Q1	(data_o [30])
);

IFDDRRSE fddr_in15 (
	.C0	(capture_i [1]),
	.C1	(~capture_i [0]),
	.CE	(1'b1),
	.D	(ddr_dq_io [15]),
	.R	(1'b0),
	.S	(1'b0),
	.Q0	(data_o [15]),
	.Q1	(data_o [31])
);


endmodule	// iobs_data
