//#############################################################################//
//         Internal dqs delay structure for ddr sdram controller               //                          
//#############################################################################//
  `timescale 1ns/100ps
module tap_dly (
		    clk,  
		    reset,
		    tapIn,
		    flop2
		  );
input clk;
input reset;
input tapIn;
output [31:0]flop2;

wire [31:0]tap/*synthesis syn_keep =1 */;
wire [31:0]flop1/*synthesis syn_keep =1 */;
wire high;

assign high = 1'b1;   


LUT4 l0    ( .I0(high), .I1(high), .I2(high), .I3(tapIn), .O(tap[0]));

defparam l0 .INIT = 16'h0080;

LUT4 l1    ( .I0(tap[0]), .I1(high), .I2(high), .I3(high), .O(tap[1]));

defparam l1 .INIT = 16'h4000;

LUT4 l2    ( .I0(high), .I1(high), .I2(high), .I3(tap[1]), .O(tap[2]));

defparam l2 .INIT = 16'h0080;

LUT4 l3    ( .I0(high), .I1(high), .I2(tap[2]), .I3(high), .O(tap[3]));

defparam l3 .INIT = 16'h0800;

LUT4 l4    ( .I0(high), .I1(high), .I2(high), .I3(tap[3]), .O(tap[4]));

defparam l4 .INIT = 16'h0080;

LUT4 l5    ( .I0(high), .I1(high), .I2(tap[4]), .I3(high), .O(tap[5]));

defparam l5 .INIT = 16'h0800;

LUT4 l6    ( .I0(high), .I1(high), .I2(high), .I3(tap[5]), .O(tap[6]));

defparam l6 .INIT = 16'h0080;

LUT4 l7    ( .I0(tap[6]), .I1(high), .I2(high), .I3(high), .O(tap[7]));

defparam l7 .INIT = 16'h4000;

LUT4 l8    ( .I0(high), .I1(high), .I2(high), .I3(tap[7]), .O(tap[8]));

defparam l8 .INIT = 16'h0080;

LUT4 l9    ( .I0(tap[8]), .I1(high), .I2(high), .I3(high), .O(tap[9]));

defparam l9 .INIT = 16'h4000;

LUT4 l10    ( .I0(high), .I1(high), .I2(high), .I3(tap[9]), .O(tap[10]));

defparam l10 .INIT = 16'h0080;

LUT4 l11    ( .I0(high), .I1(high), .I2(tap[10]), .I3(high), .O(tap[11]));

defparam l11 .INIT = 16'h0800;

LUT4 l12    ( .I0(high), .I1(high), .I2(high), .I3(tap[11]), .O(tap[12]));

defparam l12 .INIT = 16'h0080;

LUT4 l13    ( .I0(high), .I1(high), .I2(tap[12]), .I3(high), .O(tap[13]));

defparam l13 .INIT = 16'h0800;


LUT4 l14    ( .I0(high), .I1(high), .I2(high), .I3(tap[13]), .O(tap[14]));

defparam l14 .INIT = 16'h0080;


LUT4 l15    ( .I0(tap[14]), .I1(high), .I2(high), .I3(high), .O(tap[15]));

defparam l15 .INIT = 16'h4000;

LUT4 l16    ( .I0(high), .I1(high), .I2(high), .I3(tap[15]), .O(tap[16]));

defparam l16 .INIT = 16'h0080;

LUT4 l17    ( .I0(high), .I1(high), .I2(tap[16]), .I3(high), .O(tap[17]));

defparam l17 .INIT = 16'h0800;

LUT4 l18    ( .I0(high), .I1(high), .I2(high), .I3(tap[17]), .O(tap[18]));

defparam l18 .INIT = 16'h0080;

LUT4 l19    ( .I0(high), .I1(high), .I2(tap[18]), .I3(high), .O(tap[19]));

defparam l19 .INIT = 16'h0800;

LUT4 l20    ( .I0(high), .I1(high), .I2(high), .I3(tap[19]), .O(tap[20]));

defparam l20 .INIT = 16'h0080;

LUT4 l21    ( .I0(high), .I1(high), .I2(high), .I3(tap[20]), .O(tap[21]));

defparam l21 .INIT = 16'h0080;

LUT4 l22    ( .I0(tap[21]), .I1(high), .I2(high), .I3(high), .O(tap[22]));

defparam l22 .INIT = 16'h4000;

LUT4 l23    ( .I0(high), .I1(high), .I2(high), .I3(tap[22]), .O(tap[23]));

defparam l23 .INIT = 16'h0080;

LUT4 l24    ( .I0(high), .I1(high), .I2(tap[23]), .I3(high), .O(tap[24]));

defparam l24 .INIT = 16'h0800;

LUT4 l25    ( .I0(high), .I1(high), .I2(high), .I3(tap[24]), .O(tap[25]));

defparam l25 .INIT = 16'h0080;

LUT4 l26    ( .I0(high), .I1(high), .I2(tap[25]), .I3(high), .O(tap[26]));

defparam l26 .INIT = 16'h0800;

LUT4 l27    ( .I0(high), .I1(high), .I2(high), .I3(tap[26]), .O(tap[27]));

defparam l27 .INIT = 16'h0080;

LUT4 l28    ( .I0(tap[27]), .I1(high), .I2(high), .I3(high), .O(tap[28]));

defparam l28 .INIT = 16'h4000;

LUT4 l29    ( .I0(high), .I1(high), .I2(high), .I3(tap[28]), .O(tap[29]));

defparam l29 .INIT = 16'h0080;

LUT4 l30    ( .I0(tap[29]), .I1(high), .I2(high), .I3(high), .O(tap[30]));

defparam l30 .INIT = 16'h4000;


LUT4 l31    ( .I0(high), .I1(high), .I2(high), .I3(tap[30]), .O(tap[31]));

defparam l31 .INIT = 16'h0080;


FDR r0 ( .Q(flop1[0]), .C(clk), .D(tap[0]), .R(reset) 	);

FDR r1 ( .Q(flop1[1]), .C(clk), .D(tap[1]), .R(reset) 	);

FDR r2 ( .Q(flop1[2]), .C(clk), .D(tap[2]), .R(reset) 	);

FDR r3 ( .Q(flop1[3]), .C(clk), .D(tap[3]), .R(reset) 	);

FDR r4 ( .Q(flop1[4]), .C(clk), .D(tap[4]), .R(reset) 	);

FDR r5 ( .Q(flop1[5]), .C(clk), .D(tap[5]), .R(reset) 	);

FDR r6 ( .Q(flop1[6]), .C(clk), .D(tap[6]), .R(reset) 	);

FDR r7 ( .Q(flop1[7]), .C(clk), .D(tap[7]), .R(reset) 	);

FDR r8 ( .Q(flop1[8]), .C(clk), .D(tap[8]), .R(reset) 	);

FDR r9 ( .Q(flop1[9]), .C(clk), .D(tap[9]), .R(reset) 	);

FDR r10 ( .Q(flop1[10]), .C(clk), .D(tap[10]), .R(reset) 	);

FDR r11 ( .Q(flop1[11]), .C(clk), .D(tap[11]), .R(reset) 	);

FDR r12 ( .Q(flop1[12]), .C(clk), .D(tap[12]), .R(reset) 	);

FDR r13 ( .Q(flop1[13]), .C(clk), .D(tap[13]), .R(reset) 	);

FDR r14 ( .Q(flop1[14]), .C(clk), .D(tap[14]), .R(reset) 	);

FDR r15 ( .Q(flop1[15]), .C(clk), .D(tap[15]), .R(reset) 	);

FDR r16 ( .Q(flop1[16]), .C(clk), .D(tap[16]), .R(reset) 	);

FDR r17 ( .Q(flop1[17]), .C(clk), .D(tap[17]), .R(reset) 	);

FDR r18 ( .Q(flop1[18]), .C(clk), .D(tap[18]), .R(reset) 	);

FDR r19 ( .Q(flop1[19]), .C(clk), .D(tap[19]), .R(reset) 	);

FDR r20 ( .Q(flop1[20]), .C(clk), .D(tap[20]), .R(reset) 	);

FDR r21 ( .Q(flop1[21]), .C(clk), .D(tap[21]), .R(reset) 	);

FDR r22 ( .Q(flop1[22]), .C(clk), .D(tap[22]), .R(reset) 	);

FDR r23 ( .Q(flop1[23]), .C(clk), .D(tap[23]), .R(reset) 	);

FDR r24 ( .Q(flop1[24]), .C(clk), .D(tap[24]), .R(reset) 	);

FDR r25 ( .Q(flop1[25]), .C(clk), .D(tap[25]), .R(reset) 	);

FDR r26 ( .Q(flop1[26]), .C(clk), .D(tap[26]), .R(reset) 	);

FDR r27 ( .Q(flop1[27]), .C(clk), .D(tap[27]), .R(reset) 	);

FDR r28 ( .Q(flop1[28]), .C(clk), .D(tap[28]), .R(reset) 	);

FDR r29 ( .Q(flop1[29]), .C(clk), .D(tap[29]), .R(reset) 	);

FDR r30 ( .Q(flop1[30]), .C(clk), .D(tap[30]), .R(reset) 	);

FDR r31 ( .Q(flop1[31]), .C(clk), .D(tap[31]), .R(reset) 	);


FDR u0  ( .Q(flop2[0]), .C(clk), .D(flop1[0]), .R(reset)  ); 	

FDR u1  ( .Q(flop2[1]), .C(clk), .D(flop1[1]), .R(reset)  ); 	

FDR u2  ( .Q(flop2[2]), .C(clk), .D(flop1[2]), .R(reset)  ); 	

FDR u3  ( .Q(flop2[3]), .C(clk), .D(flop1[3]), .R(reset)  ); 	

FDR u4  ( .Q(flop2[4]), .C(clk), .D(flop1[4]), .R(reset)  ); 	

FDR u5  ( .Q(flop2[5]), .C(clk), .D(flop1[5]), .R(reset)  ); 	

FDR u6  ( .Q(flop2[6]), .C(clk), .D(flop1[6]), .R(reset)  ); 	

FDR u7  ( .Q(flop2[7]), .C(clk), .D(flop1[7]), .R(reset)  ); 	

FDR u8  ( .Q(flop2[8]), .C(clk), .D(flop1[8]), .R(reset)  ); 	

FDR u9  ( .Q(flop2[9]), .C(clk), .D(flop1[9]), .R(reset)  ); 	

FDR u10  ( .Q(flop2[10]), .C(clk), .D(flop1[10]), .R(reset)  ); 	

FDR u11  ( .Q(flop2[11]), .C(clk), .D(flop1[11]), .R(reset)  ); 	

FDR u12  ( .Q(flop2[12]), .C(clk), .D(flop1[12]), .R(reset)  ); 	

FDR u13  ( .Q(flop2[13]), .C(clk), .D(flop1[13]), .R(reset)  ); 	

FDR u14  ( .Q(flop2[14]), .C(clk), .D(flop1[14]), .R(reset)  ); 	

FDR u15  ( .Q(flop2[15]), .C(clk), .D(flop1[15]), .R(reset)  ); 	

FDR u16  ( .Q(flop2[16]), .C(clk), .D(flop1[16]), .R(reset)  ); 	

FDR u17  ( .Q(flop2[17]), .C(clk), .D(flop1[17]), .R(reset)  ); 	

FDR u18  ( .Q(flop2[18]), .C(clk), .D(flop1[18]), .R(reset)  ); 	

FDR u19  ( .Q(flop2[19]), .C(clk), .D(flop1[19]), .R(reset)  ); 	

FDR u20  ( .Q(flop2[20]), .C(clk), .D(flop1[20]), .R(reset)  ); 	

FDR u21  ( .Q(flop2[21]), .C(clk), .D(flop1[21]), .R(reset)  ); 	

FDR u22  ( .Q(flop2[22]), .C(clk), .D(flop1[22]), .R(reset)  ); 	

FDR u23  ( .Q(flop2[23]), .C(clk), .D(flop1[23]), .R(reset)  ); 	

FDR u24  ( .Q(flop2[24]), .C(clk), .D(flop1[24]), .R(reset)  ); 	

FDR u25  ( .Q(flop2[25]), .C(clk), .D(flop1[25]), .R(reset)  ); 	

FDR u26  ( .Q(flop2[26]), .C(clk), .D(flop1[26]), .R(reset)  ); 	

FDR u27  ( .Q(flop2[27]), .C(clk), .D(flop1[27]), .R(reset)  ); 	

FDR u28  ( .Q(flop2[28]), .C(clk), .D(flop1[28]), .R(reset)  ); 	

FDR u29  ( .Q(flop2[29]), .C(clk), .D(flop1[29]), .R(reset)  ); 	

FDR u30  ( .Q(flop2[30]), .C(clk), .D(flop1[30]), .R(reset)  ); 	

FDR u31  ( .Q(flop2[31]), .C(clk), .D(flop1[31]), .R(reset)  ); 	

endmodule

