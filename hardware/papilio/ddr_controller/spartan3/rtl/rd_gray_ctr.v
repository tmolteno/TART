//******************************************************************************
//
//  Xilinx, Inc. 2002                 www.xilinx.com
//
//
//*******************************************************************************
//  Contact : e-mail  hotline@xilinx.com
//            phone   + 1 800 255 7778 
//
//  Disclaimer: LIMITED WARRANTY AND DISCLAMER. These designs are 
//              provided to you "as is". Xilinx and its licensors make and you 
//              receive no warranties or conditions, express, implied, 
//              statutory or otherwise, and Xilinx specifically disclaims any 
//              implied warranties of merchantability, non-infringement, or 
//              fitness for a particular purpose. Xilinx does not warrant that 
//              the functions contained in these designs will meet your 
//              requirements, or that the operation of these designs will be 
//              uninterrupted or error free, or that defects in the Designs 
//              will be corrected. Furthermore, Xilinx does not warrant or 
//              make any representations regarding use or the results of the 
//              use of the designs in terms of correctness, accuracy, 
//              reliability, or otherwise. 
//
//              LIMITATION OF LIABILITY. In no event will Xilinx or its 
//              licensors be liable for any loss of data, lost profits, cost 
//              or procurement of substitute goods or services, or for any 
//              special, incidental, consequential, or indirect damages 
//              arising from the use or operation of the designs or 
//              accompanying documentation, however caused and on any theory 
//              of liability. This limitation will apply even if Xilinx 
//              has been advised of the possibility of such damage. This 
//              limitation shall apply not-withstanding the failure of the 
//              essential purpose of any limited remedies herein. 
//
//  Copyright © 2002 Xilinx, Inc.
//  All rights reserved 
// 
//*****************************************************************************

`timescale 1ns/100ps
module rd_gray_cntr(	clk, 				       
			reset,				
			cnt_en,			                              
			rgc_gcnt );
   input clk;
   input reset;
   input cnt_en;
   output [3:0] rgc_gcnt;


 
wire  [3:0]gc_int ;
reg  [3:0]d_in   ;  



assign    rgc_gcnt = gc_int;

always@(gc_int)
begin
case (gc_int)
4'b0000:  d_in <= 4'b0001;   //1
4'b0001:  d_in <= 4'b0011;   //3
4'b0010:  d_in <= 4'b0110;		//6
4'b0011:  d_in <= 4'b0010;  //2
4'b0100:  d_in <= 4'b1100;  //c
4'b0101:  d_in <= 4'b0100;  //4
4'b0110:  d_in <= 4'b0111;  //7
4'b0111:  d_in <= 4'b0101;  //5
4'b1000:  d_in <= 4'b0000;  //0
4'b1001:  d_in <= 4'b1000;  //8
4'b1010:  d_in <= 4'b1011;  //b
4'b1011:  d_in <= 4'b1001;  //9
4'b1100:  d_in <= 4'b1101;  //d
4'b1101:  d_in <= 4'b1111;  //f
4'b1110:  d_in <= 4'b1010;  //a
4'b1111:  d_in <= 4'b1110;  //e
default : d_in <= 4'b0001;  //1
endcase
end




 FDRE bit0 (
                      .Q( gc_int[0]),
                      .C(clk),
                      .CE(cnt_en),
                      .D(d_in[0]),
		      .R(reset)
                     );

FDRE bit1 (
                      .Q( gc_int[1]),
                      .C(clk),
                      .CE(cnt_en),
                      .D(d_in[1]),
		      .R(reset)
                     );

FDRE bit2 (
                      .Q( gc_int[2]),
                      .C(clk),
                      .CE(cnt_en),
                      .D(d_in[2]),
		      .R(reset)
                     );

FDRE bit3 (
                      .Q( gc_int[3]),
                      .C(clk),
                      .CE(cnt_en),
                      .D(d_in[3]),
		      .R(reset)
                     );



endmodule



