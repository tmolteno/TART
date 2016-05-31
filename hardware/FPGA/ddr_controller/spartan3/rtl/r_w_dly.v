//******************************************************************************
//
//  Xilinx, Inc. 2002                 www.xilinx.com
//
//
//*******************************************************************************
//
//  File name :       r_w_dly.v
//
//  Description :     This module issues read and write commands for hardware test
//                    
//  Date - revision : 12/22/2003
//
//  Author :          Maria George (converted to verilog)
//
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

module r_w_dly  (
	         clk90,
	         rst90,
                 clk0,
                 rst0,
	         dly_inc,
	         dly_tc,
                 r_w0, 
	         r_w
                );
               
   input          clk90;
   input          rst90;
   input          clk0;
   input          rst0;
   input          dly_inc;
   
   output         dly_tc;   
   output         r_w;
   output         r_w0; 
   
   reg [4:0]      delay_count;
   reg            delay_tc; /* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg            delay_tc1; /* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg            read_write;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg            read_write0; /* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   
assign dly_tc = delay_tc1;
assign r_w    = read_write;    
assign r_w0    = read_write0;  
   
always @ (posedge clk90)
begin
  if (rst90 == 1'b1)
    read_write <= 1'b0;
  else if (delay_tc == 1'b1)
    read_write <= ~read_write;
end

always @ (posedge clk0)
begin
  if (rst0 == 1'b1)
    read_write0 <= 1'b0;
  else if (delay_tc == 1'b1)
    read_write0 <= ~read_write0;
end
	
always @ (posedge clk90)
begin
  if (rst90 == 1'b1)
    delay_count <= 5'b00000;
  else if (dly_inc == 1'b1)
    delay_count <= delay_count + 1'b1 ;
end
	
always @ (posedge clk90)
begin
  if (rst90 == 1'b1)
    delay_tc <= 1'b0;
  else if (delay_count == 5'b11000) 
    delay_tc <= 1'b1;
  else 
    delay_tc <= 1'b0;
end


always @ (posedge clk0)
begin
  if (rst0 == 1'b1)
    delay_tc1 <= 1'b0;
  else if (delay_count == 5'b11000) 
    delay_tc1 <= 1'b1;
  else 
    delay_tc1 <= 1'b0;
end




   
endmodule   