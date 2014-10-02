//******************************************************************************
//
//  Xilinx, Inc. 2002                 www.xilinx.com
//
//
//*******************************************************************************
//
//  File name :       lfsr32.v
//
//  Description :     This module generates the user input data for hardware test 
//                    
//  Date - revision : 12/22/2003
//
//  Author :          Maria George
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

module lfsr32  (
	         clk,
	         rst,
	         lfsr_rst,
	         lfsr_ena,
	         lfsr_out
                );
               
   input          clk;
   input          rst;
   input          lfsr_rst;
   input          lfsr_ena;   

   output [143:0] lfsr_out;
   
   reg [15:0]     lfsr;
   wire [7:0]     counter_r;
   wire [7:0]     counter_f;
   wire [15:0]    counter;
   
assign counter_r = ((rst == 1'b1) || (lfsr_rst == 1'b1)) ? 8'b00000000 :
                   (lfsr_ena == 1'b1) ? (lfsr[15:8] + 1'b1) :
                   lfsr[15:8];
             
assign counter_f = ((rst == 1'b1) || (lfsr_rst == 1'b1)) ? 8'b00000000 :
                   (lfsr_ena == 1'b1) ? (lfsr[15:8] + 1'b1) :
                   lfsr[15:8]; 
             
assign counter  =  {counter_r, counter_f};

assign lfsr_out = {lfsr, lfsr, lfsr, lfsr, lfsr, lfsr, lfsr, lfsr, lfsr};                          
           

always @ (posedge clk)
begin
  if (rst == 1'b1)
   lfsr <= 16'h0000;
  else
   begin
   if (lfsr_rst == 1'b1)
     lfsr <= 16'h0000;
   else if (lfsr_ena == 1'b1)
     lfsr <= counter;
   else
     lfsr <= lfsr;
   end
end

endmodule