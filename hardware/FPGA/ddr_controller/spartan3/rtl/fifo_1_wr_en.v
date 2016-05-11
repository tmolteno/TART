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
module fifo_1_wr_en ( clk,
                      rst_dqs_delay_n,
                      reset,
                      din,
                      dout);

   input clk;
   input rst_dqs_delay_n;
   input reset;
   input din;
   output dout;


   wire din_delay;
   wire din_delay_1;
   wire dout0;
   wire rst_dqs_delay;

   parameter TIE_HIGH = 1'b1;

   assign rst_dqs_delay = ~rst_dqs_delay_n;
   assign dout0 = din & rst_dqs_delay_n;
   assign dout = rst_dqs_delay | din_delay_1;

   FDCE delay_ff_1 (.Q(din_delay_1), .C(clk), .CE(TIE_HIGH), .CLR(reset), .D(dout0));



endmodule 

