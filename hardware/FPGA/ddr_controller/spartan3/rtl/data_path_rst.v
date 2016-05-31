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
module data_path_rst( clk90,
                      clk,
                      reset,
                      reset90,
                      reset180,
                      reset270,
                      reset_r,
                      reset90_r,
                      reset90_r1,
                      reset180_r,
                      reset270_r );

   input clk90;
   input reset;
   input reset90;
   input reset180;
   input reset270;
   input clk; 

   output reset_r;
   output reset90_r;
   output reset90_r1;
   output reset180_r;
   output reset270_r;
   
   wire  clk180 ;  
   wire clk270; 


   assign clk180 = ~ clk;
   assign clk270 = ~ clk90;


  FD rst0_r (.Q(reset_r), .C(clk),. D(reset));
  FD rst90_r (.Q(reset90_r), .C(clk90),. D(reset90));
  FD rst90_r1 (.Q(reset90_r1), .C(clk90),. D(reset90));
  FD rst180_r (.Q(reset180_r), .C(clk180),. D(reset180));
  FD rst270_r (.Q(reset270_r), .C(clk270),. D(reset270));

                      
endmodule 

















      
