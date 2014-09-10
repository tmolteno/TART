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
module clk_dcm (input_clk,
        rst,
        clk,
        clk90,
        dcm_lock);

   input input_clk;
   input rst;
   output clk;
   output clk90;
   output dcm_lock; 

      // synthesis translate_off
       defparam DCM_INST1.DLL_FREQUENCY_MODE = "LOW";
       defparam DCM_INST1.DUTY_CYCLE_CORRECTION = "TRUE";
       // synthesis translate_on

   wire clk0dcm;
   wire clk90dcm;
   wire clk0_buf;
   wire clk90_buf;
   wire dcm1_lock;
   wire [7:0] status; 

   parameter GND = 1'b0;

   assign clk = clk0_buf;
   assign clk90 = clk90_buf;

   DCM DCM_INST1  
                  (         .CLKIN    (input_clk),
                            .CLKFB    (clk0_buf),
                            .DSSEN    (GND),
                            .PSINCDEC (GND),
                            .PSEN     (GND),
                            .PSCLK    (GND),
                            .RST      (rst),
                            .CLK0     (clk0dcm),
                            .CLK90    (clk90dcm),
                            .CLK180   (open),
                            .CLK270   (open),
		            .CLK2X    (open),
                            .CLK2X180 (open),
                            .CLKDV    (open),
                            .CLKFX    (open),
                            .CLKFX180 (open),
                            .LOCKED   (dcm1_lock),
                            .PSDONE   (open),
                            .STATUS   (status)
                    )
                    /* synthesis  DUTY_CYCLE_CORRECTION  = "TRUE",
                       DLL_FREQUENCY_MODE  = "LOW" */ ;


   mybufg BUFG_CLK0 (.I(clk0dcm), .O(clk0_buf));
   mybufg BUFG_CLK90 (.I(clk90dcm), .O(clk90_buf));

   assign dcm_lock = dcm1_lock;


endmodule 
                    
                          

