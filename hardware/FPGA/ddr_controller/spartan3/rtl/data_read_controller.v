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
module data_read_controller (     
     clk90,
     reset_r,
     reset90_r,
     rst_dqs_div_in,
     rst_dqs_div_in2,
     delay_sel,
     dqs_int_delay_in0,
     dqs_int_delay_in1,
     dqs_int_delay_in2,
     dqs_int_delay_in3,
     dqs_int_delay_in4,
     dqs_int_delay_in5,
     dqs_int_delay_in6,
     dqs_int_delay_in7,
     dqs_int_delay_in8,
     fifo0_rd_addr,
     fifo1_rd_addr,
     u_data_val,
     read_valid_data_1_val,
     fifo_00_wr_en_val,
     fifo_10_wr_en_val,	
     fifo_20_wr_en_val,	
     fifo_30_wr_en_val,
     fifo_40_wr_en_val,	
     fifo_50_wr_en_val,	
     fifo_60_wr_en_val,	
     fifo_70_wr_en_val,
     fifo_80_wr_en_val,
     fifo_01_wr_en_val,
     fifo_11_wr_en_val,	
     fifo_21_wr_en_val,	
     fifo_31_wr_en_val,
     fifo_41_wr_en_val,	
     fifo_51_wr_en_val,	
     fifo_61_wr_en_val,	
     fifo_71_wr_en_val,
     fifo_81_wr_en_val,
     fifo_00_wr_en_val_1,	
     fifo_10_wr_en_val_1,
     fifo_20_wr_en_val_1,
     fifo_30_wr_en_val_1,
     fifo_40_wr_en_val_1,
     fifo_50_wr_en_val_1,
     fifo_60_wr_en_val_1,
     fifo_70_wr_en_val_1,
     fifo_80_wr_en_val_1,
     fifo_01_wr_en_val_1,
     fifo_11_wr_en_val_1,
     fifo_21_wr_en_val_1,
     fifo_31_wr_en_val_1,
     fifo_41_wr_en_val_1,
     fifo_51_wr_en_val_1,
     fifo_61_wr_en_val_1,
     fifo_71_wr_en_val_1,
     fifo_81_wr_en_val_1,
     fifo_00_wr_addr_val_1,
     fifo_01_wr_addr_val_1,
     fifo_10_wr_addr_val_1,
     fifo_11_wr_addr_val_1,
     fifo_20_wr_addr_val_1,
     fifo_21_wr_addr_val_1,
     fifo_30_wr_addr_val_1,
     fifo_31_wr_addr_val_1,
     fifo_40_wr_addr_val_1,
     fifo_41_wr_addr_val_1,
     fifo_50_wr_addr_val_1,
     fifo_51_wr_addr_val_1,
     fifo_60_wr_addr_val_1,
     fifo_61_wr_addr_val_1,
     fifo_70_wr_addr_val_1,
     fifo_71_wr_addr_val_1,
     fifo_80_wr_addr_val_1,
     fifo_81_wr_addr_val_1,
     
     fifo_00_wr_addr_val,
     fifo_01_wr_addr_val,
     fifo_10_wr_addr_val,
     fifo_11_wr_addr_val,
     fifo_20_wr_addr_val,
     fifo_21_wr_addr_val,
     fifo_30_wr_addr_val,
     fifo_31_wr_addr_val,
     fifo_40_wr_addr_val,
     fifo_41_wr_addr_val,
     fifo_50_wr_addr_val,
     fifo_51_wr_addr_val,
     fifo_60_wr_addr_val,
     fifo_61_wr_addr_val,
     fifo_70_wr_addr_val,
     fifo_71_wr_addr_val,
     fifo_80_wr_addr_val,
     fifo_81_wr_addr_val,
     dqs0_delayed_col1_val,
     dqs1_delayed_col1_val,
     dqs2_delayed_col1_val,
     dqs3_delayed_col1_val,
     dqs4_delayed_col1_val,
     dqs5_delayed_col1_val,
     dqs6_delayed_col1_val,
     dqs7_delayed_col1_val,
     dqs8_delayed_col1_val,

     dqs0_delayed_col0_val,
     dqs1_delayed_col0_val,
     dqs2_delayed_col0_val,
     dqs3_delayed_col0_val,
     dqs4_delayed_col0_val,
     dqs5_delayed_col0_val,
     dqs6_delayed_col0_val,
     dqs7_delayed_col0_val,
     dqs8_delayed_col0_val);





     input    clk90;
     input     reset_r;
     input     reset90_r;
     input     rst_dqs_div_in;
     input     rst_dqs_div_in2; 
     input     [4:0] delay_sel;   
     input     dqs_int_delay_in0;
     input     dqs_int_delay_in1;
     input     dqs_int_delay_in2;
     input     dqs_int_delay_in3 ;
     input     dqs_int_delay_in4 ;
     input     dqs_int_delay_in5 ;
     input     dqs_int_delay_in6 ;
     input     dqs_int_delay_in7 ;
     input     dqs_int_delay_in8 ; 
     input     [3:0] fifo0_rd_addr ;
     input     [3:0] fifo1_rd_addr ;
					  
					  
     output     u_data_val;
     output     read_valid_data_1_val;    
     output     fifo_00_wr_en_val;	
     output     fifo_10_wr_en_val;	
     output     fifo_20_wr_en_val;	
     output     fifo_30_wr_en_val;
     output     fifo_40_wr_en_val;	
     output     fifo_50_wr_en_val;	
     output     fifo_60_wr_en_val;	
     output     fifo_70_wr_en_val;
     output     fifo_80_wr_en_val;
     output     fifo_01_wr_en_val;	
     output     fifo_11_wr_en_val;	
     output     fifo_21_wr_en_val;	
     output     fifo_31_wr_en_val;
     output     fifo_41_wr_en_val;	
     output     fifo_51_wr_en_val;	
     output     fifo_61_wr_en_val;	
     output     fifo_71_wr_en_val;
     output     fifo_81_wr_en_val;					  
     output     fifo_00_wr_en_val_1;	
     output     fifo_10_wr_en_val_1;	
     output     fifo_20_wr_en_val_1;	
     output     fifo_30_wr_en_val_1;
     output     fifo_40_wr_en_val_1;	
     output     fifo_50_wr_en_val_1;	
     output     fifo_60_wr_en_val_1;	
     output     fifo_70_wr_en_val_1;
     output     fifo_80_wr_en_val_1;
     output     fifo_01_wr_en_val_1;	
     output     fifo_11_wr_en_val_1;	
     output     fifo_21_wr_en_val_1;	
     output     fifo_31_wr_en_val_1;
     output     fifo_41_wr_en_val_1;	
     output     fifo_51_wr_en_val_1;	
     output     fifo_61_wr_en_val_1;	
     output     fifo_71_wr_en_val_1;
     output     fifo_81_wr_en_val_1;
     output     [3:0] fifo_00_wr_addr_val_1;
     output     [3:0] fifo_01_wr_addr_val_1;
     output     [3:0] fifo_10_wr_addr_val_1;
     output     [3:0] fifo_11_wr_addr_val_1;
     output     [3:0] fifo_20_wr_addr_val_1;
     output     [3:0] fifo_21_wr_addr_val_1;
     output     [3:0] fifo_30_wr_addr_val_1;
     output     [3:0] fifo_31_wr_addr_val_1;
     output     [3:0] fifo_40_wr_addr_val_1;
     output     [3:0] fifo_41_wr_addr_val_1;
     output     [3:0] fifo_50_wr_addr_val_1;
     output     [3:0] fifo_51_wr_addr_val_1;
     output     [3:0] fifo_60_wr_addr_val_1;
     output     [3:0] fifo_61_wr_addr_val_1;
     output     [3:0] fifo_70_wr_addr_val_1;
     output     [3:0] fifo_71_wr_addr_val_1;
     output     [3:0] fifo_80_wr_addr_val_1;
     output     [3:0] fifo_81_wr_addr_val_1;     				  
     output     [3:0] fifo_00_wr_addr_val;
     output     [3:0] fifo_01_wr_addr_val;
     output     [3:0] fifo_10_wr_addr_val;
     output     [3:0] fifo_11_wr_addr_val;
     output     [3:0] fifo_20_wr_addr_val;
     output     [3:0] fifo_21_wr_addr_val;
     output     [3:0] fifo_30_wr_addr_val;
     output     [3:0] fifo_31_wr_addr_val;
     output     [3:0] fifo_40_wr_addr_val;
     output     [3:0] fifo_41_wr_addr_val;
     output     [3:0] fifo_50_wr_addr_val;
     output     [3:0] fifo_51_wr_addr_val;
     output     [3:0] fifo_60_wr_addr_val;
     output     [3:0] fifo_61_wr_addr_val;
     output     [3:0] fifo_70_wr_addr_val;
     output     [3:0] fifo_71_wr_addr_val;
     output     [3:0] fifo_80_wr_addr_val;
     output     [3:0] fifo_81_wr_addr_val;     
     output     dqs0_delayed_col1_val;
     output     dqs1_delayed_col1_val;
     output     dqs2_delayed_col1_val;
     output     dqs3_delayed_col1_val;
     output     dqs4_delayed_col1_val;
     output     dqs5_delayed_col1_val;
     output     dqs6_delayed_col1_val;
     output     dqs7_delayed_col1_val;
     output     dqs8_delayed_col1_val;			  
     output     dqs0_delayed_col0_val;
     output     dqs1_delayed_col0_val;
     output     dqs2_delayed_col0_val;
     output     dqs3_delayed_col0_val;
     output     dqs4_delayed_col0_val;
     output     dqs5_delayed_col0_val;
     output     dqs6_delayed_col0_val;
     output     dqs7_delayed_col0_val;
     output     dqs8_delayed_col0_val; 


   wire [8:0] dqs_delayed_col0; 
   wire [8:0] dqs_delayed_col1; 

   wire fifo_00_empty;
   wire fifo_01_empty;

   wire [3:0] fifo_00_wr_addr;
   wire [3:0] fifo_01_wr_addr;
   wire [3:0] fifo_10_wr_addr;
   wire [3:0] fifo_11_wr_addr;
   wire [3:0] fifo_20_wr_addr;
   wire [3:0] fifo_21_wr_addr;
   wire [3:0] fifo_30_wr_addr;
   wire [3:0] fifo_31_wr_addr;
   wire [3:0] fifo_40_wr_addr;
   wire [3:0] fifo_41_wr_addr;
   wire [3:0] fifo_50_wr_addr;
   wire [3:0] fifo_51_wr_addr;
   wire [3:0] fifo_60_wr_addr;
   wire [3:0] fifo_61_wr_addr;
   wire [3:0] fifo_70_wr_addr;
   wire [3:0] fifo_71_wr_addr;
   wire [3:0] fifo_80_wr_addr;
   wire [3:0] fifo_81_wr_addr;
   wire [3:0] fifo_00_wr_addr_1;
   wire [3:0] fifo_01_wr_addr_1;
   wire [3:0] fifo_10_wr_addr_1;
   wire [3:0] fifo_11_wr_addr_1;
   wire [3:0] fifo_20_wr_addr_1;
   wire [3:0] fifo_21_wr_addr_1;
   wire [3:0] fifo_30_wr_addr_1;
   wire [3:0] fifo_31_wr_addr_1;
   wire [3:0] fifo_40_wr_addr_1;
   wire [3:0] fifo_41_wr_addr_1;
   wire [3:0] fifo_50_wr_addr_1;
   wire [3:0] fifo_51_wr_addr_1;
   wire [3:0] fifo_60_wr_addr_1;
   wire [3:0] fifo_61_wr_addr_1;
   wire [3:0] fifo_70_wr_addr_1;
   wire [3:0] fifo_71_wr_addr_1;
   wire [3:0] fifo_80_wr_addr_1;
   wire [3:0] fifo_81_wr_addr_1;

   wire read_valid_data_0_1;
   reg  read_valid_data_r; 
   reg  read_valid_data_r1; 
   wire dqs0_delayed_col0;
   wire dqs1_delayed_col0;
   wire dqs2_delayed_col0;
   wire dqs3_delayed_col0;
   wire dqs4_delayed_col0;
   wire dqs5_delayed_col0;
   wire dqs6_delayed_col0;
   wire dqs7_delayed_col0;
   wire dqs8_delayed_col0;
   wire dqs0_delayed_col1;
   wire dqs1_delayed_col1;
   wire dqs2_delayed_col1;
   wire dqs3_delayed_col1;
   wire dqs4_delayed_col1;
   wire dqs5_delayed_col1;
   wire dqs6_delayed_col1;
   wire dqs7_delayed_col1;
   wire dqs8_delayed_col1;
   wire fifo_00_wr_en;
   wire fifo_10_wr_en;
   wire fifo_20_wr_en;
   wire fifo_30_wr_en;
   wire fifo_40_wr_en;
   wire fifo_50_wr_en;
   wire fifo_60_wr_en;
   wire fifo_70_wr_en;
   wire fifo_80_wr_en;
   wire fifo_01_wr_en;
   wire fifo_11_wr_en;
   wire fifo_21_wr_en;
   wire fifo_31_wr_en;
   wire fifo_41_wr_en;
   wire fifo_51_wr_en;
   wire fifo_61_wr_en;
   wire fifo_71_wr_en;
   wire fifo_81_wr_en;
   wire fifo_00_wr_en_1;
   wire fifo_10_wr_en_1;
   wire fifo_20_wr_en_1;
   wire fifo_30_wr_en_1;
   wire fifo_40_wr_en_1;
   wire fifo_50_wr_en_1;
   wire fifo_60_wr_en_1;
   wire fifo_70_wr_en_1;
   wire fifo_80_wr_en_1;
   wire fifo_01_wr_en_1;
   wire fifo_11_wr_en_1;
   wire fifo_21_wr_en_1;
   wire fifo_31_wr_en_1;
   wire fifo_41_wr_en_1;
   wire fifo_51_wr_en_1;
   wire fifo_61_wr_en_1;
   wire fifo_71_wr_en_1;
   wire fifo_81_wr_en_1;
   reg [3:0] fifo_00_wr_addr_d;
   reg [3:0] fifo_00_wr_addr_2d;
   reg [3:0] fifo_00_wr_addr_3d;
   reg [3:0] fifo_01_wr_addr_d;
   reg [3:0] fifo_01_wr_addr_2d;
   reg [3:0] fifo_01_wr_addr_3d;
   wire [71:0] ddr_dq_in;
   wire [71:0] write_data270_1;
   wire [71:0] write_data270_2;
   wire rst_dqs_div;
   wire rst_dqs_div2; 
   wire rst_dqs_delay_0_n;
   wire rst_dqs_delay_1_n;
   wire rst_dqs_delay_2_n;
   wire rst_dqs_delay_3_n;
   wire rst_dqs_delay_4_n;
   wire rst_dqs_delay_5_n;
   wire rst_dqs_delay_6_n;
   wire rst_dqs_delay_7_n;
   wire rst_dqs_delay_8_n;
   wire rst_dqs_delay_0_n_1;
   wire rst_dqs_delay_1_n_1;
   wire rst_dqs_delay_2_n_1;
   wire rst_dqs_delay_3_n_1;
   wire rst_dqs_delay_4_n_1;
   wire rst_dqs_delay_5_n_1;
   wire rst_dqs_delay_6_n_1;
   wire rst_dqs_delay_7_n_1;
   wire rst_dqs_delay_8_n_1;
   wire dqs0_delayed_col0_n;
   wire dqs1_delayed_col0_n;
   wire dqs2_delayed_col0_n;
   wire dqs3_delayed_col0_n;
   wire dqs4_delayed_col0_n;
   wire dqs5_delayed_col0_n;
   wire dqs6_delayed_col0_n;
   wire dqs7_delayed_col0_n;
   wire dqs8_delayed_col0_n;
   wire dqs0_delayed_col1_n;
   wire dqs1_delayed_col1_n;
   wire dqs2_delayed_col1_n;
   wire dqs3_delayed_col1_n;
   wire dqs4_delayed_col1_n;
   wire dqs5_delayed_col1_n;
   wire dqs6_delayed_col1_n;
   wire dqs7_delayed_col1_n;
   wire dqs8_delayed_col1_n;
   reg  u_data_val_r;

   
   assign dqs0_delayed_col0 = dqs_delayed_col0[0];
   assign dqs1_delayed_col0 = dqs_delayed_col0[1];
   assign dqs2_delayed_col0 = dqs_delayed_col0[2];
   assign dqs3_delayed_col0 = dqs_delayed_col0[3];
   assign dqs4_delayed_col0 = dqs_delayed_col0[4];
   assign dqs5_delayed_col0 = dqs_delayed_col0[5];
   assign dqs6_delayed_col0 = dqs_delayed_col0[6];
   assign dqs7_delayed_col0 = dqs_delayed_col0[7];
   assign dqs8_delayed_col0 = dqs_delayed_col0[8];

   assign dqs0_delayed_col1 = dqs_delayed_col1[0];
   assign dqs1_delayed_col1 = dqs_delayed_col1[1];
   assign dqs2_delayed_col1 = dqs_delayed_col1[2];
   assign dqs3_delayed_col1 = dqs_delayed_col1[3];
   assign dqs4_delayed_col1 = dqs_delayed_col1[4];
   assign dqs5_delayed_col1 = dqs_delayed_col1[5];
   assign dqs6_delayed_col1 = dqs_delayed_col1[6];
   assign dqs7_delayed_col1 = dqs_delayed_col1[7];
   assign dqs8_delayed_col1 = dqs_delayed_col1[8];

   assign fifo_00_wr_addr_val = fifo_00_wr_addr; 
   assign fifo_01_wr_addr_val = fifo_01_wr_addr; 
   assign fifo_10_wr_addr_val = fifo_10_wr_addr; 
   assign fifo_11_wr_addr_val = fifo_11_wr_addr; 
   assign fifo_20_wr_addr_val = fifo_20_wr_addr; 
   assign fifo_21_wr_addr_val = fifo_21_wr_addr; 
   assign fifo_30_wr_addr_val = fifo_30_wr_addr; 
   assign fifo_31_wr_addr_val = fifo_31_wr_addr; 
   assign fifo_40_wr_addr_val = fifo_40_wr_addr; 
   assign fifo_41_wr_addr_val = fifo_41_wr_addr; 
   assign fifo_50_wr_addr_val = fifo_50_wr_addr; 
   assign fifo_51_wr_addr_val = fifo_51_wr_addr; 
   assign fifo_60_wr_addr_val = fifo_60_wr_addr; 
   assign fifo_61_wr_addr_val = fifo_61_wr_addr; 
   assign fifo_70_wr_addr_val = fifo_70_wr_addr; 
   assign fifo_71_wr_addr_val = fifo_71_wr_addr; 
   assign fifo_80_wr_addr_val = fifo_80_wr_addr; 
   assign fifo_81_wr_addr_val = fifo_81_wr_addr;

   assign fifo_00_wr_addr_val_1 = fifo_00_wr_addr_1; 
   assign fifo_01_wr_addr_val_1 = fifo_01_wr_addr_1; 
   assign fifo_10_wr_addr_val_1 = fifo_10_wr_addr_1; 
   assign fifo_11_wr_addr_val_1 = fifo_11_wr_addr_1; 
   assign fifo_20_wr_addr_val_1 = fifo_20_wr_addr_1; 
   assign fifo_21_wr_addr_val_1 = fifo_21_wr_addr_1; 
   assign fifo_30_wr_addr_val_1 = fifo_30_wr_addr_1; 
   assign fifo_31_wr_addr_val_1 = fifo_31_wr_addr_1; 
   assign fifo_40_wr_addr_val_1 = fifo_40_wr_addr_1; 
   assign fifo_41_wr_addr_val_1 = fifo_41_wr_addr_1; 
   assign fifo_50_wr_addr_val_1 = fifo_50_wr_addr_1; 
   assign fifo_51_wr_addr_val_1 = fifo_51_wr_addr_1; 
   assign fifo_60_wr_addr_val_1 = fifo_60_wr_addr_1; 
   assign fifo_61_wr_addr_val_1 = fifo_61_wr_addr_1; 
   assign fifo_70_wr_addr_val_1 = fifo_70_wr_addr_1; 
   assign fifo_71_wr_addr_val_1 = fifo_71_wr_addr_1; 
   assign fifo_80_wr_addr_val_1 = fifo_80_wr_addr_1; 
   assign fifo_81_wr_addr_val_1 = fifo_81_wr_addr_1; 

   assign fifo_00_wr_en_val   =	fifo_00_wr_en;
   assign fifo_10_wr_en_val   =	fifo_10_wr_en;
   assign fifo_20_wr_en_val   =	fifo_20_wr_en;
   assign fifo_30_wr_en_val   =	fifo_30_wr_en;
   assign fifo_40_wr_en_val   =	fifo_40_wr_en;
   assign fifo_50_wr_en_val   =	fifo_50_wr_en;
   assign fifo_60_wr_en_val   =	fifo_60_wr_en;
   assign fifo_70_wr_en_val   =	fifo_70_wr_en;
   assign fifo_80_wr_en_val   =	fifo_80_wr_en;
 
   assign fifo_01_wr_en_val   =	fifo_01_wr_en;
   assign fifo_11_wr_en_val   =	fifo_11_wr_en;
   assign fifo_21_wr_en_val   =	fifo_21_wr_en;
   assign fifo_31_wr_en_val   =	fifo_31_wr_en;
   assign fifo_41_wr_en_val   =	fifo_41_wr_en;
   assign fifo_51_wr_en_val   =	fifo_51_wr_en;
   assign fifo_61_wr_en_val   =	fifo_61_wr_en;
   assign fifo_71_wr_en_val   =	fifo_71_wr_en;
   assign fifo_81_wr_en_val   =	fifo_81_wr_en;

   assign fifo_00_wr_en_val_1   =	fifo_00_wr_en_1;
   assign fifo_10_wr_en_val_1   =	fifo_10_wr_en_1;
   assign fifo_20_wr_en_val_1   =	fifo_20_wr_en_1;
   assign fifo_30_wr_en_val_1   =	fifo_30_wr_en_1;
   assign fifo_40_wr_en_val_1   =	fifo_40_wr_en_1;
   assign fifo_50_wr_en_val_1   =	fifo_50_wr_en_1;
   assign fifo_60_wr_en_val_1   =	fifo_60_wr_en_1;
   assign fifo_70_wr_en_val_1   =	fifo_70_wr_en_1;
   assign fifo_80_wr_en_val_1   =	fifo_80_wr_en_1;
 
   assign fifo_01_wr_en_val_1   =	fifo_01_wr_en_1;
   assign fifo_11_wr_en_val_1   =	fifo_11_wr_en_1;
   assign fifo_21_wr_en_val_1   =	fifo_21_wr_en_1;
   assign fifo_31_wr_en_val_1   =	fifo_31_wr_en_1;
   assign fifo_41_wr_en_val_1   =	fifo_41_wr_en_1;
   assign fifo_51_wr_en_val_1   =	fifo_51_wr_en_1;
   assign fifo_61_wr_en_val_1   =	fifo_61_wr_en_1;
   assign fifo_71_wr_en_val_1   =	fifo_71_wr_en_1;
   assign fifo_81_wr_en_val_1   =	fifo_81_wr_en_1;


   assign dqs0_delayed_col1_val = dqs0_delayed_col1;
   assign dqs1_delayed_col1_val = dqs1_delayed_col1;
   assign dqs2_delayed_col1_val = dqs2_delayed_col1;
   assign dqs3_delayed_col1_val = dqs3_delayed_col1; 
   assign dqs4_delayed_col1_val = dqs4_delayed_col1;
   assign dqs5_delayed_col1_val = dqs5_delayed_col1;
   assign dqs6_delayed_col1_val = dqs6_delayed_col1;
   assign dqs7_delayed_col1_val = dqs7_delayed_col1;
   assign dqs8_delayed_col1_val = dqs8_delayed_col1;



   assign dqs0_delayed_col0_val = dqs0_delayed_col0;
   assign dqs1_delayed_col0_val = dqs1_delayed_col0;
   assign dqs2_delayed_col0_val = dqs2_delayed_col0;
   assign dqs3_delayed_col0_val = dqs3_delayed_col0; 
   assign dqs4_delayed_col0_val = dqs4_delayed_col0;
   assign dqs5_delayed_col0_val = dqs5_delayed_col0;
   assign dqs6_delayed_col0_val = dqs6_delayed_col0;
   assign dqs7_delayed_col0_val = dqs7_delayed_col0;
   assign dqs8_delayed_col0_val = dqs8_delayed_col0;

   assign dqs0_delayed_col0_n = ~ dqs0_delayed_col0;
   assign dqs1_delayed_col0_n = ~ dqs1_delayed_col0;
   assign dqs2_delayed_col0_n = ~ dqs2_delayed_col0;
   assign dqs3_delayed_col0_n = ~ dqs3_delayed_col0;
   assign dqs4_delayed_col0_n = ~ dqs4_delayed_col0;
   assign dqs5_delayed_col0_n = ~ dqs5_delayed_col0;
   assign dqs6_delayed_col0_n = ~ dqs6_delayed_col0;
   assign dqs7_delayed_col0_n = ~ dqs7_delayed_col0;
   assign dqs8_delayed_col0_n = ~ dqs8_delayed_col0;

   assign dqs0_delayed_col1_n = ~ dqs0_delayed_col1;
   assign dqs1_delayed_col1_n = ~ dqs1_delayed_col1;
   assign dqs2_delayed_col1_n = ~ dqs2_delayed_col1;
   assign dqs3_delayed_col1_n = ~ dqs3_delayed_col1;
   assign dqs4_delayed_col1_n = ~ dqs4_delayed_col1;
   assign dqs5_delayed_col1_n = ~ dqs5_delayed_col1;
   assign dqs6_delayed_col1_n = ~ dqs6_delayed_col1;
   assign dqs7_delayed_col1_n = ~ dqs7_delayed_col1;
   assign dqs8_delayed_col1_n = ~ dqs8_delayed_col1;

   assign fifo_00_empty = (fifo0_rd_addr == fifo_00_wr_addr_3d) ? 1'b1 : 1'b0;
   assign fifo_01_empty = (fifo1_rd_addr == fifo_01_wr_addr_3d) ? 1'b1 : 1'b0;

   assign read_valid_data_0_1 = ((~fifo_00_empty) & (~fifo_01_empty));
   assign read_valid_data_1_val = (read_valid_data_0_1);
   assign u_data_val = u_data_val_r; 

   

   
   always@(posedge clk90)begin
      if(reset90_r)begin
         fifo_00_wr_addr_d <= 4'd0;
         fifo_01_wr_addr_d <= 4'd0;
         fifo_00_wr_addr_2d <= 4'd0;
         fifo_01_wr_addr_2d <= 4'd0;
         fifo_00_wr_addr_3d <= 4'd0;
         fifo_01_wr_addr_3d <= 4'd0;
      end
      else begin
         fifo_00_wr_addr_d <= fifo_00_wr_addr;
         fifo_01_wr_addr_d <= fifo_01_wr_addr;
         fifo_00_wr_addr_2d <= fifo_00_wr_addr_d;
         fifo_01_wr_addr_2d <= fifo_01_wr_addr_d;
         fifo_00_wr_addr_3d <= fifo_00_wr_addr_2d;
         fifo_01_wr_addr_3d <= fifo_01_wr_addr_2d;
      end
   end


   always@(posedge clk90)begin
      if(reset90_r)begin
         u_data_val_r <= 1'b0;
         read_valid_data_r <= 1'b0;
         read_valid_data_r1 <= 1'b0;
      end
      else begin
         read_valid_data_r <= read_valid_data_0_1;
         read_valid_data_r1 <= read_valid_data_r;
         u_data_val_r <= read_valid_data_r1;
      end
    end

   // rst_dqs_div instantation. 

   dqs_delay rst_dqs_div_delayed1 (.clk_in(rst_dqs_div_in), .sel_in(delay_sel), .clk_out(rst_dqs_div));
   dqs_delay rst_dqs_div_delayed2 (.clk_in(rst_dqs_div_in2), .sel_in(delay_sel), .clk_out(rst_dqs_div2));


  //DQS Internal Delay Circuit implemented in LUTs

   dqs_delay dqs_delay0_col0(.clk_in(dqs_int_delay_in0), .sel_in(delay_sel), .clk_out(dqs_delayed_col0[0]));
   dqs_delay dqs_delay0_col1(.clk_in(dqs_int_delay_in0), .sel_in(delay_sel), .clk_out(dqs_delayed_col1[0]));
   dqs_delay dqs_delay1_col0(.clk_in(dqs_int_delay_in1), .sel_in(delay_sel), .clk_out(dqs_delayed_col0[1]));
   dqs_delay dqs_delay1_col1(.clk_in(dqs_int_delay_in1), .sel_in(delay_sel), .clk_out(dqs_delayed_col1[1]));
   dqs_delay dqs_delay2_col0(.clk_in(dqs_int_delay_in2), .sel_in(delay_sel), .clk_out(dqs_delayed_col0[2]));
   dqs_delay dqs_delay2_col1(.clk_in(dqs_int_delay_in2), .sel_in(delay_sel), .clk_out(dqs_delayed_col1[2]));
   dqs_delay dqs_delay3_col0(.clk_in(dqs_int_delay_in3), .sel_in(delay_sel), .clk_out(dqs_delayed_col0[3]));
   dqs_delay dqs_delay3_col1(.clk_in(dqs_int_delay_in3), .sel_in(delay_sel), .clk_out(dqs_delayed_col1[3]));
   dqs_delay dqs_delay4_col0(.clk_in(dqs_int_delay_in4), .sel_in(delay_sel), .clk_out(dqs_delayed_col0[4]));
   dqs_delay dqs_delay4_col1(.clk_in(dqs_int_delay_in4), .sel_in(delay_sel), .clk_out(dqs_delayed_col1[4]));
   dqs_delay dqs_delay5_col0(.clk_in(dqs_int_delay_in5), .sel_in(delay_sel), .clk_out(dqs_delayed_col0[5]));
   dqs_delay dqs_delay5_col1(.clk_in(dqs_int_delay_in5), .sel_in(delay_sel), .clk_out(dqs_delayed_col1[5]));
   dqs_delay dqs_delay6_col0(.clk_in(dqs_int_delay_in6), .sel_in(delay_sel), .clk_out(dqs_delayed_col0[6]));
   dqs_delay dqs_delay6_col1(.clk_in(dqs_int_delay_in6), .sel_in(delay_sel), .clk_out(dqs_delayed_col1[6]));
   dqs_delay dqs_delay7_col0(.clk_in(dqs_int_delay_in7), .sel_in(delay_sel), .clk_out(dqs_delayed_col0[7]));
   dqs_delay dqs_delay7_col1(.clk_in(dqs_int_delay_in7), .sel_in(delay_sel), .clk_out(dqs_delayed_col1[7]));
   dqs_delay dqs_delay8_col0(.clk_in(dqs_int_delay_in8), .sel_in(delay_sel), .clk_out(dqs_delayed_col0[8]));
   dqs_delay dqs_delay8_col1(.clk_in(dqs_int_delay_in8), .sel_in(delay_sel), .clk_out(dqs_delayed_col1[8]));

   // FIFO write enables
   
   fifo_0_wr_en fifo_00_wr_en_inst (.clk(dqs0_delayed_col1_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_0_n), .dout(fifo_00_wr_en));
   fifo_0_wr_en fifo_00_wr_en_inst_1 (.clk(dqs0_delayed_col0_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_0_n_1), .dout(fifo_00_wr_en_1));
   fifo_1_wr_en fifo_01_wr_en_inst (.clk(dqs0_delayed_col0), .rst_dqs_delay_n(rst_dqs_delay_0_n),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_01_wr_en));
   fifo_1_wr_en fifo_01_wr_en_inst_1 (.clk(dqs0_delayed_col1), .rst_dqs_delay_n(rst_dqs_delay_0_n_1),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_01_wr_en_1));
   fifo_0_wr_en fifo_10_wr_en_inst (.clk(dqs1_delayed_col1_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_1_n), .dout(fifo_10_wr_en));
   fifo_0_wr_en fifo_10_wr_en_inst_1 (.clk(dqs1_delayed_col0_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_1_n_1), .dout(fifo_10_wr_en_1));
   fifo_1_wr_en fifo_11_wr_en_inst (.clk(dqs1_delayed_col0), .rst_dqs_delay_n(rst_dqs_delay_1_n),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_11_wr_en));
   fifo_1_wr_en fifo_11_wr_en_inst_1 (.clk(dqs1_delayed_col1), .rst_dqs_delay_n(rst_dqs_delay_1_n_1),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_11_wr_en_1));  
   fifo_0_wr_en fifo_20_wr_en_inst (.clk(dqs2_delayed_col1_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_2_n), .dout(fifo_20_wr_en));
   fifo_0_wr_en fifo_20_wr_en_inst_1 (.clk(dqs2_delayed_col0_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_2_n_1), .dout(fifo_20_wr_en_1));
   fifo_1_wr_en fifo_21_wr_en_inst (.clk(dqs2_delayed_col0), .rst_dqs_delay_n(rst_dqs_delay_2_n),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_21_wr_en));
   fifo_1_wr_en fifo_21_wr_en_inst_1 (.clk(dqs2_delayed_col1), .rst_dqs_delay_n(rst_dqs_delay_2_n_1),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_21_wr_en_1));     
   fifo_0_wr_en fifo_30_wr_en_inst (.clk(dqs3_delayed_col1_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_3_n), .dout(fifo_30_wr_en));
   fifo_0_wr_en fifo_30_wr_en_inst_1 (.clk(dqs3_delayed_col0_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_3_n_1), .dout(fifo_30_wr_en_1));
   fifo_1_wr_en fifo_31_wr_en_inst (.clk(dqs3_delayed_col0), .rst_dqs_delay_n(rst_dqs_delay_3_n),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_31_wr_en));
   fifo_1_wr_en fifo_31_wr_en_inst_1 (.clk(dqs3_delayed_col1), .rst_dqs_delay_n(rst_dqs_delay_3_n_1),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_31_wr_en_1));   
   fifo_0_wr_en fifo_40_wr_en_inst (.clk(dqs4_delayed_col1_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_4_n), .dout(fifo_40_wr_en));
   fifo_0_wr_en fifo_40_wr_en_inst_1 (.clk(dqs4_delayed_col0_n), .reset(reset_r), .din(rst_dqs_div),
                                    .rst_dqs_delay_n(rst_dqs_delay_4_n_1), .dout(fifo_40_wr_en_1));
   fifo_1_wr_en fifo_41_wr_en_inst (.clk(dqs4_delayed_col0), .rst_dqs_delay_n(rst_dqs_delay_4_n),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_41_wr_en));
   fifo_1_wr_en fifo_41_wr_en_inst_1 (.clk(dqs4_delayed_col1), .rst_dqs_delay_n(rst_dqs_delay_4_n_1),
                                    .reset(reset_r), .din(rst_dqs_div), .dout(fifo_41_wr_en_1));  
   fifo_0_wr_en fifo_50_wr_en_inst (.clk(dqs5_delayed_col1_n), .reset(reset_r), .din(rst_dqs_div2),
                                    .rst_dqs_delay_n(rst_dqs_delay_5_n), .dout(fifo_50_wr_en));
   fifo_0_wr_en fifo_50_wr_en_inst_1 (.clk(dqs5_delayed_col0_n), .reset(reset_r), .din(rst_dqs_div2),
                                    .rst_dqs_delay_n(rst_dqs_delay_5_n_1), .dout(fifo_50_wr_en_1));
   fifo_1_wr_en fifo_51_wr_en_inst (.clk(dqs5_delayed_col0), .rst_dqs_delay_n(rst_dqs_delay_5_n),
                                    .reset(reset_r), .din(rst_dqs_div2), .dout(fifo_51_wr_en));
   fifo_1_wr_en fifo_51_wr_en_inst_1 (.clk(dqs5_delayed_col1), .rst_dqs_delay_n(rst_dqs_delay_5_n_1),
                                    .reset(reset_r), .din(rst_dqs_div2), .dout(fifo_51_wr_en_1));
   fifo_0_wr_en fifo_60_wr_en_inst (.clk(dqs6_delayed_col1_n), .reset(reset_r), .din(rst_dqs_div2),
                                    .rst_dqs_delay_n(rst_dqs_delay_6_n), .dout(fifo_60_wr_en));
   fifo_0_wr_en fifo_60_wr_en_inst_1 (.clk(dqs6_delayed_col0_n), .reset(reset_r), .din(rst_dqs_div2),
                                    .rst_dqs_delay_n(rst_dqs_delay_6_n_1), .dout(fifo_60_wr_en_1));
   fifo_1_wr_en fifo_61_wr_en_inst (.clk(dqs6_delayed_col0), .rst_dqs_delay_n(rst_dqs_delay_6_n),
                                    .reset(reset_r), .din(rst_dqs_div2), .dout(fifo_61_wr_en));
   fifo_1_wr_en fifo_61_wr_en_inst_1 (.clk(dqs6_delayed_col1), .rst_dqs_delay_n(rst_dqs_delay_6_n_1),
                                    .reset(reset_r), .din(rst_dqs_div2), .dout(fifo_61_wr_en_1));
   fifo_0_wr_en fifo_70_wr_en_inst (.clk(dqs7_delayed_col1_n), .reset(reset_r), .din(rst_dqs_div2),
                                    .rst_dqs_delay_n(rst_dqs_delay_7_n), .dout(fifo_70_wr_en));
   fifo_0_wr_en fifo_70_wr_en_inst_1 (.clk(dqs7_delayed_col0_n), .reset(reset_r), .din(rst_dqs_div2),
                                    .rst_dqs_delay_n(rst_dqs_delay_7_n_1), .dout(fifo_70_wr_en_1));
   fifo_1_wr_en fifo_71_wr_en_inst (.clk(dqs7_delayed_col0), .rst_dqs_delay_n(rst_dqs_delay_7_n),
                                    .reset(reset_r), .din(rst_dqs_div2), .dout(fifo_71_wr_en));
   fifo_1_wr_en fifo_71_wr_en_inst_1 (.clk(dqs7_delayed_col1), .rst_dqs_delay_n(rst_dqs_delay_7_n_1),
                                    .reset(reset_r), .din(rst_dqs_div2), .dout(fifo_71_wr_en_1));
   fifo_0_wr_en fifo_80_wr_en_inst (.clk(dqs8_delayed_col1_n), .reset(reset_r), .din(rst_dqs_div2),
                                    .rst_dqs_delay_n(rst_dqs_delay_8_n), .dout(fifo_80_wr_en));
   fifo_0_wr_en fifo_80_wr_en_inst_1 (.clk(dqs8_delayed_col0_n), .reset(reset_r), .din(rst_dqs_div2),
                                    .rst_dqs_delay_n(rst_dqs_delay_8_n_1), .dout(fifo_80_wr_en_1));
   fifo_1_wr_en fifo_81_wr_en_inst (.clk(dqs8_delayed_col0), .rst_dqs_delay_n(rst_dqs_delay_8_n),
                                    .reset(reset_r), .din(rst_dqs_div2), .dout(fifo_81_wr_en));
   fifo_1_wr_en fifo_81_wr_en_inst_1 (.clk(dqs8_delayed_col1), .rst_dqs_delay_n(rst_dqs_delay_8_n_1),
                                    .reset(reset_r), .din(rst_dqs_div2), .dout(fifo_81_wr_en_1));

   //write pointer gray counter instances

   wr_gray_cntr fifo_00_wr_addr_inst (.clk(dqs0_delayed_col1), .reset(reset_r), .cnt_en(fifo_00_wr_en),
                                      .wgc_gcnt(fifo_00_wr_addr));
   wr_gray_cntr fifo_01_wr_addr_inst (.clk(dqs0_delayed_col0_n), .reset(reset_r), .cnt_en(fifo_01_wr_en),
                                      .wgc_gcnt(fifo_01_wr_addr));
   wr_gray_cntr fifo_10_wr_addr_inst (.clk(dqs1_delayed_col1), .reset(reset_r), .cnt_en(fifo_10_wr_en),
                                      .wgc_gcnt(fifo_10_wr_addr));
   wr_gray_cntr fifo_11_wr_addr_inst (.clk(dqs1_delayed_col0_n), .reset(reset_r), .cnt_en(fifo_11_wr_en),
                                      .wgc_gcnt(fifo_11_wr_addr));
   wr_gray_cntr fifo_20_wr_addr_inst (.clk(dqs2_delayed_col1), .reset(reset_r), .cnt_en(fifo_20_wr_en),
                                      .wgc_gcnt(fifo_20_wr_addr));
   wr_gray_cntr fifo_21_wr_addr_inst (.clk(dqs2_delayed_col0_n), .reset(reset_r), .cnt_en(fifo_21_wr_en),
                                      .wgc_gcnt(fifo_21_wr_addr));
   wr_gray_cntr fifo_30_wr_addr_inst (.clk(dqs3_delayed_col1), .reset(reset_r), .cnt_en(fifo_30_wr_en),
                                      .wgc_gcnt(fifo_30_wr_addr));
   wr_gray_cntr fifo_31_wr_addr_inst (.clk(dqs3_delayed_col0_n), .reset(reset_r), .cnt_en(fifo_31_wr_en),
                                      .wgc_gcnt(fifo_31_wr_addr));
   wr_gray_cntr fifo_40_wr_addr_inst (.clk(dqs4_delayed_col1), .reset(reset_r), .cnt_en(fifo_40_wr_en),
                                      .wgc_gcnt(fifo_40_wr_addr));
   wr_gray_cntr fifo_41_wr_addr_inst (.clk(dqs4_delayed_col0_n), .reset(reset_r), .cnt_en(fifo_41_wr_en),
                                      .wgc_gcnt(fifo_41_wr_addr));
   wr_gray_cntr fifo_50_wr_addr_inst (.clk(dqs5_delayed_col1), .reset(reset_r), .cnt_en(fifo_50_wr_en),
                                      .wgc_gcnt(fifo_50_wr_addr));
   wr_gray_cntr fifo_51_wr_addr_inst (.clk(dqs5_delayed_col0_n), .reset(reset_r), .cnt_en(fifo_51_wr_en),
                                      .wgc_gcnt(fifo_51_wr_addr));
   wr_gray_cntr fifo_60_wr_addr_inst (.clk(dqs6_delayed_col1), .reset(reset_r), .cnt_en(fifo_60_wr_en),
                                      .wgc_gcnt(fifo_60_wr_addr));
   wr_gray_cntr fifo_61_wr_addr_inst (.clk(dqs6_delayed_col0_n), .reset(reset_r), .cnt_en(fifo_61_wr_en),
                                      .wgc_gcnt(fifo_61_wr_addr));
   wr_gray_cntr fifo_70_wr_addr_inst (.clk(dqs7_delayed_col1), .reset(reset_r), .cnt_en(fifo_70_wr_en),
                                      .wgc_gcnt(fifo_70_wr_addr));
   wr_gray_cntr fifo_71_wr_addr_inst (.clk(dqs7_delayed_col0_n), .reset(reset_r), .cnt_en(fifo_71_wr_en),
                                      .wgc_gcnt(fifo_71_wr_addr));
   wr_gray_cntr fifo_80_wr_addr_inst (.clk(dqs8_delayed_col1), .reset(reset_r), .cnt_en(fifo_80_wr_en),
                                      .wgc_gcnt(fifo_80_wr_addr));
   wr_gray_cntr fifo_81_wr_addr_inst (.clk(dqs8_delayed_col0_n), .reset(reset_r), .cnt_en(fifo_81_wr_en),
                                      .wgc_gcnt(fifo_81_wr_addr));
   wr_gray_cntr fifo_00_wr_addr_inst_1 (.clk(dqs0_delayed_col0), .reset(reset_r), .cnt_en(fifo_00_wr_en),
                                      .wgc_gcnt(fifo_00_wr_addr_1));
   wr_gray_cntr fifo_01_wr_addr_inst_1 (.clk(dqs0_delayed_col1_n), .reset(reset_r), .cnt_en(fifo_01_wr_en),
                                      .wgc_gcnt(fifo_01_wr_addr_1));
   wr_gray_cntr fifo_10_wr_addr_inst_1 (.clk(dqs1_delayed_col0), .reset(reset_r), .cnt_en(fifo_10_wr_en),
                                      .wgc_gcnt(fifo_10_wr_addr_1));
   wr_gray_cntr fifo_11_wr_addr_inst_1 (.clk(dqs1_delayed_col1_n), .reset(reset_r), .cnt_en(fifo_11_wr_en),
                                      .wgc_gcnt(fifo_11_wr_addr_1));
   wr_gray_cntr fifo_20_wr_addr_inst_1 (.clk(dqs2_delayed_col0), .reset(reset_r), .cnt_en(fifo_20_wr_en),
                                      .wgc_gcnt(fifo_20_wr_addr_1));
   wr_gray_cntr fifo_21_wr_addr_inst_1 (.clk(dqs2_delayed_col1_n), .reset(reset_r), .cnt_en(fifo_21_wr_en),
                                      .wgc_gcnt(fifo_21_wr_addr_1));
   wr_gray_cntr fifo_30_wr_addr_inst_1 (.clk(dqs3_delayed_col0), .reset(reset_r), .cnt_en(fifo_30_wr_en),
                                      .wgc_gcnt(fifo_30_wr_addr_1));
   wr_gray_cntr fifo_31_wr_addr_inst_1 (.clk(dqs3_delayed_col1_n), .reset(reset_r), .cnt_en(fifo_31_wr_en),
                                      .wgc_gcnt(fifo_31_wr_addr_1));
   wr_gray_cntr fifo_40_wr_addr_inst_1 (.clk(dqs4_delayed_col0), .reset(reset_r), .cnt_en(fifo_40_wr_en),
                                      .wgc_gcnt(fifo_40_wr_addr_1));
   wr_gray_cntr fifo_41_wr_addr_inst_1 (.clk(dqs4_delayed_col1_n), .reset(reset_r), .cnt_en(fifo_41_wr_en),
                                      .wgc_gcnt(fifo_41_wr_addr_1));
   wr_gray_cntr fifo_50_wr_addr_inst_1 (.clk(dqs5_delayed_col0), .reset(reset_r), .cnt_en(fifo_50_wr_en),
                                      .wgc_gcnt(fifo_50_wr_addr_1));
   wr_gray_cntr fifo_51_wr_addr_inst_1 (.clk(dqs5_delayed_col1_n), .reset(reset_r), .cnt_en(fifo_51_wr_en),
                                      .wgc_gcnt(fifo_51_wr_addr_1));
   wr_gray_cntr fifo_60_wr_addr_inst_1 (.clk(dqs6_delayed_col0), .reset(reset_r), .cnt_en(fifo_60_wr_en),
                                      .wgc_gcnt(fifo_60_wr_addr_1));
   wr_gray_cntr fifo_61_wr_addr_inst_1 (.clk(dqs6_delayed_col1_n), .reset(reset_r), .cnt_en(fifo_61_wr_en),
                                      .wgc_gcnt(fifo_61_wr_addr_1));
   wr_gray_cntr fifo_70_wr_addr_inst_1 (.clk(dqs7_delayed_col0), .reset(reset_r), .cnt_en(fifo_70_wr_en),
                                      .wgc_gcnt(fifo_70_wr_addr_1));
   wr_gray_cntr fifo_71_wr_addr_inst_1 (.clk(dqs7_delayed_col1_n), .reset(reset_r), .cnt_en(fifo_71_wr_en),
                                      .wgc_gcnt(fifo_71_wr_addr_1));
   wr_gray_cntr fifo_80_wr_addr_inst_1 (.clk(dqs8_delayed_col0), .reset(reset_r), .cnt_en(fifo_80_wr_en),
                                      .wgc_gcnt(fifo_80_wr_addr_1));
   wr_gray_cntr fifo_81_wr_addr_inst_1 (.clk(dqs8_delayed_col1_n), .reset(reset_r), .cnt_en(fifo_81_wr_en),
                                      .wgc_gcnt(fifo_81_wr_addr_1));
   

endmodule 

