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
module data_path(

     user_input_data,
     clk,                
     clk90,              
     reset,              
     reset90,            
     reset180,           
     reset270,           
     write_enable,       
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
     dq,                        
     u_data_val,         
     user_output_data,  
     write_en_val,       
     write_en_val1,      
     reset90_r_val,      
     data_mask_f,        
     data_mask_r,        
     write_data_falling,
     write_data_rising);  


   input      [143:0] user_input_data;
   input      clk;
   input      clk90;
   input      reset;
   input      reset90;
   input      reset180;
   input      reset270;
   input      write_enable;
   input      rst_dqs_div_in;
   input      rst_dqs_div_in2;
   input      [4:0] delay_sel;   
   input      dqs_int_delay_in0;
   input      dqs_int_delay_in1;
   input      dqs_int_delay_in2;
   input      dqs_int_delay_in3;
   input      dqs_int_delay_in4;
   input      dqs_int_delay_in5;
   input      dqs_int_delay_in6;
   input      dqs_int_delay_in7;
   input      dqs_int_delay_in8;  
   input      [71:0] dq;       
   output     u_data_val;
   output     [143:0] user_output_data;
   output     write_en_val;
   output     write_en_val1;
   output     reset90_r_val;
   output     [8:0] data_mask_f;
   output     [8:0] data_mask_r;
   output     [71:0] write_data_falling;
   output     [71:0] write_data_rising;





   wire  reset_r;
   wire  reset90_r;
   wire  reset90_r1;
   wire  reset180_r;
   wire  reset270_r;                                
   wire  [3:0] fifo0_rd_addr;
   wire  [3:0] fifo1_rd_addr;                          
   wire  read_valid_data_1; 
   wire  [3:0] fifo_00_wr_addr;           
   wire  [3:0] fifo_01_wr_addr;           
   wire  [3:0] fifo_10_wr_addr;          
   wire  [3:0] fifo_11_wr_addr;             
   wire  [3:0] fifo_20_wr_addr;           
   wire  [3:0] fifo_21_wr_addr;           
   wire  [3:0] fifo_30_wr_addr;           
   wire  [3:0] fifo_31_wr_addr;           
   wire  [3:0] fifo_40_wr_addr;           
   wire  [3:0] fifo_41_wr_addr;           
   wire  [3:0] fifo_50_wr_addr;           
   wire  [3:0] fifo_51_wr_addr;           
   wire  [3:0] fifo_60_wr_addr;           
   wire  [3:0] fifo_61_wr_addr;           
   wire  [3:0] fifo_70_wr_addr;           
   wire  [3:0] fifo_71_wr_addr;           
   wire  [3:0] fifo_80_wr_addr;           
   wire  [3:0] fifo_81_wr_addr;           
   wire  fifo_00_wr_en;
   wire  fifo_10_wr_en;
   wire  fifo_20_wr_en;
   wire  fifo_30_wr_en;
   wire  fifo_40_wr_en;
   wire  fifo_50_wr_en;
   wire  fifo_60_wr_en;
   wire  fifo_70_wr_en;
   wire  fifo_80_wr_en;
   wire  fifo_01_wr_en;
   wire  fifo_11_wr_en;
   wire  fifo_21_wr_en;
   wire  fifo_31_wr_en;
   wire  fifo_41_wr_en;
   wire  fifo_51_wr_en;
   wire  fifo_61_wr_en;
   wire  fifo_71_wr_en;
   wire  fifo_81_wr_en;
   wire  [3:0] fifo_00_wr_addr_1;           
   wire  [3:0] fifo_01_wr_addr_1;           
   wire  [3:0] fifo_10_wr_addr_1;          
   wire  [3:0] fifo_11_wr_addr_1;             
   wire  [3:0] fifo_20_wr_addr_1;           
   wire  [3:0] fifo_21_wr_addr_1;           
   wire  [3:0] fifo_30_wr_addr_1;           
   wire  [3:0] fifo_31_wr_addr_1;           
   wire  [3:0] fifo_40_wr_addr_1;           
   wire  [3:0] fifo_41_wr_addr_1;           
   wire  [3:0] fifo_50_wr_addr_1;           
   wire  [3:0] fifo_51_wr_addr_1;           
   wire  [3:0] fifo_60_wr_addr_1;           
   wire  [3:0] fifo_61_wr_addr_1;           
   wire  [3:0] fifo_70_wr_addr_1;           
   wire  [3:0] fifo_71_wr_addr_1;           
   wire  [3:0] fifo_80_wr_addr_1;           
   wire  [3:0] fifo_81_wr_addr_1;           
   wire  fifo_00_wr_en_1;
   wire  fifo_10_wr_en_1;
   wire  fifo_20_wr_en_1;
   wire  fifo_30_wr_en_1;
   wire  fifo_40_wr_en_1;
   wire  fifo_50_wr_en_1;
   wire  fifo_60_wr_en_1;
   wire  fifo_70_wr_en_1;
   wire  fifo_80_wr_en_1;
   wire  fifo_01_wr_en_1;
   wire  fifo_11_wr_en_1;
   wire  fifo_21_wr_en_1;
   wire  fifo_31_wr_en_1;
   wire  fifo_41_wr_en_1;
   wire  fifo_51_wr_en_1;
   wire  fifo_61_wr_en_1;
   wire  fifo_71_wr_en_1;
   wire  fifo_81_wr_en_1;
   wire  dqs0_delayed_col1;
   wire  dqs1_delayed_col1;
   wire  dqs2_delayed_col1;
   wire  dqs3_delayed_col1;
   wire  dqs4_delayed_col1;
   wire  dqs5_delayed_col1;
   wire  dqs6_delayed_col1;
   wire  dqs7_delayed_col1;
   wire  dqs8_delayed_col1;
   wire  dqs0_delayed_col0;
   wire  dqs1_delayed_col0;
   wire  dqs2_delayed_col0;
   wire  dqs3_delayed_col0;
   wire  dqs4_delayed_col0;
   wire  dqs5_delayed_col0;
   wire  dqs6_delayed_col0;
   wire  dqs7_delayed_col0;
   wire  dqs8_delayed_col0;
   


   assign reset90_r_val = reset90_r;

   data_read data_read0(
            .clk90               (clk90),
            .reset90_r           (reset90_r1),
	    .ddr_dq_in           (dq),
            .read_valid_data_1   (read_valid_data_1),   
	    .fifo_00_wr_en	( fifo_00_wr_en),
	    .fifo_10_wr_en	( fifo_10_wr_en),
	    .fifo_20_wr_en	( fifo_20_wr_en),
	    .fifo_30_wr_en	( fifo_30_wr_en),
	    .fifo_40_wr_en	( fifo_40_wr_en),
	    .fifo_50_wr_en	( fifo_50_wr_en),
	    .fifo_60_wr_en	( fifo_60_wr_en),
	    .fifo_70_wr_en	( fifo_70_wr_en),
	    .fifo_80_wr_en	( fifo_80_wr_en),
	    .fifo_01_wr_en	( fifo_01_wr_en),
	    .fifo_11_wr_en	( fifo_11_wr_en),
	    .fifo_21_wr_en	( fifo_21_wr_en),
	    .fifo_31_wr_en	( fifo_31_wr_en),
	    .fifo_41_wr_en	( fifo_41_wr_en),
	    .fifo_51_wr_en	( fifo_51_wr_en),
	    .fifo_61_wr_en	( fifo_61_wr_en),
	    .fifo_71_wr_en	( fifo_71_wr_en),
	    .fifo_81_wr_en	( fifo_81_wr_en),           
            .fifo_00_wr_addr     (fifo_00_wr_addr),
            .fifo_01_wr_addr     (fifo_01_wr_addr),
            .fifo_10_wr_addr     (fifo_10_wr_addr),
            .fifo_11_wr_addr     (fifo_11_wr_addr),
            .fifo_20_wr_addr     (fifo_20_wr_addr),
            .fifo_21_wr_addr     (fifo_21_wr_addr),
            .fifo_30_wr_addr     (fifo_30_wr_addr),
            .fifo_31_wr_addr     (fifo_31_wr_addr),
            .fifo_40_wr_addr     (fifo_40_wr_addr),
            .fifo_41_wr_addr     (fifo_41_wr_addr),
            .fifo_50_wr_addr     (fifo_50_wr_addr),
            .fifo_51_wr_addr     (fifo_51_wr_addr),
            .fifo_60_wr_addr     (fifo_60_wr_addr),
            .fifo_61_wr_addr     (fifo_61_wr_addr),
            .fifo_70_wr_addr     (fifo_70_wr_addr),
            .fifo_71_wr_addr     (fifo_71_wr_addr),
            .fifo_80_wr_addr     (fifo_80_wr_addr),
            .fifo_81_wr_addr     (fifo_81_wr_addr),
	    .fifo_00_wr_en_1	(fifo_00_wr_en_1),
	    .fifo_10_wr_en_1	(fifo_10_wr_en_1),
	    .fifo_20_wr_en_1	(fifo_20_wr_en_1),
	    .fifo_30_wr_en_1	(fifo_30_wr_en_1),
	    .fifo_40_wr_en_1	(fifo_40_wr_en_1),
	    .fifo_50_wr_en_1	(fifo_50_wr_en_1),
	    .fifo_60_wr_en_1	(fifo_60_wr_en_1),
	    .fifo_70_wr_en_1	(fifo_70_wr_en_1),
	    .fifo_80_wr_en_1	(fifo_80_wr_en_1),
	    .fifo_01_wr_en_1	(fifo_01_wr_en_1),
	    .fifo_11_wr_en_1	(fifo_11_wr_en_1),
	    .fifo_21_wr_en_1	(fifo_21_wr_en_1),
	    .fifo_31_wr_en_1	(fifo_31_wr_en_1),
	    .fifo_41_wr_en_1	(fifo_41_wr_en_1),
	    .fifo_51_wr_en_1	(fifo_51_wr_en_1),
	    .fifo_61_wr_en_1	(fifo_61_wr_en_1),
	    .fifo_71_wr_en_1	(fifo_71_wr_en_1),
	    .fifo_81_wr_en_1	(fifo_81_wr_en_1),
            .fifo_00_wr_addr_1   (fifo_00_wr_addr_1),
            .fifo_01_wr_addr_1   (fifo_01_wr_addr_1),
            .fifo_10_wr_addr_1   (fifo_10_wr_addr_1),
            .fifo_11_wr_addr_1   (fifo_11_wr_addr_1),
            .fifo_20_wr_addr_1   (fifo_20_wr_addr_1),
            .fifo_21_wr_addr_1   (fifo_21_wr_addr_1),
            .fifo_30_wr_addr_1   (fifo_30_wr_addr_1),
            .fifo_31_wr_addr_1   (fifo_31_wr_addr_1),
            .fifo_40_wr_addr_1   (fifo_40_wr_addr_1),
            .fifo_41_wr_addr_1   (fifo_41_wr_addr_1),
            .fifo_50_wr_addr_1   (fifo_50_wr_addr_1),
            .fifo_51_wr_addr_1   (fifo_51_wr_addr_1),
            .fifo_60_wr_addr_1   (fifo_60_wr_addr_1),
            .fifo_61_wr_addr_1   (fifo_61_wr_addr_1),
            .fifo_70_wr_addr_1   (fifo_70_wr_addr_1),
            .fifo_71_wr_addr_1   (fifo_71_wr_addr_1),
            .fifo_80_wr_addr_1   (fifo_80_wr_addr_1),
            .fifo_81_wr_addr_1   (fifo_81_wr_addr_1),		
            .dqs0_delayed_col1   (dqs0_delayed_col1),
            .dqs1_delayed_col1   (dqs1_delayed_col1),
            .dqs2_delayed_col1   (dqs2_delayed_col1),
            .dqs3_delayed_col1   (dqs3_delayed_col1),
            .dqs4_delayed_col1   (dqs4_delayed_col1),
            .dqs5_delayed_col1   (dqs5_delayed_col1),
            .dqs6_delayed_col1   (dqs6_delayed_col1),
            .dqs7_delayed_col1   (dqs7_delayed_col1),
            .dqs8_delayed_col1   (dqs8_delayed_col1),
 	    .dqs0_delayed_col0   (dqs0_delayed_col0),
	    .dqs1_delayed_col0   (dqs1_delayed_col0),
	    .dqs2_delayed_col0   (dqs2_delayed_col0),
	    .dqs3_delayed_col0   (dqs3_delayed_col0),
	    .dqs4_delayed_col0   (dqs4_delayed_col0),
	    .dqs5_delayed_col0   (dqs5_delayed_col0),
	    .dqs6_delayed_col0   (dqs6_delayed_col0),
	    .dqs7_delayed_col0   (dqs7_delayed_col0),
	    .dqs8_delayed_col0   (dqs8_delayed_col0),
            .user_output_data    (user_output_data),
            .fifo0_rd_addr_val   (fifo0_rd_addr),
            .fifo1_rd_addr_val   (fifo1_rd_addr ));



    data_read_controller  data_read_controller0(
            .clk90                  (clk90),
            .reset_r                (reset_r),
            .reset90_r              (reset90_r1),
            .rst_dqs_div_in         (rst_dqs_div_in),
            .rst_dqs_div_in2        (rst_dqs_div_in2), 
            .delay_sel              (delay_sel),
            .dqs_int_delay_in0      (dqs_int_delay_in0),
            .dqs_int_delay_in1      (dqs_int_delay_in1),
            .dqs_int_delay_in2      (dqs_int_delay_in2),
            .dqs_int_delay_in3      (dqs_int_delay_in3),
            .dqs_int_delay_in4      (dqs_int_delay_in4),
            .dqs_int_delay_in5      (dqs_int_delay_in5),
            .dqs_int_delay_in6      (dqs_int_delay_in6),
            .dqs_int_delay_in7      (dqs_int_delay_in7),
            .dqs_int_delay_in8      (dqs_int_delay_in8),
            .fifo0_rd_addr          (fifo0_rd_addr),
            .fifo1_rd_addr          (fifo1_rd_addr),
            .u_data_val             (u_data_val),
            .read_valid_data_1_val  (read_valid_data_1),
            .fifo_00_wr_en_val      (fifo_00_wr_en),
            .fifo_10_wr_en_val	   (fifo_10_wr_en),            
            .fifo_20_wr_en_val	   (fifo_20_wr_en),            
            .fifo_30_wr_en_val	   (fifo_30_wr_en),
            .fifo_40_wr_en_val	   (fifo_40_wr_en),
            .fifo_50_wr_en_val	   (fifo_50_wr_en),
            .fifo_60_wr_en_val	   (fifo_60_wr_en),
            .fifo_70_wr_en_val	   (fifo_70_wr_en),
            .fifo_80_wr_en_val	   (fifo_80_wr_en),                                    	    
            .fifo_01_wr_en_val	   (fifo_01_wr_en),
            .fifo_11_wr_en_val	   (fifo_11_wr_en),            
            .fifo_21_wr_en_val	   (fifo_21_wr_en),            
            .fifo_31_wr_en_val	   (fifo_31_wr_en),
            .fifo_41_wr_en_val	   (fifo_41_wr_en),
            .fifo_51_wr_en_val	   (fifo_51_wr_en),
            .fifo_61_wr_en_val	   (fifo_61_wr_en),
            .fifo_71_wr_en_val	   (fifo_71_wr_en),
            .fifo_81_wr_en_val	   (fifo_81_wr_en),            
            .fifo_00_wr_addr_val    (fifo_00_wr_addr),
            .fifo_01_wr_addr_val    (fifo_01_wr_addr),
            .fifo_10_wr_addr_val    (fifo_10_wr_addr),
            .fifo_11_wr_addr_val    (fifo_11_wr_addr),
            .fifo_20_wr_addr_val    (fifo_20_wr_addr),
            .fifo_21_wr_addr_val    (fifo_21_wr_addr),
            .fifo_30_wr_addr_val    (fifo_30_wr_addr),
            .fifo_31_wr_addr_val    (fifo_31_wr_addr),
            .fifo_40_wr_addr_val    (fifo_40_wr_addr),
            .fifo_41_wr_addr_val    (fifo_41_wr_addr),
            .fifo_50_wr_addr_val    (fifo_50_wr_addr),
            .fifo_51_wr_addr_val    (fifo_51_wr_addr),
            .fifo_60_wr_addr_val    (fifo_60_wr_addr),
            .fifo_61_wr_addr_val    (fifo_61_wr_addr),
            .fifo_70_wr_addr_val    (fifo_70_wr_addr),
            .fifo_71_wr_addr_val    (fifo_71_wr_addr),
            .fifo_80_wr_addr_val    (fifo_80_wr_addr),
            .fifo_81_wr_addr_val    (fifo_81_wr_addr),
            .fifo_00_wr_en_val_1    (fifo_00_wr_en_1),
            .fifo_10_wr_en_val_1	   (fifo_10_wr_en_1),            
            .fifo_20_wr_en_val_1	   (fifo_20_wr_en_1),            
            .fifo_30_wr_en_val_1	   (fifo_30_wr_en_1),
            .fifo_40_wr_en_val_1	   (fifo_40_wr_en_1),
            .fifo_50_wr_en_val_1	   (fifo_50_wr_en_1),
            .fifo_60_wr_en_val_1	   (fifo_60_wr_en_1),
            .fifo_70_wr_en_val_1	   (fifo_70_wr_en_1),
            .fifo_80_wr_en_val_1	   (fifo_80_wr_en_1),                        
            .fifo_01_wr_en_val_1	   (fifo_01_wr_en_1),
            .fifo_11_wr_en_val_1	   (fifo_11_wr_en_1),            
            .fifo_21_wr_en_val_1	   (fifo_21_wr_en_1),            
            .fifo_31_wr_en_val_1	   (fifo_31_wr_en_1),
            .fifo_41_wr_en_val_1	   (fifo_41_wr_en_1),
            .fifo_51_wr_en_val_1	   (fifo_51_wr_en_1),
            .fifo_61_wr_en_val_1	   (fifo_61_wr_en_1),
            .fifo_71_wr_en_val_1	   (fifo_71_wr_en_1),
            .fifo_81_wr_en_val_1	   (fifo_81_wr_en_1),            
            .fifo_00_wr_addr_val_1  (fifo_00_wr_addr_1),
            .fifo_01_wr_addr_val_1  (fifo_01_wr_addr_1),
            .fifo_10_wr_addr_val_1  (fifo_10_wr_addr_1),
            .fifo_11_wr_addr_val_1  (fifo_11_wr_addr_1),
            .fifo_20_wr_addr_val_1  (fifo_20_wr_addr_1),
            .fifo_21_wr_addr_val_1  (fifo_21_wr_addr_1),
            .fifo_30_wr_addr_val_1  (fifo_30_wr_addr_1),
            .fifo_31_wr_addr_val_1  (fifo_31_wr_addr_1),
            .fifo_40_wr_addr_val_1  (fifo_40_wr_addr_1),
            .fifo_41_wr_addr_val_1  (fifo_41_wr_addr_1),
            .fifo_50_wr_addr_val_1  (fifo_50_wr_addr_1),
            .fifo_51_wr_addr_val_1  (fifo_51_wr_addr_1),
            .fifo_60_wr_addr_val_1  (fifo_60_wr_addr_1),
            .fifo_61_wr_addr_val_1  (fifo_61_wr_addr_1),
            .fifo_70_wr_addr_val_1  (fifo_70_wr_addr_1),
            .fifo_71_wr_addr_val_1  (fifo_71_wr_addr_1),
            .fifo_80_wr_addr_val_1  (fifo_80_wr_addr_1),
            .fifo_81_wr_addr_val_1  (fifo_81_wr_addr_1),
	    .dqs0_delayed_col0_val  (dqs0_delayed_col0),
	    .dqs1_delayed_col0_val  (dqs1_delayed_col0),
	    .dqs2_delayed_col0_val  (dqs2_delayed_col0),
	    .dqs3_delayed_col0_val  (dqs3_delayed_col0),
	    .dqs4_delayed_col0_val  (dqs4_delayed_col0), 
	    .dqs5_delayed_col0_val  (dqs5_delayed_col0),
	    .dqs6_delayed_col0_val  (dqs6_delayed_col0),
	    .dqs7_delayed_col0_val  (dqs7_delayed_col0),
	    .dqs8_delayed_col0_val  (dqs8_delayed_col0),
            .dqs0_delayed_col1_val  (dqs0_delayed_col1),
	    .dqs1_delayed_col1_val  (dqs1_delayed_col1),
	    .dqs2_delayed_col1_val  (dqs2_delayed_col1),
	    .dqs3_delayed_col1_val  (dqs3_delayed_col1),
	    .dqs4_delayed_col1_val  (dqs4_delayed_col1), 
	    .dqs5_delayed_col1_val  (dqs5_delayed_col1),
	    .dqs6_delayed_col1_val  (dqs6_delayed_col1),
	    .dqs7_delayed_col1_val  (dqs7_delayed_col1),
	    .dqs8_delayed_col1_val  (dqs8_delayed_col1));

         
    data_write data_write0(
          .user_input_data    (user_input_data),
          .clk90              (clk90),
          .reset90_r          (reset90_r),
          .reset270_r         (reset270_r),
          .write_enable       (write_enable),
          .write_en_val       (write_en_val),
          .write_en_val1      (write_en_val1),
          .write_data_falling (write_data_falling),
          .write_data_rising  (write_data_rising),
          .data_mask_f        (data_mask_f),
          .data_mask_r        (data_mask_r));

   data_path_rst  data_path_rst0 (
          .clk                (clk),            
          .clk90              (clk90),
          .reset              (reset),
          .reset90            (reset90),
          .reset180           (reset180),
          .reset270           (reset270),
          .reset_r            (reset_r),
          .reset90_r          (reset90_r),
          .reset90_r1         (reset90_r1),
          .reset180_r         (reset180_r),
          .reset270_r         (reset270_r));

endmodule 

