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
module ddr1_top (
     SYS_CLK,
     SYS_CLKb,
     dip1,   	
     rst_dqs_div_in,
     rst_dqs_div_in2,
     rst_dqs_div_out,
     rst_dqs_div_out2,
     reset_in,   
     user_input_data,
     user_output_data,
     user_data_valid,
     user_input_address,
     user_bank_address,
     user_config_register,
     user_command_register,
     user_cmd_ack,      
     burst_done,
     read_out_data1,
     pass1,
     init_val,
     ar_done,
     ddr_dqs,
     ddr_dq,
     ddr_cke,
     ddr_csb,
     ddr_rasb,
     ddr_casb,
     ddr_web,
     ddr_dm, 
     ddr_ba,
     ddr_address,
     ddr1_clk0,
     ddr1_clk0b,
     ddr1_clk1,
     ddr1_clk1b,
     ddr1_clk2,
     ddr1_clk2b,
     ddr1_clk3,
     ddr1_clk3b,
     ddr1_clk4,
     ddr1_clk4b,
     dimm_clk0,
     dimm_clk0b,
     dimm_clk1,
     dimm_clk1b,
     dimm_clk2,
     dimm_clk2b, 
     JP1_header,    
     JP2_header,  
     clk_int_val,
     clk90_int_val, 
     sys_rst_val,
     sys_rst90_val,
     sys_rst180_val,
     sys_rst270_val); 






   input     SYS_CLK;
   input     SYS_CLKb;
   input     dip1;   	
   input     rst_dqs_div_in;
   input     rst_dqs_div_in2;
   input     reset_in;   
   input     [143:0] user_input_data;
   input     [21:0] user_input_address;
   input     [1:0] user_bank_address;
   input     [9:0] user_config_register;
   input     [2:0] user_command_register;    
   input     burst_done;
   input     [143:0] read_out_data1;
   input     pass1;


   inout     [8:0] ddr_dqs;
   inout     [71:0] ddr_dq;



   output     ddr_cke;
   output     ddr_csb;
   output     ddr_rasb;
   output     ddr_casb;
   output     ddr_web;
   output     [8:0] ddr_dm; 
   output     [1:0] ddr_ba;
   output     [12:0] ddr_address;
   output     ddr1_clk0;
   output     ddr1_clk0b;
   output     ddr1_clk1;
   output     ddr1_clk1b;
   output     ddr1_clk2;
   output     ddr1_clk2b;
   output     ddr1_clk3;
   output     ddr1_clk3b;
   output     ddr1_clk4;
   output     ddr1_clk4b;
   output     dimm_clk0;
   output     dimm_clk0b;
   output     dimm_clk1;
   output     dimm_clk1b;
   output     dimm_clk2;
   output     dimm_clk2b; 
   output     [7:0] JP1_header;    
   output     [7:0] JP2_header;  
   output     clk_int_val;
   output     clk90_int_val; 
   output     sys_rst_val;
   output     sys_rst90_val;
   output     sys_rst180_val;
   output     sys_rst270_val;
   output     rst_dqs_div_out;
   output     rst_dqs_div_out2;
   output     [143:0] user_output_data;
   output     user_data_valid;
   output     user_cmd_ack; 
   output     init_val;
   output     ar_done;






   wire  sys_clk_ibuf;
   wire  rst_calib;
   wire  [4:0] delay_sel;
   wire  sys_rst;
   wire  sys_rst90;
   wire  sys_rst180;
   wire  sys_rst270;
   wire  clk_int;
   wire  clk90_int;
   wire  [7:0] JP1_header_infra;
   wire  [7:0] JP2_header_infra;
   wire  write_enable;
   wire  dqs_div_rst;
   wire  dqs_div_rst2;
   wire  dqs_enable;
   wire  dqs_reset;
   wire  dqs_int_delay_in0;
   wire  dqs_int_delay_in1;
   wire  dqs_int_delay_in2;
   wire  dqs_int_delay_in3;
   wire  dqs_int_delay_in4;
   wire  dqs_int_delay_in5;
   wire  dqs_int_delay_in6;
   wire  dqs_int_delay_in7;
   wire  dqs_int_delay_in8;
   wire  [71:0] dq;
   wire  u_data_val;
   wire  write_en_val;
   wire  write_en_val1;
   wire  reset90_r;
   wire  [8:0] data_mask_f;
   wire  [8:0] data_mask_r;
   wire  [71:0] write_data_falling;
   wire  [71:0] write_data_rising;
   wire  ddr_rasb_cntrl;
   wire  ddr_casb_cntrl;
   wire  ddr_web_cntrl;
   wire  [1:0] ddr_ba_cntrl;
   wire  [12:0] ddr_address_cntrl;
   wire  ddr_cke_cntrl;
   wire  ddr_csb_cntrl;
   wire  rst_dqs_div_int;
   wire  rst_dqs_div_int1;


   assign sys_rst_val = sys_rst;
   assign sys_rst90_val = sys_rst90;
   assign sys_rst180_val = sys_rst180;
   assign sys_rst270_val = sys_rst270;
   assign clk_int_val = clk_int;
   assign clk90_int_val = clk90_int;

   controller controller0 (
                                   .dip1               ( dip1),
                                   .clk                ( clk_int), 
                                   .rst0               ( sys_rst),
                                   .rst180             ( sys_rst180),
                                   .address            ( user_input_address),
                                   .bank_address       ( user_bank_address),
                                   .config_register    ( user_config_register),
                                   .command_register   ( user_command_register),
                                   .burst_done         ( burst_done),
                                   .ddr_rasb_cntrl     ( ddr_rasb_cntrl),
                                   .ddr_casb_cntrl     ( ddr_casb_cntrl),
                                   .ddr_web_cntrl      ( ddr_web_cntrl),
                                   .ddr_ba_cntrl       ( ddr_ba_cntrl),
                                   .ddr_address_cntrl  ( ddr_address_cntrl),
                                   .ddr_cke_cntrl      ( ddr_cke_cntrl),
                                   .ddr_csb_cntrl      ( ddr_csb_cntrl),
                                   .dqs_enable         ( dqs_enable),
                                   .dqs_reset          ( dqs_reset),
                                   .write_enable       ( write_enable),
                                   .rst_calib          ( rst_calib),
                                   .rst_dqs_div_int    ( rst_dqs_div_int),
                                   .rst_dqs_div_int1    ( rst_dqs_div_int1),
                                   .cmd_ack            ( user_cmd_ack),
                                   .init               ( init_val),
                                   .ar_done            ( ar_done)
                                   
                                  );
                             
   data_path data_path0 ( 
                                 .user_input_data     ( user_input_data),
                                 .clk                 ( clk_int),
                                 .clk90               ( clk90_int),
                                 .reset               ( sys_rst),
                                 .reset90             ( sys_rst90),
                                 .reset180            ( sys_rst180),
                                 .reset270            ( sys_rst270),
                                 .write_enable        ( write_enable),
                                 .rst_dqs_div_in      ( dqs_div_rst),
                                 .rst_dqs_div_in2     ( dqs_div_rst2), 
                                 .delay_sel           ( delay_sel),
                                 .dqs_int_delay_in0   ( dqs_int_delay_in0),
                                 .dqs_int_delay_in1   ( dqs_int_delay_in1),
                                 .dqs_int_delay_in2   ( dqs_int_delay_in2),
                                 .dqs_int_delay_in3   ( dqs_int_delay_in3),
                                 .dqs_int_delay_in4   ( dqs_int_delay_in4),
                                 .dqs_int_delay_in5   ( dqs_int_delay_in5),
                                 .dqs_int_delay_in6   ( dqs_int_delay_in6),
                                 .dqs_int_delay_in7   ( dqs_int_delay_in7),
                                 .dqs_int_delay_in8   ( dqs_int_delay_in8),
                                 .dq                  ( dq),      
                                 .u_data_val          ( user_data_valid),
                                 .user_output_data    ( user_output_data),
                                 .write_en_val        ( write_en_val),
                                 .write_en_val1       ( write_en_val1),
                                 .reset90_r_val       ( reset90_r),
                                 .data_mask_f         ( data_mask_f),
                                 .data_mask_r         ( data_mask_r),
                                 .write_data_falling  ( write_data_falling),
                                 .write_data_rising   ( write_data_rising)                     
                                );                           
                          		
							
   infrastructure infrastructure0
                                        (
                                         .reset_in           (   reset_in),
                                         .sys_clk_ibuf       (   sys_clk_ibuf),
                                         .read_out_data1     (   read_out_data1),
                                         .pass1              (   pass1),
                                         .rst_calib1         (   rst_calib),
                                         .delay_sel_val1_val (   delay_sel),
                                         .sys_rst_val        (   sys_rst),
                                         .sys_rst90_val      (   sys_rst90),
                                         .sys_rst180_val     (   sys_rst180),
                                         .sys_rst270_val     (   sys_rst270),
                                         .clk_int_val        (   clk_int),
                                         .clk90_int_val      (   clk90_int),
                                         .JP1_header_infra   (   JP1_header_infra),
                                         .JP2_header_infra   (   JP2_header_infra )
                                        );  
   iobs iobs0 
                    (
                     .SYS_CLK            (  SYS_CLK),  
                     .SYS_CLKb           (  SYS_CLKb),
                     .clk                (  clk_int),
                     .clk90              (  clk90_int),
                     .JP1_header_infra   (  JP1_header_infra),
                     .JP2_header_infra   (  JP2_header_infra),
                     .ddr_rasb_cntrl     (  ddr_rasb_cntrl), 
                     .ddr_casb_cntrl     (  ddr_casb_cntrl),
                     .ddr_web_cntrl      (  ddr_web_cntrl),
                     .ddr_cke_cntrl      (  ddr_cke_cntrl),
                     .ddr_csb_cntrl      (  ddr_csb_cntrl),
                     .ddr_address_cntrl  (  ddr_address_cntrl),
                     .ddr_ba_cntrl       (  ddr_ba_cntrl),
                     .rst_dqs_div_int    (  rst_dqs_div_int),
                     .rst_dqs_div_int1   (  rst_dqs_div_int1),
                     .dqs_reset          (  dqs_reset),
                     .dqs_enable         (  dqs_enable),
                     .ddr_dqs            (  ddr_dqs),
                     .ddr_dq             (  ddr_dq),
                     .write_data_falling (  write_data_falling), 
                     .write_data_rising  (  write_data_rising),
                     .write_en_val       (  write_en_val),
                     .write_en_val1      (  write_en_val1),
                     .reset90_r          (  reset90_r),
                     .data_mask_f        (  data_mask_f), 
                     .data_mask_r        (  data_mask_r),
                     .sys_clk_ibuf       (  sys_clk_ibuf),
                     .ddr1_clk0          (  ddr1_clk0), 
                     .ddr1_clk0b         (  ddr1_clk0b),
                     .ddr1_clk1          (  ddr1_clk1),
                     .ddr1_clk1b         (  ddr1_clk1b),
                     .ddr1_clk2          (  ddr1_clk2),
                     .ddr1_clk2b         (  ddr1_clk2b),
                     .ddr1_clk3          (  ddr1_clk3),
                     .ddr1_clk3b         (  ddr1_clk3b),
                     .ddr1_clk4          (  ddr1_clk4),
                     .ddr1_clk4b         (  ddr1_clk4b),
                     .dimm_clk0          (  dimm_clk0),
                     .dimm_clk0b         (  dimm_clk0b),
                     .dimm_clk1          (  dimm_clk1),
                     .dimm_clk1b         (  dimm_clk1b),
                     .dimm_clk2          (  dimm_clk2),
                     .dimm_clk2b         (  dimm_clk2b),
                     .JP1_header         (  JP1_header),
                     .JP2_header         (  JP2_header),   
                     .ddr_rasb           (  ddr_rasb), 
                     .ddr_casb           (  ddr_casb),
                     .ddr_web            (  ddr_web),
                     .ddr_ba             (  ddr_ba),
                     .ddr_address        (  ddr_address),
                     .ddr_cke            (  ddr_cke),
                     .ddr_csb            (  ddr_csb),
                     .rst_dqs_div        (  dqs_div_rst), 
                     .rst_dqs_div2       (  dqs_div_rst2),                 
  		     .rst_dqs_div_in	    (  rst_dqs_div_in),
                     .rst_dqs_div_in2    (  rst_dqs_div_in2), 
		     .rst_dqs_div_out	   (  rst_dqs_div_out),
                     .rst_dqs_div_out2   (  rst_dqs_div_out2), 
                     .dqs_int_delay_in0  (  dqs_int_delay_in0),
                     .dqs_int_delay_in1  (  dqs_int_delay_in1),
                     .dqs_int_delay_in2  (  dqs_int_delay_in2),
                     .dqs_int_delay_in3  (  dqs_int_delay_in3), 
                     .dqs_int_delay_in4  (  dqs_int_delay_in4),
                     .dqs_int_delay_in5  (  dqs_int_delay_in5),
                     .dqs_int_delay_in6  (  dqs_int_delay_in6),
                     .dqs_int_delay_in7  (  dqs_int_delay_in7),
                     .dqs_int_delay_in8  (  dqs_int_delay_in8),
                     .dq                 (  dq),
                     .ddr_dm             (  ddr_dm)
                    );	




endmodule 
   
      
