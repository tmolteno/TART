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
module ddr1_test (
       dip1,
       dip2,
       dip3,
       reset_in,
       SYS_CLK,
       SYS_CLKb,
       rst_dqs_div_in1,
       rst_dqs_div_in2,
       rst_dqs_div_out1,
       rst_dqs_div_out2, 
       ddr1_casb,
       ddr1_cke,
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
       ddr1_csb,
       ddr1_rasb,
       ddr1_web,
       ddr1_address,
       ddr1_ba,
       ddr1_dm,
       display1,
       led_error_output1,
       ddr1_dq,
       ddr1_dqs,
       dimm_clk0,
       dimm_clk0b,
       dimm_clk1,
       dimm_clk1b,
       dimm_clk2,
       dimm_clk2b,  
       //read_data_out,
      // lfsr_data_out,      
       JP1_header,
       JP2_header );

   input       dip2;
   input       dip1; 
   input       dip3;
   input       reset_in;
   input       SYS_CLK;
   input       SYS_CLKb;
   input       rst_dqs_div_in1;
   input       rst_dqs_div_in2;

   inout       [71:0] ddr1_dq;
   inout       [8:0] ddr1_dqs;

   output       rst_dqs_div_out1;
   output       rst_dqs_div_out2; 
   output       ddr1_casb;
   output       ddr1_cke;
   output       ddr1_clk0;
   output       ddr1_clk0b;
   output       ddr1_clk1;
   output       ddr1_clk1b;
   output       ddr1_clk2;
   output       ddr1_clk2b;
   output       ddr1_clk3;
   output       ddr1_clk3b;
   output       ddr1_clk4;
   output       ddr1_clk4b;       
   output       ddr1_csb;
   output       ddr1_rasb;
   output       ddr1_web;
   output       [12:0] ddr1_address;
   output       [1:0] ddr1_ba;
   output       [8:0] ddr1_dm;
   output       [6:0] display1;
   output       led_error_output1;
   output       dimm_clk0;
   output       dimm_clk0b;
   output       dimm_clk1;
   output       dimm_clk1b;
   output       dimm_clk2;
   output       dimm_clk2b;       
   output       [7:0] JP1_header;
   output       [7:0] JP2_header;  
   //output[143:0] lfsr_data_out;
  // output[143:0] read_data_out;  


   wire [143:0] controller1_data_output;
   wire [23:0] u1_address;
   wire user_data_val1;
   wire [9:0] u1_config_parms;
   wire [2:0] user_cmd1;
   wire user_ack1;
   wire [143:0] u1_data_i;
   wire burst_done_val1;
   wire init_val1;
   wire pass_val1;
   wire ar_done_val1;
   wire clk_int;
   wire clk90_int;
   wire sys_rst;
   wire sys_rst90;
   wire sys_rst180;
   wire sys_rst270;
   wire data_valid_out1;  
   wire [143:0] lfsr_data_out;
   wire [143:0] read_data_out;  
   assign display1 = 7'd0; 


   ddr1_top ddr1_top0(
                               .SYS_CLK                (   SYS_CLK),
                               .SYS_CLKb               (   SYS_CLKb),
                               .dip1                   (   dip1),
                               .rst_dqs_div_in	        ( rst_dqs_div_in1),
			       .rst_dqs_div_out	       ( rst_dqs_div_out1),
                               .rst_dqs_div_in2	        ( rst_dqs_div_in2),
			       .rst_dqs_div_out2	       ( rst_dqs_div_out2),
                               .reset_in               (   reset_in),
                               .user_input_data        (   u1_data_i),
                               .user_output_data       (   controller1_data_output),
                               .user_data_valid        (   user_data_val1),
                               .user_input_address     (   u1_address[23:2]),
                               .user_bank_address      (   u1_address[1:0]),
                               .user_config_register   (   u1_config_parms),
                               .user_command_register  (   user_cmd1),
                               .user_cmd_ack           (   user_ack1),
                               .burst_done             (   burst_done_val1),
                               .read_out_data1         (   read_data_out),
                               .pass1                  (   pass_val1),
                              
                               .init_val               (   init_val1),
                               .ar_done                (   ar_done_val1),
                               .ddr_dqs                (   ddr1_dqs),
                               .ddr_dq                 (   ddr1_dq),
                               .ddr_cke                (   ddr1_cke),
                               .ddr_csb                (   ddr1_csb),
                               .ddr_rasb               (   ddr1_rasb),
                               .ddr_casb               (   ddr1_casb),
                               .ddr_web                (   ddr1_web),
                               .ddr_dm                 (   ddr1_dm),
                               .ddr_ba                 (   ddr1_ba),
                               .ddr_address            (   ddr1_address),
                               .ddr1_clk0              (   ddr1_clk0),
                               .ddr1_clk0b             (   ddr1_clk0b),
                               .ddr1_clk1              (   ddr1_clk1),
                               .ddr1_clk1b             (   ddr1_clk1b),
                               .ddr1_clk2              (   ddr1_clk2),
                               .ddr1_clk2b             (   ddr1_clk2b),
                               .ddr1_clk3              (   ddr1_clk3),
                               .ddr1_clk3b             (   ddr1_clk3b),
                               .ddr1_clk4              (   ddr1_clk4),
                               .ddr1_clk4b             (   ddr1_clk4b),
                               .dimm_clk0              (   dimm_clk0),
                               .dimm_clk0b             (   dimm_clk0b),
                               .dimm_clk1              (   dimm_clk1),
                               .dimm_clk1b             (   dimm_clk1b),
                               .dimm_clk2              (   dimm_clk2),
                               .dimm_clk2b             (   dimm_clk2b),
                               .JP1_header             (   JP1_header),
                               .JP2_header             (   JP2_header),
                               .clk_int_val            (   clk_int),
                               .clk90_int_val          (   clk90_int),
                               .sys_rst_val            (   sys_rst),
                               .sys_rst90_val          (   sys_rst90),
                               .sys_rst180_val         (   sys_rst180),
                               .sys_rst270_val         (   sys_rst270)
                               );                                            

   ddr1_test_bench ddr1_test_bench0 (
                                             .dip2                  ( dip1 ),
                                             .fpga_clk              ( clk_int),
                                             .fpga_rst90            ( sys_rst90),
                                             .fpga_rst0             ( sys_rst),
                                             .fpga_rst180           ( sys_rst180),
                                             .clk90                 ( clk90_int),
                                             
                                             .burst_done            ( burst_done_val1),
                                             .INIT_DONE             ( init_val1),
                                             .ar_done               ( ar_done_val1),
                                             .u_ack                 ( user_ack1),
                                             .u_data_val            ( user_data_val1),
                                             .u_data_o              ( controller1_data_output),
                                             .u_addr                ( u1_address),
                                             .u_cmd                 ( user_cmd1), 
                                             .u_data_i              ( u1_data_i ),
                                             .u_config_parms        ( u1_config_parms),                                             
                                             .led_error_output      ( led_error_output1),
                                             .lfsr_data_out         ( lfsr_data_out),
                                             .read_data_out         ( read_data_out),
                                             .data_valid_out        ( data_valid_out1)
                                            );
                                   


endmodule 
   
      
