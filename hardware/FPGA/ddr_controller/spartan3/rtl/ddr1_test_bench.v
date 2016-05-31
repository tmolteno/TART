//******************************************************************************
//
//  Xilinx, Inc. 2002                 www.xilinx.com
//
//
//*******************************************************************************
//
//  File name :       ddr1_test_bench.v
//
//  Description :     This module comprises the command, address and data associated
//                    with a write and a read command.
// 
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


module ddr1_test_bench(
                       dip2,
	               fpga_clk,
	               fpga_rst90,
	               fpga_rst0,
	               fpga_rst180,
	               clk90,
	           //-    clk180,
	               burst_done,
	               INIT_DONE,
	               ar_done,
	               u_ack,
	               u_data_val,
	               u_data_o,
	               u_addr,
	               u_cmd,
	               u_data_i,
	               u_config_parms,
	               led_error_output,
	               read_data_out,
	               lfsr_data_out, 
	               data_valid_out
	               
                       );
                       
   input          dip2;
   input          fpga_clk;
   input          fpga_rst90;
   input          fpga_rst0;   
   input 	  	  fpga_rst180;
   input 	        clk90; 
  // input 	        clk180;   
   input 	        INIT_DONE;
   input          ar_done;
   input 	        u_ack;
   input          u_data_val;      
   input [143:0]  u_data_o;               
   
   output 	       burst_done;
   output [23:0]  u_addr;
   output [2:0]   u_cmd;  
   output [143:0] u_data_i;
   output [9:0]   u_config_parms;
   output         led_error_output;
   output         [143:0] read_data_out;
   output         [143:0] lfsr_data_out; 
   output         data_valid_out;
 
   
   wire           clk;
   wire           addr_inc;
   wire           addr_rst;
   wire           cmd_ack;
   wire           cnt_roll;
   wire           ctrl_ready;
   wire           data_valid;
   wire           dly_inc;
   wire           dly_tc;
   wire           lfsr_rst;
   wire           r_w;
   wire           r_w0; 
   wire [143:0]   lfsr_data;
   wire [4:0]     test_cnt;
   wire [23:0]    addr_out;
   wire [6:0]     state;
   wire [7:0]     column_address_count;   
   reg            rst0_r;
   reg            rst90_r;
   reg            rst180_r;   
   reg            lfsr_ena;
   wire clk180 ;
   wire clk270 ;
//  Input : CONFIG REGISTER FORMAT 
// config_register = {   EMR(Enable/Disable DLL),
//                       BMR (Normal operation/Normal Operation with Reset DLL),
//                       BMR/EMR,
//                       CAS_latency (3),
//                       Burst type ,
//                       Burst_length (3) }
//
// Input : COMMAND REGISTER FORMAT
//          000  - NOP
//          001  - Precharge 
//          010  - Auto Refresh
//          011  - SElf REfresh
//          100  - Write Request
//          101  - Load Mode Register
//          110  - Read request
//          111  - Burst terminate
//
// Input : Address format
//   row address = input address(19 downto 8)
//   column addrs = input address( 7 downto 0)
//
// provide u_cmd
// provide u_config_parms
//
//  Terminals assignment 
//  Input terminals
assign clk180 = ~ clk;
assign clk270 = ~ clk90;

assign clk         = fpga_clk;
assign cmd_ack     = u_ack;
assign data_valid  = u_data_val;

assign ctrl_ready  = 1'b1;

assign u_addr[23:0] = addr_out[23:0];
assign u_data_i     = lfsr_data;
assign u_config_parms = 10'b0000110010; 

always @ (posedge clk)
begin
  rst0_r <= fpga_rst0;
end

always @ (posedge clk90)
begin
  rst90_r <= fpga_rst90;
end

always @ (posedge clk180)
begin
  rst180_r <= fpga_rst180;
end

always @ (posedge clk90)
begin
  if (rst90_r == 1'b1)
     lfsr_ena <= 1'b0;
  else if (((r_w == 1'b1) && (u_data_val == 1'b1)) || ((r_w == 1'b0) && (u_ack == 1'b1)))
     lfsr_ena <= 1'b1;
  else
     lfsr_ena <= 1'b0;
end

addr_gen  INST1  ( 
                  .clk(clk),
                  .rst(rst0_r),
                  .addr_rst(addr_rst),
                  .addr_inc(addr_inc),
                  .addr_out(addr_out),
                  .column_address(column_address_count),
                  .test_cnt_ena(ctrl_ready),
                  .test_out(test_cnt),
                  .burst_done(burst_done),
                  .cnt_roll(cnt_roll)
                  );
                  
cmd_fsm  INST_2  ( 
                  .clk(clk),
                  .clk90(clk90),
                  .cmd_ack(cmd_ack),
                  .cnt_roll(cnt_roll),
                  .dip2(dip2),
                  .dly_tc(dly_tc),
                  .r_w(r_w0),
                  .refresh_done(ar_done),
                  .rst(rst0_r),
                  .rst90(rst90_r),
                  .rst180(rst180_r),
                  .init_val(INIT_DONE),
                  .u_data_val(u_data_val),
                  .addr_inc(addr_inc),
                  .addr_rst(addr_rst),
                  .u_cmd(u_cmd),
                  .dly_inc(dly_inc),
                  .init_counter(state),
                  .lfsr_rst(lfsr_rst)
                  );

cmp_data  INST3  ( 
                  .clk(clk90),
                  .data_valid(data_valid),
                  .lfsr_data(lfsr_data),
                  .read_data(u_data_o),
                  .rst(rst90_r),
                  .led_error_output(led_error_output),
                  .read_data_out(read_data_out),
                  .lfsr_data_out(lfsr_data_out), 
                  .data_valid_out(data_valid_out)
                  );

lfsr32  INST5  ( 
                .clk(clk90),
                .rst(rst90_r),
                .lfsr_rst(lfsr_rst),
                .lfsr_ena(lfsr_ena),
                .lfsr_out(lfsr_data)
                );
                
r_w_dly  INST6  ( 
                 .clk90(clk90),
                 .rst90(rst90_r),
                 .clk0(clk),
                 .rst0(rst0_r),
                 .dly_inc(dly_inc),
                 .dly_tc(dly_tc),
                 .r_w0(r_w0), 
                 .r_w(r_w)
                 );
                 
endmodule
       