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
module data_read(clk90,
                 reset90_r,
                 ddr_dq_in,
                 read_valid_data_1,
                 fifo_00_wr_en,
                 fifo_10_wr_en,
                 fifo_20_wr_en,
                 fifo_30_wr_en,
                 fifo_40_wr_en,
                 fifo_50_wr_en,
                 fifo_60_wr_en,
                 fifo_70_wr_en,
                 fifo_80_wr_en,
                 fifo_01_wr_en,
                 fifo_11_wr_en,
                 fifo_21_wr_en,
                 fifo_31_wr_en,
                 fifo_41_wr_en,
                 fifo_51_wr_en,
                 fifo_61_wr_en,
                 fifo_71_wr_en,
                 fifo_81_wr_en,
                 fifo_00_wr_addr,
                 fifo_01_wr_addr,
                 fifo_10_wr_addr,
                 fifo_11_wr_addr,
                 fifo_20_wr_addr,
                 fifo_21_wr_addr,
                 fifo_30_wr_addr,
                 fifo_31_wr_addr,
                 fifo_40_wr_addr,
                 fifo_41_wr_addr,
                 fifo_50_wr_addr,
                 fifo_51_wr_addr,
                 fifo_60_wr_addr,
                 fifo_61_wr_addr,
                 fifo_70_wr_addr,
                 fifo_71_wr_addr,
                 fifo_80_wr_addr,
                 fifo_81_wr_addr,
                 fifo_00_wr_en_1,
                 fifo_10_wr_en_1,
                 fifo_20_wr_en_1,
                 fifo_30_wr_en_1,
                 fifo_40_wr_en_1,
                 fifo_50_wr_en_1,
                 fifo_60_wr_en_1,
                 fifo_70_wr_en_1,
                 fifo_80_wr_en_1,
                 fifo_01_wr_en_1,
                 fifo_11_wr_en_1,
                 fifo_21_wr_en_1,
                 fifo_31_wr_en_1,
                 fifo_41_wr_en_1,
                 fifo_51_wr_en_1,
                 fifo_61_wr_en_1,
                 fifo_71_wr_en_1,
                 fifo_81_wr_en_1,
                 fifo_00_wr_addr_1,
                 fifo_01_wr_addr_1,
                 fifo_10_wr_addr_1,
                 fifo_11_wr_addr_1,
                 fifo_20_wr_addr_1,
                 fifo_21_wr_addr_1,
                 fifo_30_wr_addr_1,
                 fifo_31_wr_addr_1,
                 fifo_40_wr_addr_1,
                 fifo_41_wr_addr_1,
                 fifo_50_wr_addr_1,
                 fifo_51_wr_addr_1,
                 fifo_60_wr_addr_1,
                 fifo_61_wr_addr_1,
                 fifo_70_wr_addr_1,
                 fifo_71_wr_addr_1,
                 fifo_80_wr_addr_1,
                 fifo_81_wr_addr_1,
                 dqs0_delayed_col1,
                 dqs1_delayed_col1,
                 dqs2_delayed_col1,
                 dqs3_delayed_col1,
                 dqs4_delayed_col1,
                 dqs5_delayed_col1,
                 dqs6_delayed_col1,
                 dqs7_delayed_col1,
                 dqs8_delayed_col1,    		     
                 dqs0_delayed_col0,
                 dqs1_delayed_col0,
                 dqs2_delayed_col0,
                 dqs3_delayed_col0,
                 dqs4_delayed_col0,
                 dqs5_delayed_col0,
                 dqs6_delayed_col0,
                 dqs7_delayed_col0,
                 dqs8_delayed_col0,
                 user_output_data,
                 fifo0_rd_addr_val,
                 fifo1_rd_addr_val); 

   
   input     clk90;
   input     reset90_r;
   input     [71:0] ddr_dq_in;   
   input     read_valid_data_1;    
   input     fifo_00_wr_en;
   input     fifo_10_wr_en;
   input     fifo_20_wr_en;
   input     fifo_30_wr_en;
   input     fifo_40_wr_en;
   input     fifo_50_wr_en;
   input     fifo_60_wr_en;
   input     fifo_70_wr_en;
   input     fifo_80_wr_en;
   input     fifo_01_wr_en;
   input     fifo_11_wr_en;
   input     fifo_21_wr_en;
   input     fifo_31_wr_en;
   input     fifo_41_wr_en;
   input     fifo_51_wr_en;
   input     fifo_61_wr_en;
   input     fifo_71_wr_en;
   input     fifo_81_wr_en;		
   input     [3:0] fifo_00_wr_addr;
   input     [3:0] fifo_01_wr_addr;
   input     [3:0] fifo_10_wr_addr;
   input     [3:0] fifo_11_wr_addr;
   input     [3:0] fifo_20_wr_addr;
   input     [3:0] fifo_21_wr_addr;
   input     [3:0] fifo_30_wr_addr;
   input     [3:0] fifo_31_wr_addr;
   input     [3:0] fifo_40_wr_addr;
   input     [3:0] fifo_41_wr_addr;
   input     [3:0] fifo_50_wr_addr;
   input     [3:0] fifo_51_wr_addr;
   input     [3:0] fifo_60_wr_addr;
   input     [3:0] fifo_61_wr_addr;
   input     [3:0] fifo_70_wr_addr;
   input     [3:0] fifo_71_wr_addr;
   input     [3:0] fifo_80_wr_addr;
   input     [3:0] fifo_81_wr_addr;
   input     fifo_00_wr_en_1;
   input     fifo_10_wr_en_1;
   input     fifo_20_wr_en_1;
   input     fifo_30_wr_en_1;
   input     fifo_40_wr_en_1;
   input     fifo_50_wr_en_1;
   input     fifo_60_wr_en_1;
   input     fifo_70_wr_en_1;
   input     fifo_80_wr_en_1;
   input     fifo_01_wr_en_1;
   input     fifo_11_wr_en_1;
   input     fifo_21_wr_en_1;
   input     fifo_31_wr_en_1;
   input     fifo_41_wr_en_1;
   input     fifo_51_wr_en_1;
   input     fifo_61_wr_en_1;
   input     fifo_71_wr_en_1;
   input     fifo_81_wr_en_1;		
   input     [3:0] fifo_00_wr_addr_1;
   input     [3:0] fifo_01_wr_addr_1;
   input     [3:0] fifo_10_wr_addr_1;
   input     [3:0] fifo_11_wr_addr_1;
   input     [3:0] fifo_20_wr_addr_1;
   input     [3:0] fifo_21_wr_addr_1;
   input     [3:0] fifo_30_wr_addr_1;
   input     [3:0] fifo_31_wr_addr_1;
   input     [3:0] fifo_40_wr_addr_1;
   input     [3:0] fifo_41_wr_addr_1;
   input     [3:0] fifo_50_wr_addr_1;
   input     [3:0] fifo_51_wr_addr_1;
   input     [3:0] fifo_60_wr_addr_1;
   input     [3:0] fifo_61_wr_addr_1;
   input     [3:0] fifo_70_wr_addr_1;
   input     [3:0] fifo_71_wr_addr_1;
   input     [3:0] fifo_80_wr_addr_1;
   input     [3:0] fifo_81_wr_addr_1;		
   input     dqs0_delayed_col1;
   input     dqs1_delayed_col1;
   input     dqs2_delayed_col1;
   input     dqs3_delayed_col1;
   input     dqs4_delayed_col1;
   input     dqs5_delayed_col1;
   input     dqs6_delayed_col1;
   input     dqs7_delayed_col1;
   input     dqs8_delayed_col1;     
   input     dqs0_delayed_col0;
   input     dqs1_delayed_col0;
   input     dqs2_delayed_col0;
   input     dqs3_delayed_col0;
   input     dqs4_delayed_col0;
   input     dqs5_delayed_col0;
   input     dqs6_delayed_col0;
   input     dqs7_delayed_col0;
   input     dqs8_delayed_col0;
  
  
   output     [143:0] user_output_data;
   output     [3:0] fifo0_rd_addr_val;
   output     [3:0] fifo1_rd_addr_val;     


   reg read_valid_data_1_r;
   reg read_valid_data_1_r1;
   reg read_valid_data_1_r2;
   reg [3:0] fifo00_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo01_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo10_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo11_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo20_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo21_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo30_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo31_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo40_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo41_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo50_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo51_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo60_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo61_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo70_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo71_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo80_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifo81_rd_addr_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [3:0] fifop_rd_addr_r ;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_00_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_01_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_10_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_11_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_20_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_21_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_30_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_31_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_40_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_41_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_50_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_51_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_60_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_61_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_70_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_71_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_80_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [7:0] fifo_81_data_out_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
   reg [143:0] first_sdr_data;

   wire [3:0] fifo00_rd_addr;
   wire [3:0] fifo01_rd_addr;
   wire [7:0] fifo_00_data_out;
   wire [7:0] fifo_01_data_out;
   wire [7:0] fifo_10_data_out;
   wire [7:0] fifo_11_data_out;
   wire [7:0] fifo_20_data_out;
   wire [7:0] fifo_21_data_out;
   wire [7:0] fifo_30_data_out;
   wire [7:0] fifo_31_data_out;
   wire [7:0] fifo_40_data_out;
   wire [7:0] fifo_41_data_out;
   wire [7:0] fifo_50_data_out;
   wire [7:0] fifo_51_data_out;
   wire [7:0] fifo_60_data_out;
   wire [7:0] fifo_61_data_out;
   wire [7:0] fifo_70_data_out;
   wire [7:0] fifo_71_data_out;
   wire [7:0] fifo_80_data_out;
   wire [7:0] fifo_81_data_out;
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
   assign user_output_data    = first_sdr_data;
   assign fifo0_rd_addr_val   = fifo01_rd_addr;
   assign fifo1_rd_addr_val   = fifo00_rd_addr;


   
   always@(posedge clk90)begin
      if(reset90_r)begin
         fifo_00_data_out_r <= 8'd0;
         fifo_01_data_out_r <= 8'd0;
         fifo_20_data_out_r <= 8'd0;
         fifo_21_data_out_r <= 8'd0;
         fifo_30_data_out_r <= 8'd0;
         fifo_31_data_out_r <= 8'd0;
         fifo_40_data_out_r <= 8'd0;
         fifo_41_data_out_r <= 8'd0;
         fifo_50_data_out_r <= 8'd0;
         fifo_51_data_out_r <= 8'd0;
         fifo_60_data_out_r <= 8'd0;
         fifo_61_data_out_r <= 8'd0;
         fifo_70_data_out_r <= 8'd0;
         fifo_71_data_out_r <= 8'd0;
         fifo_80_data_out_r <= 8'd0;
         fifo_81_data_out_r <= 8'd0;
      end
      else 
      begin
         fifo_00_data_out_r <= fifo_00_data_out;
         fifo_01_data_out_r <= fifo_01_data_out;
         fifo_10_data_out_r <= fifo_10_data_out;
         fifo_11_data_out_r <= fifo_11_data_out;
         fifo_20_data_out_r <= fifo_20_data_out;
         fifo_21_data_out_r <= fifo_21_data_out;
         fifo_30_data_out_r <= fifo_30_data_out;
         fifo_31_data_out_r <= fifo_31_data_out;
         fifo_40_data_out_r <= fifo_40_data_out;
         fifo_41_data_out_r <= fifo_41_data_out;
         fifo_50_data_out_r <= fifo_50_data_out;
         fifo_51_data_out_r <= fifo_51_data_out;
         fifo_60_data_out_r <= fifo_60_data_out;
         fifo_61_data_out_r <= fifo_61_data_out;
         fifo_70_data_out_r <= fifo_70_data_out;
         fifo_71_data_out_r <= fifo_71_data_out;
         fifo_80_data_out_r <= fifo_80_data_out;
         fifo_81_data_out_r <= fifo_81_data_out;
      end
   end

   always@(posedge clk90)begin
      if(reset90_r)begin
         fifo01_rd_addr_r <= 4'd0;
         fifo10_rd_addr_r <= 4'd0;
         fifo11_rd_addr_r <= 4'd0;
         fifo20_rd_addr_r <= 4'd0;
         fifo21_rd_addr_r <= 4'd0;
         fifo30_rd_addr_r <= 4'd0;
         fifo31_rd_addr_r <= 4'd0;
         fifo40_rd_addr_r <= 4'd0;
         fifo41_rd_addr_r <= 4'd0;
         fifo50_rd_addr_r <= 4'd0;
         fifo51_rd_addr_r <= 4'd0;
         fifo60_rd_addr_r <= 4'd0;
         fifo61_rd_addr_r <= 4'd0;
         fifo70_rd_addr_r <= 4'd0;
         fifo71_rd_addr_r <= 4'd0;
         fifo80_rd_addr_r <= 4'd0;
         fifo81_rd_addr_r <= 4'd0;
         fifop_rd_addr_r  <= 4'd0;
      end
      else begin
         fifo00_rd_addr_r <= fifo00_rd_addr;
         fifo01_rd_addr_r <= fifo00_rd_addr;
         fifo10_rd_addr_r <= fifo00_rd_addr;
         fifo11_rd_addr_r <= fifo00_rd_addr;
         fifo20_rd_addr_r <= fifo00_rd_addr;
         fifo21_rd_addr_r <= fifo00_rd_addr;
         fifo30_rd_addr_r <= fifo00_rd_addr;
         fifo31_rd_addr_r <= fifo00_rd_addr;
         fifo40_rd_addr_r <= fifo00_rd_addr;
         fifo41_rd_addr_r <= fifo00_rd_addr;
         fifo50_rd_addr_r <= fifo01_rd_addr;
         fifo51_rd_addr_r <= fifo01_rd_addr;
         fifo60_rd_addr_r <= fifo01_rd_addr;
         fifo61_rd_addr_r <= fifo01_rd_addr;
         fifo70_rd_addr_r <= fifo01_rd_addr;
         fifo71_rd_addr_r <= fifo01_rd_addr;
         fifo80_rd_addr_r <= fifo01_rd_addr;
         fifo81_rd_addr_r <= fifo01_rd_addr;
         fifop_rd_addr_r  <= fifo01_rd_addr; 
      end
   end


   always@(posedge clk90)begin
      if(reset90_r)begin
         first_sdr_data   <= 144'd0;  
         read_valid_data_1_r <= 1'b0;
         read_valid_data_1_r1 <= 1'b0;
         read_valid_data_1_r2 <= 1'b0;
      end
      else begin
         read_valid_data_1_r <= read_valid_data_1;
         read_valid_data_1_r1 <= read_valid_data_1_r;
         read_valid_data_1_r2 <= read_valid_data_1_r1;
         if(read_valid_data_1_r1)begin
            first_sdr_data  <= {fifo_00_data_out_r, fifo_01_data_out_r, 
            fifo_10_data_out_r,  fifo_11_data_out_r,  fifo_20_data_out_r, 
            fifo_21_data_out_r,  fifo_30_data_out_r,  fifo_31_data_out_r,  
	    fifo_40_data_out_r,  fifo_41_data_out_r,  fifo_50_data_out_r,  
	    fifo_51_data_out_r,  fifo_60_data_out_r,  fifo_61_data_out_r,  
	    fifo_70_data_out_r,  fifo_71_data_out_r,  fifo_80_data_out_r,  
	    fifo_81_data_out_r};
         end
      end
   end

   // rd address gray counters
   rd_gray_cntr fifo0_rd_addr_inst (.clk(clk90), .reset(reset90_r), .cnt_en(read_valid_data_1), 
                                    .rgc_gcnt(fifo00_rd_addr));

   rd_gray_cntr fifo1_rd_addr_inst (.clk(clk90), .reset(reset90_r), .cnt_en(read_valid_data_1), 
                                    .rgc_gcnt(fifo01_rd_addr));

         
   // 16X1 fifo instantations


   RAM16X1D fifo0_bit0  (.DPO (fifo_00_data_out[0]), .A0(fifo_00_wr_addr[0]), .A1(fifo_00_wr_addr[1]),
                          .A2(fifo_00_wr_addr[2]), .A3(fifo_00_wr_addr[3]), .D(ddr_dq_in[0]),
                          .DPRA0(fifo00_rd_addr_r[0]), .DPRA1(fifo00_rd_addr_r[1]), 
                          .DPRA2(fifo00_rd_addr_r[2]), .DPRA3(fifo00_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col1), .WE(fifo_00_wr_en));

   RAM16X1D fifo1_bit0  (.DPO (fifo_01_data_out[0]), .A0(fifo_01_wr_addr_1[0]), .A1(fifo_01_wr_addr_1[1]),
                          .A2(fifo_01_wr_addr_1[2]), .A3(fifo_01_wr_addr_1[3]), .D(ddr_dq_in[0]),
                          .DPRA0(fifo00_rd_addr_r[0]), .DPRA1(fifo00_rd_addr_r[1]), 
                          .DPRA2(fifo00_rd_addr_r[2]), .DPRA3(fifo00_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col1_n), .WE(fifo_01_wr_en_1));

   RAM16X1D fifo0_bit1  (.DPO (fifo_00_data_out[1]), .A0(fifo_00_wr_addr_1[0]), .A1(fifo_00_wr_addr_1[1]),
                          .A2(fifo_00_wr_addr_1[2]), .A3(fifo_00_wr_addr_1[3]), .D(ddr_dq_in[1]),
                          .DPRA0(fifo01_rd_addr_r[0]), .DPRA1(fifo01_rd_addr_r[1]), 
                          .DPRA2(fifo01_rd_addr_r[2]), .DPRA3(fifo01_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col0), .WE(fifo_00_wr_en_1));

   RAM16X1D fifo1_bit1  (.DPO (fifo_01_data_out[1]), .A0(fifo_01_wr_addr[0]), .A1(fifo_01_wr_addr[1]),
                          .A2(fifo_01_wr_addr[2]), .A3(fifo_01_wr_addr[3]), .D(ddr_dq_in[1]),
                          .DPRA0(fifo01_rd_addr_r[0]), .DPRA1(fifo01_rd_addr_r[1]), 
                          .DPRA2(fifo01_rd_addr_r[2]), .DPRA3(fifo01_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col0_n), .WE(fifo_01_wr_en));

  RAM16X1D fifo0_bit2  (.DPO (fifo_00_data_out[2]), .A0(fifo_00_wr_addr[0]), .A1(fifo_00_wr_addr[1]),
                          .A2(fifo_00_wr_addr[2]), .A3(fifo_00_wr_addr[3]), .D(ddr_dq_in[2]),
                          .DPRA0(fifo00_rd_addr_r[0]), .DPRA1(fifo00_rd_addr_r[1]), 
                          .DPRA2(fifo00_rd_addr_r[2]), .DPRA3(fifo00_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col1), .WE(fifo_00_wr_en));

   RAM16X1D fifo1_bit2  (.DPO (fifo_01_data_out[2]), .A0(fifo_01_wr_addr_1[0]), .A1(fifo_01_wr_addr_1[1]),
                          .A2(fifo_01_wr_addr_1[2]), .A3(fifo_01_wr_addr_1[3]), .D(ddr_dq_in[2]),
                          .DPRA0(fifo00_rd_addr_r[0]), .DPRA1(fifo00_rd_addr_r[1]), 
                          .DPRA2(fifo00_rd_addr_r[2]), .DPRA3(fifo00_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col1_n), .WE(fifo_01_wr_en_1));

   RAM16X1D fifo0_bit3  (.DPO (fifo_00_data_out[3]), .A0(fifo_00_wr_addr_1[0]), .A1(fifo_00_wr_addr_1[1]),
                          .A2(fifo_00_wr_addr_1[2]), .A3(fifo_00_wr_addr_1[3]), .D(ddr_dq_in[3]),
                          .DPRA0(fifo01_rd_addr_r[0]), .DPRA1(fifo01_rd_addr_r[1]), 
                          .DPRA2(fifo01_rd_addr_r[2]), .DPRA3(fifo01_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col0), .WE(fifo_00_wr_en_1));

   RAM16X1D fifo1_bit3  (.DPO (fifo_01_data_out[3]), .A0(fifo_01_wr_addr[0]), .A1(fifo_01_wr_addr[1]),
                          .A2(fifo_01_wr_addr[2]), .A3(fifo_01_wr_addr[3]), .D(ddr_dq_in[3]),
                          .DPRA0(fifo01_rd_addr_r[0]), .DPRA1(fifo01_rd_addr_r[1]), 
                          .DPRA2(fifo01_rd_addr_r[2]), .DPRA3(fifo01_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col0_n), .WE(fifo_01_wr_en));


   RAM16X1D fifo0_bit4  (.DPO (fifo_00_data_out[4]), .A0(fifo_00_wr_addr[0]), .A1(fifo_00_wr_addr[1]),
                          .A2(fifo_00_wr_addr[2]), .A3(fifo_00_wr_addr[3]), .D(ddr_dq_in[4]),
                          .DPRA0(fifo00_rd_addr_r[0]), .DPRA1(fifo00_rd_addr_r[1]), 
                          .DPRA2(fifo00_rd_addr_r[2]), .DPRA3(fifo00_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col1), .WE(fifo_00_wr_en));

   RAM16X1D fifo1_bit4  (.DPO (fifo_01_data_out[4]), .A0(fifo_01_wr_addr_1[0]), .A1(fifo_01_wr_addr_1[1]),
                          .A2(fifo_01_wr_addr_1[2]), .A3(fifo_01_wr_addr_1[3]), .D(ddr_dq_in[4]),
                          .DPRA0(fifo00_rd_addr_r[0]), .DPRA1(fifo00_rd_addr_r[1]), 
                          .DPRA2(fifo00_rd_addr_r[2]), .DPRA3(fifo00_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col1_n), .WE(fifo_01_wr_en_1));

   RAM16X1D fifo0_bit5  (.DPO (fifo_00_data_out[5]), .A0(fifo_00_wr_addr_1[0]), .A1(fifo_00_wr_addr_1[1]),
                          .A2(fifo_00_wr_addr_1[2]), .A3(fifo_00_wr_addr_1[3]), .D(ddr_dq_in[5]),
                          .DPRA0(fifo01_rd_addr_r[0]), .DPRA1(fifo01_rd_addr_r[1]), 
                          .DPRA2(fifo01_rd_addr_r[2]), .DPRA3(fifo01_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col0), .WE(fifo_00_wr_en_1));

   RAM16X1D fifo1_bit5  (.DPO (fifo_01_data_out[5]), .A0(fifo_01_wr_addr[0]), .A1(fifo_01_wr_addr[1]),
                          .A2(fifo_01_wr_addr[2]), .A3(fifo_01_wr_addr[3]), .D(ddr_dq_in[5]),
                          .DPRA0(fifo01_rd_addr_r[0]), .DPRA1(fifo01_rd_addr_r[1]), 
                          .DPRA2(fifo01_rd_addr_r[2]), .DPRA3(fifo01_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col0_n), .WE(fifo_01_wr_en));

   RAM16X1D fifo0_bit6  (.DPO (fifo_00_data_out[6]), .A0(fifo_00_wr_addr[0]), .A1(fifo_00_wr_addr[1]),
                          .A2(fifo_00_wr_addr[2]), .A3(fifo_00_wr_addr[3]), .D(ddr_dq_in[6]),
                          .DPRA0(fifo00_rd_addr_r[0]), .DPRA1(fifo00_rd_addr_r[1]), 
                          .DPRA2(fifo00_rd_addr_r[2]), .DPRA3(fifo00_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col1), .WE(fifo_00_wr_en));

   RAM16X1D fifo1_bit6  (.DPO (fifo_01_data_out[6]), .A0(fifo_01_wr_addr_1[0]), .A1(fifo_01_wr_addr_1[1]),
                          .A2(fifo_01_wr_addr_1[2]), .A3(fifo_01_wr_addr_1[3]), .D(ddr_dq_in[6]),
                          .DPRA0(fifo00_rd_addr_r[0]), .DPRA1(fifo00_rd_addr_r[1]), 
                          .DPRA2(fifo00_rd_addr_r[2]), .DPRA3(fifo00_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col1_n), .WE(fifo_01_wr_en_1));

   RAM16X1D fifo0_bit7  (.DPO (fifo_00_data_out[7]), .A0(fifo_00_wr_addr_1[0]), .A1(fifo_00_wr_addr_1[1]),
                          .A2(fifo_00_wr_addr_1[2]), .A3(fifo_00_wr_addr_1[3]), .D(ddr_dq_in[7]),
                          .DPRA0(fifo01_rd_addr_r[0]), .DPRA1(fifo01_rd_addr_r[1]), 
                          .DPRA2(fifo01_rd_addr_r[2]), .DPRA3(fifo01_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col0), .WE(fifo_00_wr_en_1));

   RAM16X1D fifo1_bit7  (.DPO (fifo_01_data_out[7]), .A0(fifo_01_wr_addr[0]), .A1(fifo_01_wr_addr[1]),
                          .A2(fifo_01_wr_addr[2]), .A3(fifo_01_wr_addr[3]), .D(ddr_dq_in[7]),
                          .DPRA0(fifo01_rd_addr_r[0]), .DPRA1(fifo01_rd_addr_r[1]), 
                          .DPRA2(fifo01_rd_addr_r[2]), .DPRA3(fifo01_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs0_delayed_col0_n), .WE(fifo_01_wr_en));


   RAM16X1D fifo0_bit8  (.DPO (fifo_10_data_out[0]), .A0(fifo_10_wr_addr[0]), .A1(fifo_10_wr_addr[1]),
                          .A2(fifo_10_wr_addr[2]), .A3(fifo_10_wr_addr[3]), .D(ddr_dq_in[8]),
                          .DPRA0(fifo10_rd_addr_r[0]), .DPRA1(fifo10_rd_addr_r[1]), 
                          .DPRA2(fifo10_rd_addr_r[2]), .DPRA3(fifo10_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col1), .WE(fifo_10_wr_en));

   RAM16X1D fifo1_bit8  (.DPO (fifo_11_data_out[0]), .A0(fifo_11_wr_addr_1[0]), .A1(fifo_11_wr_addr_1[1]),
                          .A2(fifo_11_wr_addr_1[2]), .A3(fifo_11_wr_addr_1[3]), .D(ddr_dq_in[8]),
                          .DPRA0(fifo10_rd_addr_r[0]), .DPRA1(fifo10_rd_addr_r[1]), 
                          .DPRA2(fifo10_rd_addr_r[2]), .DPRA3(fifo10_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col1_n), .WE(fifo_11_wr_en_1));

   RAM16X1D fifo0_bit9  (.DPO (fifo_10_data_out[1]), .A0(fifo_10_wr_addr_1[0]), .A1(fifo_10_wr_addr_1[1]),
                          .A2(fifo_10_wr_addr_1[2]), .A3(fifo_10_wr_addr_1[3]), .D(ddr_dq_in[9]),
                          .DPRA0(fifo11_rd_addr_r[0]), .DPRA1(fifo11_rd_addr_r[1]), 
                          .DPRA2(fifo11_rd_addr_r[2]), .DPRA3(fifo11_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col0), .WE(fifo_10_wr_en_1));

   RAM16X1D fifo1_bit9  (.DPO (fifo_11_data_out[1]), .A0(fifo_11_wr_addr[0]), .A1(fifo_11_wr_addr[1]),
                          .A2(fifo_11_wr_addr[2]), .A3(fifo_11_wr_addr[3]), .D(ddr_dq_in[9]),
                          .DPRA0(fifo11_rd_addr_r[0]), .DPRA1(fifo11_rd_addr_r[1]), 
                          .DPRA2(fifo11_rd_addr_r[2]), .DPRA3(fifo11_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col0_n), .WE(fifo_11_wr_en));

  RAM16X1D fifo0_bit10  (.DPO (fifo_10_data_out[2]), .A0(fifo_10_wr_addr[0]), .A1(fifo_10_wr_addr[1]),
                          .A2(fifo_10_wr_addr[2]), .A3(fifo_10_wr_addr[3]), .D(ddr_dq_in[10]),
                          .DPRA0(fifo10_rd_addr_r[0]), .DPRA1(fifo10_rd_addr_r[1]), 
                          .DPRA2(fifo10_rd_addr_r[2]), .DPRA3(fifo10_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col1), .WE(fifo_10_wr_en));

   RAM16X1D fifo1_bit10  (.DPO (fifo_11_data_out[2]), .A0(fifo_11_wr_addr_1[0]), .A1(fifo_11_wr_addr_1[1]),
                          .A2(fifo_11_wr_addr_1[2]), .A3(fifo_11_wr_addr_1[3]), .D(ddr_dq_in[10]),
                          .DPRA0(fifo10_rd_addr_r[0]), .DPRA1(fifo10_rd_addr_r[1]), 
                          .DPRA2(fifo10_rd_addr_r[2]), .DPRA3(fifo10_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col1_n), .WE(fifo_11_wr_en_1));

   RAM16X1D fifo0_bit11  (.DPO (fifo_10_data_out[3]), .A0(fifo_10_wr_addr_1[0]), .A1(fifo_10_wr_addr_1[1]),
                          .A2(fifo_10_wr_addr_1[2]), .A3(fifo_10_wr_addr_1[3]), .D(ddr_dq_in[11]),
                          .DPRA0(fifo11_rd_addr_r[0]), .DPRA1(fifo11_rd_addr_r[1]), 
                          .DPRA2(fifo11_rd_addr_r[2]), .DPRA3(fifo11_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col0), .WE(fifo_10_wr_en_1));

   RAM16X1D fifo1_bit11  (.DPO (fifo_11_data_out[3]), .A0(fifo_11_wr_addr[0]), .A1(fifo_11_wr_addr[1]),
                          .A2(fifo_11_wr_addr[2]), .A3(fifo_11_wr_addr[3]), .D(ddr_dq_in[11]),
                          .DPRA0(fifo11_rd_addr_r[0]), .DPRA1(fifo11_rd_addr_r[1]), 
                          .DPRA2(fifo11_rd_addr_r[2]), .DPRA3(fifo11_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col0_n), .WE(fifo_11_wr_en));


   RAM16X1D fifo0_bit12  (.DPO (fifo_10_data_out[4]), .A0(fifo_10_wr_addr[0]), .A1(fifo_10_wr_addr[1]),
                          .A2(fifo_10_wr_addr[2]), .A3(fifo_10_wr_addr[3]), .D(ddr_dq_in[12]),
                          .DPRA0(fifo10_rd_addr_r[0]), .DPRA1(fifo10_rd_addr_r[1]), 
                          .DPRA2(fifo10_rd_addr_r[2]), .DPRA3(fifo10_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col1), .WE(fifo_10_wr_en));

   RAM16X1D fifo1_bit12  (.DPO (fifo_11_data_out[4]), .A0(fifo_11_wr_addr_1[0]), .A1(fifo_11_wr_addr_1[1]),
                          .A2(fifo_11_wr_addr_1[2]), .A3(fifo_11_wr_addr_1[3]), .D(ddr_dq_in[12]),
                          .DPRA0(fifo10_rd_addr_r[0]), .DPRA1(fifo10_rd_addr_r[1]), 
                          .DPRA2(fifo10_rd_addr_r[2]), .DPRA3(fifo10_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col1_n), .WE(fifo_11_wr_en_1));

   RAM16X1D fifo0_bit13  (.DPO (fifo_10_data_out[5]), .A0(fifo_10_wr_addr_1[0]), .A1(fifo_10_wr_addr_1[1]),
                          .A2(fifo_10_wr_addr_1[2]), .A3(fifo_10_wr_addr_1[3]), .D(ddr_dq_in[13]),
                          .DPRA0(fifo11_rd_addr_r[0]), .DPRA1(fifo11_rd_addr_r[1]), 
                          .DPRA2(fifo11_rd_addr_r[2]), .DPRA3(fifo11_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col0), .WE(fifo_10_wr_en_1));

   RAM16X1D fifo1_bit13  (.DPO (fifo_11_data_out[5]), .A0(fifo_11_wr_addr[0]), .A1(fifo_11_wr_addr[1]),
                          .A2(fifo_11_wr_addr[2]), .A3(fifo_11_wr_addr[3]), .D(ddr_dq_in[13]),
                          .DPRA0(fifo11_rd_addr_r[0]), .DPRA1(fifo11_rd_addr_r[1]), 
                          .DPRA2(fifo11_rd_addr_r[2]), .DPRA3(fifo11_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col0_n), .WE(fifo_11_wr_en));

   RAM16X1D fifo0_bit14  (.DPO (fifo_10_data_out[6]), .A0(fifo_10_wr_addr[0]), .A1(fifo_10_wr_addr[1]),
                          .A2(fifo_10_wr_addr[2]), .A3(fifo_10_wr_addr[3]), .D(ddr_dq_in[14]),
                          .DPRA0(fifo10_rd_addr_r[0]), .DPRA1(fifo10_rd_addr_r[1]), 
                          .DPRA2(fifo10_rd_addr_r[2]), .DPRA3(fifo10_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col1), .WE(fifo_10_wr_en));

   RAM16X1D fifo1_bit14  (.DPO (fifo_11_data_out[6]), .A0(fifo_11_wr_addr_1[0]), .A1(fifo_11_wr_addr_1[1]),
                          .A2(fifo_11_wr_addr_1[2]), .A3(fifo_11_wr_addr_1[3]), .D(ddr_dq_in[14]),
                          .DPRA0(fifo10_rd_addr_r[0]), .DPRA1(fifo10_rd_addr_r[1]), 
                          .DPRA2(fifo10_rd_addr_r[2]), .DPRA3(fifo10_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col1_n), .WE(fifo_11_wr_en_1));

   RAM16X1D fifo0_bit15  (.DPO (fifo_10_data_out[7]), .A0(fifo_10_wr_addr_1[0]), .A1(fifo_10_wr_addr_1[1]),
                          .A2(fifo_10_wr_addr_1[2]), .A3(fifo_10_wr_addr_1[3]), .D(ddr_dq_in[15]),
                          .DPRA0(fifo11_rd_addr_r[0]), .DPRA1(fifo11_rd_addr_r[1]), 
                          .DPRA2(fifo11_rd_addr_r[2]), .DPRA3(fifo11_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col0), .WE(fifo_10_wr_en_1));

   RAM16X1D fifo1_bit15  (.DPO (fifo_11_data_out[7]), .A0(fifo_11_wr_addr[0]), .A1(fifo_11_wr_addr[1]),
                          .A2(fifo_11_wr_addr[2]), .A3(fifo_11_wr_addr[3]), .D(ddr_dq_in[15]),
                          .DPRA0(fifo11_rd_addr_r[0]), .DPRA1(fifo11_rd_addr_r[1]), 
                          .DPRA2(fifo11_rd_addr_r[2]), .DPRA3(fifo11_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs1_delayed_col0_n), .WE(fifo_11_wr_en));


   RAM16X1D fifo0_bit16  (.DPO (fifo_20_data_out[0]), .A0(fifo_20_wr_addr[0]), .A1(fifo_20_wr_addr[1]),
                          .A2(fifo_20_wr_addr[2]), .A3(fifo_20_wr_addr[3]), .D(ddr_dq_in[16]),
                          .DPRA0(fifo20_rd_addr_r[0]), .DPRA1(fifo20_rd_addr_r[1]), 
                          .DPRA2(fifo20_rd_addr_r[2]), .DPRA3(fifo20_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col1), .WE(fifo_20_wr_en));

   RAM16X1D fifo1_bit16  (.DPO (fifo_21_data_out[0]), .A0(fifo_21_wr_addr_1[0]), .A1(fifo_21_wr_addr_1[1]),
                          .A2(fifo_21_wr_addr_1[2]), .A3(fifo_21_wr_addr_1[3]), .D(ddr_dq_in[16]),
                          .DPRA0(fifo20_rd_addr_r[0]), .DPRA1(fifo20_rd_addr_r[1]), 
                          .DPRA2(fifo20_rd_addr_r[2]), .DPRA3(fifo20_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col1_n), .WE(fifo_21_wr_en_1));

   RAM16X1D fifo0_bit17  (.DPO (fifo_20_data_out[1]), .A0(fifo_20_wr_addr_1[0]), .A1(fifo_20_wr_addr_1[1]),
                          .A2(fifo_20_wr_addr_1[2]), .A3(fifo_20_wr_addr_1[3]), .D(ddr_dq_in[17]),
                          .DPRA0(fifo21_rd_addr_r[0]), .DPRA1(fifo21_rd_addr_r[1]), 
                          .DPRA2(fifo21_rd_addr_r[2]), .DPRA3(fifo21_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col0), .WE(fifo_20_wr_en_1));

   RAM16X1D fifo1_bit17  (.DPO (fifo_21_data_out[1]), .A0(fifo_21_wr_addr[0]), .A1(fifo_21_wr_addr[1]),
                          .A2(fifo_21_wr_addr[2]), .A3(fifo_21_wr_addr[3]), .D(ddr_dq_in[17]),
                          .DPRA0(fifo21_rd_addr_r[0]), .DPRA1(fifo21_rd_addr_r[1]), 
                          .DPRA2(fifo21_rd_addr_r[2]), .DPRA3(fifo21_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col0_n), .WE(fifo_21_wr_en));

  RAM16X1D fifo0_bit18  (.DPO (fifo_20_data_out[2]), .A0(fifo_20_wr_addr[0]), .A1(fifo_20_wr_addr[1]),
                          .A2(fifo_20_wr_addr[2]), .A3(fifo_20_wr_addr[3]), .D(ddr_dq_in[18]),
                          .DPRA0(fifo20_rd_addr_r[0]), .DPRA1(fifo20_rd_addr_r[1]), 
                          .DPRA2(fifo20_rd_addr_r[2]), .DPRA3(fifo20_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col1), .WE(fifo_20_wr_en));

   RAM16X1D fifo1_bit18  (.DPO (fifo_21_data_out[2]), .A0(fifo_21_wr_addr_1[0]), .A1(fifo_21_wr_addr_1[1]),
                          .A2(fifo_21_wr_addr_1[2]), .A3(fifo_21_wr_addr_1[3]), .D(ddr_dq_in[18]),
                          .DPRA0(fifo20_rd_addr_r[0]), .DPRA1(fifo20_rd_addr_r[1]), 
                          .DPRA2(fifo20_rd_addr_r[2]), .DPRA3(fifo20_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col1_n), .WE(fifo_21_wr_en_1));

   RAM16X1D fifo0_bit19  (.DPO (fifo_20_data_out[3]), .A0(fifo_20_wr_addr_1[0]), .A1(fifo_20_wr_addr_1[1]),
                          .A2(fifo_20_wr_addr_1[2]), .A3(fifo_20_wr_addr_1[3]), .D(ddr_dq_in[19]),
                          .DPRA0(fifo21_rd_addr_r[0]), .DPRA1(fifo21_rd_addr_r[1]), 
                          .DPRA2(fifo21_rd_addr_r[2]), .DPRA3(fifo21_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col0), .WE(fifo_20_wr_en_1));

   RAM16X1D fifo1_bit19  (.DPO (fifo_21_data_out[3]), .A0(fifo_21_wr_addr[0]), .A1(fifo_21_wr_addr[1]),
                          .A2(fifo_21_wr_addr[2]), .A3(fifo_21_wr_addr[3]), .D(ddr_dq_in[19]),
                          .DPRA0(fifo21_rd_addr_r[0]), .DPRA1(fifo21_rd_addr_r[1]), 
                          .DPRA2(fifo21_rd_addr_r[2]), .DPRA3(fifo21_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col0_n), .WE(fifo_21_wr_en));


   RAM16X1D fifo0_bit20  (.DPO (fifo_20_data_out[4]), .A0(fifo_20_wr_addr[0]), .A1(fifo_20_wr_addr[1]),
                          .A2(fifo_20_wr_addr[2]), .A3(fifo_20_wr_addr[3]), .D(ddr_dq_in[20]),
                          .DPRA0(fifo20_rd_addr_r[0]), .DPRA1(fifo20_rd_addr_r[1]), 
                          .DPRA2(fifo20_rd_addr_r[2]), .DPRA3(fifo20_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col1), .WE(fifo_20_wr_en));

   RAM16X1D fifo1_bit20  (.DPO (fifo_21_data_out[4]), .A0(fifo_21_wr_addr_1[0]), .A1(fifo_21_wr_addr_1[1]),
                          .A2(fifo_21_wr_addr_1[2]), .A3(fifo_21_wr_addr_1[3]), .D(ddr_dq_in[20]),
                          .DPRA0(fifo20_rd_addr_r[0]), .DPRA1(fifo20_rd_addr_r[1]), 
                          .DPRA2(fifo20_rd_addr_r[2]), .DPRA3(fifo20_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col1_n), .WE(fifo_21_wr_en_1));

   RAM16X1D fifo0_bit21  (.DPO (fifo_20_data_out[5]), .A0(fifo_20_wr_addr_1[0]), .A1(fifo_20_wr_addr_1[1]),
                          .A2(fifo_20_wr_addr_1[2]), .A3(fifo_20_wr_addr_1[3]), .D(ddr_dq_in[21]),
                          .DPRA0(fifo21_rd_addr_r[0]), .DPRA1(fifo21_rd_addr_r[1]), 
                          .DPRA2(fifo21_rd_addr_r[2]), .DPRA3(fifo21_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col0), .WE(fifo_20_wr_en_1));

   RAM16X1D fifo1_bit21  (.DPO (fifo_21_data_out[5]), .A0(fifo_21_wr_addr[0]), .A1(fifo_21_wr_addr[1]),
                          .A2(fifo_21_wr_addr[2]), .A3(fifo_21_wr_addr[3]), .D(ddr_dq_in[21]),
                          .DPRA0(fifo21_rd_addr_r[0]), .DPRA1(fifo21_rd_addr_r[1]), 
                          .DPRA2(fifo21_rd_addr_r[2]), .DPRA3(fifo21_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col0_n), .WE(fifo_21_wr_en));

   RAM16X1D fifo0_bit22  (.DPO (fifo_20_data_out[6]), .A0(fifo_20_wr_addr[0]), .A1(fifo_20_wr_addr[1]),
                          .A2(fifo_20_wr_addr[2]), .A3(fifo_20_wr_addr[3]), .D(ddr_dq_in[22]),
                          .DPRA0(fifo20_rd_addr_r[0]), .DPRA1(fifo20_rd_addr_r[1]), 
                          .DPRA2(fifo20_rd_addr_r[2]), .DPRA3(fifo20_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col1), .WE(fifo_20_wr_en));

   RAM16X1D fifo1_bit22  (.DPO (fifo_21_data_out[6]), .A0(fifo_21_wr_addr_1[0]), .A1(fifo_21_wr_addr_1[1]),
                          .A2(fifo_21_wr_addr_1[2]), .A3(fifo_21_wr_addr_1[3]), .D(ddr_dq_in[22]),
                          .DPRA0(fifo20_rd_addr_r[0]), .DPRA1(fifo20_rd_addr_r[1]), 
                          .DPRA2(fifo20_rd_addr_r[2]), .DPRA3(fifo20_rd_addr_r[3]),  .SPO(open),
                          .WCLK(dqs2_delayed_col1_n), .WE(fifo_21_wr_en_1));

   RAM16X1D fifo0_bit23  (.DPO (fifo_20_data_out[7]), .A0(fifo_20_wr_addr_1[0]), .A1(fifo_20_wr_addr_1[1]),
                          .A2(fifo_20_wr_addr_1[2]), .A3(fifo_20_wr_addr_1[3]), .D(ddr_dq_in[23]),
                          .DPRA0(fifo21_rd_addr_r[0]), .DPRA1(fifo21_rd_addr_r[1]), 
                          .DPRA2(fifo21_rd_addr_r[2]), .DPRA3(fifo21_rd_addr_r[3]),  .SPO(open),
                          .WCLK(dqs2_delayed_col0), .WE(fifo_20_wr_en_1));

   RAM16X1D fifo1_bit23  (.DPO (fifo_21_data_out[7]), .A0(fifo_21_wr_addr[0]), .A1(fifo_21_wr_addr[1]),
                          .A2(fifo_21_wr_addr[2]), .A3(fifo_21_wr_addr[3]), .D(ddr_dq_in[23]),
                          .DPRA0(fifo21_rd_addr_r[0]), .DPRA1(fifo21_rd_addr_r[1]), 
                          .DPRA2(fifo21_rd_addr_r[2]), .DPRA3(fifo21_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs2_delayed_col0_n), .WE(fifo_21_wr_en));

   RAM16X1D fifo0_bit24  (.DPO (fifo_30_data_out[0]), .A0(fifo_30_wr_addr[0]), .A1(fifo_30_wr_addr[1]),
                          .A2(fifo_30_wr_addr[2]), .A3(fifo_30_wr_addr[3]), .D(ddr_dq_in[24]),
                          .DPRA0(fifo30_rd_addr_r[0]), .DPRA1(fifo30_rd_addr_r[1]), 
                          .DPRA2(fifo30_rd_addr_r[2]), .DPRA3(fifo30_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col1), .WE(fifo_30_wr_en));

   RAM16X1D fifo1_bit24  (.DPO (fifo_31_data_out[0]), .A0(fifo_31_wr_addr_1[0]), .A1(fifo_31_wr_addr_1[1]),
                          .A2(fifo_31_wr_addr_1[2]), .A3(fifo_31_wr_addr_1[3]), .D(ddr_dq_in[24]),
                          .DPRA0(fifo30_rd_addr_r[0]), .DPRA1(fifo30_rd_addr_r[1]), 
                          .DPRA2(fifo30_rd_addr_r[2]), .DPRA3(fifo30_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col1_n), .WE(fifo_31_wr_en_1));

   RAM16X1D fifo0_bit25  (.DPO (fifo_30_data_out[1]), .A0(fifo_30_wr_addr_1[0]), .A1(fifo_30_wr_addr_1[1]),
                          .A2(fifo_30_wr_addr_1[2]), .A3(fifo_30_wr_addr_1[3]), .D(ddr_dq_in[25]),
                          .DPRA0(fifo31_rd_addr_r[0]), .DPRA1(fifo31_rd_addr_r[1]), 
                          .DPRA2(fifo31_rd_addr_r[2]), .DPRA3(fifo31_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col0), .WE(fifo_30_wr_en_1));

   RAM16X1D fifo1_bit25  (.DPO (fifo_31_data_out[1]), .A0(fifo_31_wr_addr[0]), .A1(fifo_31_wr_addr[1]),
                          .A2(fifo_31_wr_addr[2]), .A3(fifo_31_wr_addr[3]), .D(ddr_dq_in[25]),
                          .DPRA0(fifo31_rd_addr_r[0]), .DPRA1(fifo31_rd_addr_r[1]), 
                          .DPRA2(fifo31_rd_addr_r[2]), .DPRA3(fifo31_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col0_n), .WE(fifo_31_wr_en));

  RAM16X1D fifo0_bit26  (.DPO (fifo_30_data_out[2]), .A0(fifo_30_wr_addr[0]), .A1(fifo_30_wr_addr[1]),
                          .A2(fifo_30_wr_addr[2]), .A3(fifo_30_wr_addr[3]), .D(ddr_dq_in[26]),
                          .DPRA0(fifo30_rd_addr_r[0]), .DPRA1(fifo30_rd_addr_r[1]), 
                          .DPRA2(fifo30_rd_addr_r[2]), .DPRA3(fifo30_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col1), .WE(fifo_30_wr_en));

   RAM16X1D fifo1_bit26  (.DPO (fifo_31_data_out[2]), .A0(fifo_31_wr_addr_1[0]), .A1(fifo_31_wr_addr_1[1]),
                          .A2(fifo_31_wr_addr_1[2]), .A3(fifo_31_wr_addr_1[3]), .D(ddr_dq_in[26]),
                          .DPRA0(fifo30_rd_addr_r[0]), .DPRA1(fifo30_rd_addr_r[1]), 
                          .DPRA2(fifo30_rd_addr_r[2]), .DPRA3(fifo30_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col1_n), .WE(fifo_31_wr_en_1));

   RAM16X1D fifo0_bit27  (.DPO (fifo_30_data_out[3]), .A0(fifo_30_wr_addr_1[0]), .A1(fifo_30_wr_addr_1[1]),
                          .A2(fifo_30_wr_addr_1[2]), .A3(fifo_30_wr_addr_1[3]), .D(ddr_dq_in[27]),
                          .DPRA0(fifo31_rd_addr_r[0]), .DPRA1(fifo31_rd_addr_r[1]), 
                          .DPRA2(fifo31_rd_addr_r[2]), .DPRA3(fifo31_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col0), .WE(fifo_30_wr_en_1));

   RAM16X1D fifo1_bit27  (.DPO (fifo_31_data_out[3]), .A0(fifo_31_wr_addr[0]), .A1(fifo_31_wr_addr[1]),
                          .A2(fifo_31_wr_addr[2]), .A3(fifo_31_wr_addr[3]), .D(ddr_dq_in[27]),
                          .DPRA0(fifo31_rd_addr_r[0]), .DPRA1(fifo31_rd_addr_r[1]), 
                          .DPRA2(fifo31_rd_addr_r[2]), .DPRA3(fifo31_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col0_n), .WE(fifo_31_wr_en));


   RAM16X1D fifo0_bit28  (.DPO (fifo_30_data_out[4]), .A0(fifo_30_wr_addr[0]), .A1(fifo_30_wr_addr[1]),
                          .A2(fifo_30_wr_addr[2]), .A3(fifo_30_wr_addr[3]), .D(ddr_dq_in[28]),
                          .DPRA0(fifo30_rd_addr_r[0]), .DPRA1(fifo30_rd_addr_r[1]), 
                          .DPRA2(fifo30_rd_addr_r[2]), .DPRA3(fifo30_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col1), .WE(fifo_30_wr_en));

   RAM16X1D fifo1_bit28  (.DPO (fifo_31_data_out[4]), .A0(fifo_31_wr_addr_1[0]), .A1(fifo_31_wr_addr_1[1]),
                          .A2(fifo_31_wr_addr_1[2]), .A3(fifo_31_wr_addr_1[3]), .D(ddr_dq_in[28]),
                          .DPRA0(fifo30_rd_addr_r[0]), .DPRA1(fifo30_rd_addr_r[1]), 
                          .DPRA2(fifo30_rd_addr_r[2]), .DPRA3(fifo30_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col1_n), .WE(fifo_31_wr_en_1));

   RAM16X1D fifo0_bit29  (.DPO (fifo_30_data_out[5]), .A0(fifo_30_wr_addr_1[0]), .A1(fifo_30_wr_addr_1[1]),
                          .A2(fifo_30_wr_addr_1[2]), .A3(fifo_30_wr_addr_1[3]), .D(ddr_dq_in[29]),
                          .DPRA0(fifo31_rd_addr_r[0]), .DPRA1(fifo31_rd_addr_r[1]), 
                          .DPRA2(fifo31_rd_addr_r[2]), .DPRA3(fifo31_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col0), .WE(fifo_30_wr_en_1));

   RAM16X1D fifo1_bit29  (.DPO (fifo_31_data_out[5]), .A0(fifo_31_wr_addr[0]), .A1(fifo_31_wr_addr[1]),
                          .A2(fifo_31_wr_addr[2]), .A3(fifo_31_wr_addr[3]), .D(ddr_dq_in[29]),
                          .DPRA0(fifo31_rd_addr_r[0]), .DPRA1(fifo31_rd_addr_r[1]), 
                          .DPRA2(fifo31_rd_addr_r[2]), .DPRA3(fifo31_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col0_n), .WE(fifo_31_wr_en));

   RAM16X1D fifo0_bit30  (.DPO (fifo_30_data_out[6]), .A0(fifo_30_wr_addr[0]), .A1(fifo_30_wr_addr[1]),
                          .A2(fifo_30_wr_addr[2]), .A3(fifo_30_wr_addr[3]), .D(ddr_dq_in[30]),
                          .DPRA0(fifo30_rd_addr_r[0]), .DPRA1(fifo30_rd_addr_r[1]), 
                          .DPRA2(fifo30_rd_addr_r[2]), .DPRA3(fifo30_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col1), .WE(fifo_30_wr_en));

   RAM16X1D fifo1_bit30  (.DPO (fifo_31_data_out[6]), .A0(fifo_31_wr_addr_1[0]), .A1(fifo_31_wr_addr_1[1]),
                          .A2(fifo_31_wr_addr_1[2]), .A3(fifo_31_wr_addr_1[3]), .D(ddr_dq_in[30]),
                          .DPRA0(fifo30_rd_addr_r[0]), .DPRA1(fifo30_rd_addr_r[1]), 
                          .DPRA2(fifo30_rd_addr_r[2]), .DPRA3(fifo30_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col1_n), .WE(fifo_31_wr_en_1));

   RAM16X1D fifo0_bit31  (.DPO (fifo_30_data_out[7]), .A0(fifo_30_wr_addr_1[0]), .A1(fifo_30_wr_addr_1[1]),
                          .A2(fifo_30_wr_addr_1[2]), .A3(fifo_30_wr_addr_1[3]), .D(ddr_dq_in[31]),
                          .DPRA0(fifo31_rd_addr_r[0]), .DPRA1(fifo31_rd_addr_r[1]), 
                          .DPRA2(fifo31_rd_addr_r[2]), .DPRA3(fifo31_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col0), .WE(fifo_30_wr_en_1));

   RAM16X1D fifo1_bit31  (.DPO (fifo_31_data_out[7]), .A0(fifo_31_wr_addr[0]), .A1(fifo_31_wr_addr[1]),
                          .A2(fifo_31_wr_addr[2]), .A3(fifo_31_wr_addr[3]), .D(ddr_dq_in[31]),
                          .DPRA0(fifo31_rd_addr_r[0]), .DPRA1(fifo31_rd_addr_r[1]), 
                          .DPRA2(fifo31_rd_addr_r[2]), .DPRA3(fifo31_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs3_delayed_col0_n), .WE(fifo_31_wr_en));



   RAM16X1D fifo0_bit32  (.DPO (fifo_40_data_out[0]), .A0(fifo_40_wr_addr[0]), .A1(fifo_40_wr_addr[1]),
                          .A2(fifo_40_wr_addr[2]), .A3(fifo_40_wr_addr[3]), .D(ddr_dq_in[32]),
                          .DPRA0(fifo40_rd_addr_r[0]), .DPRA1(fifo40_rd_addr_r[1]), 
                          .DPRA2(fifo40_rd_addr_r[2]), .DPRA3(fifo40_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col1), .WE(fifo_40_wr_en));

   RAM16X1D fifo1_bit32  (.DPO (fifo_41_data_out[0]), .A0(fifo_41_wr_addr_1[0]), .A1(fifo_41_wr_addr_1[1]),
                          .A2(fifo_41_wr_addr_1[2]), .A3(fifo_41_wr_addr_1[3]), .D(ddr_dq_in[32]),
                          .DPRA0(fifo40_rd_addr_r[0]), .DPRA1(fifo40_rd_addr_r[1]), 
                          .DPRA2(fifo40_rd_addr_r[2]), .DPRA3(fifo40_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col1_n), .WE(fifo_41_wr_en_1));

   RAM16X1D fifo0_bit33  (.DPO (fifo_40_data_out[1]), .A0(fifo_40_wr_addr_1[0]), .A1(fifo_40_wr_addr_1[1]),
                          .A2(fifo_40_wr_addr_1[2]), .A3(fifo_40_wr_addr_1[3]), .D(ddr_dq_in[33]),
                          .DPRA0(fifo41_rd_addr_r[0]), .DPRA1(fifo41_rd_addr_r[1]), 
                          .DPRA2(fifo41_rd_addr_r[2]), .DPRA3(fifo41_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col0), .WE(fifo_40_wr_en_1));

   RAM16X1D fifo1_bit33  (.DPO (fifo_41_data_out[1]), .A0(fifo_41_wr_addr[0]), .A1(fifo_41_wr_addr[1]),
                          .A2(fifo_41_wr_addr[2]), .A3(fifo_41_wr_addr[3]), .D(ddr_dq_in[33]),
                          .DPRA0(fifo41_rd_addr_r[0]), .DPRA1(fifo41_rd_addr_r[1]), 
                          .DPRA2(fifo41_rd_addr_r[2]), .DPRA3(fifo41_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col0_n), .WE(fifo_41_wr_en));

  RAM16X1D fifo0_bit34  (.DPO (fifo_40_data_out[2]), .A0(fifo_40_wr_addr[0]), .A1(fifo_40_wr_addr[1]),
                          .A2(fifo_40_wr_addr[2]), .A3(fifo_40_wr_addr[3]), .D(ddr_dq_in[34]),
                          .DPRA0(fifo40_rd_addr_r[0]), .DPRA1(fifo40_rd_addr_r[1]), 
                          .DPRA2(fifo40_rd_addr_r[2]), .DPRA3(fifo40_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col1), .WE(fifo_40_wr_en));

   RAM16X1D fifo1_bit34  (.DPO (fifo_41_data_out[2]), .A0(fifo_41_wr_addr_1[0]), .A1(fifo_41_wr_addr_1[1]),
                          .A2(fifo_41_wr_addr_1[2]), .A3(fifo_41_wr_addr_1[3]), .D(ddr_dq_in[34]),
                          .DPRA0(fifo40_rd_addr_r[0]), .DPRA1(fifo40_rd_addr_r[1]), 
                          .DPRA2(fifo40_rd_addr_r[2]), .DPRA3(fifo40_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col1_n), .WE(fifo_41_wr_en_1));

   RAM16X1D fifo0_bit35  (.DPO (fifo_40_data_out[3]), .A0(fifo_40_wr_addr_1[0]), .A1(fifo_40_wr_addr_1[1]),
                          .A2(fifo_40_wr_addr_1[2]), .A3(fifo_40_wr_addr_1[3]), .D(ddr_dq_in[35]),
                          .DPRA0(fifo41_rd_addr_r[0]), .DPRA1(fifo41_rd_addr_r[1]), 
                          .DPRA2(fifo41_rd_addr_r[2]), .DPRA3(fifo41_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col0), .WE(fifo_40_wr_en_1)); 

   RAM16X1D fifo1_bit35  (.DPO (fifo_41_data_out[3]), .A0(fifo_41_wr_addr[0]), .A1(fifo_41_wr_addr[1]),
                          .A2(fifo_41_wr_addr[2]), .A3(fifo_41_wr_addr[3]), .D(ddr_dq_in[35]),
                          .DPRA0(fifo41_rd_addr_r[0]), .DPRA1(fifo41_rd_addr_r[1]), 
                          .DPRA2(fifo41_rd_addr_r[2]), .DPRA3(fifo41_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col0_n), .WE(fifo_41_wr_en));


   RAM16X1D fifo0_bit36  (.DPO (fifo_40_data_out[4]), .A0(fifo_40_wr_addr[0]), .A1(fifo_40_wr_addr[1]),
                          .A2(fifo_40_wr_addr[2]), .A3(fifo_40_wr_addr[3]), .D(ddr_dq_in[36]),
                          .DPRA0(fifo40_rd_addr_r[0]), .DPRA1(fifo40_rd_addr_r[1]), 
                          .DPRA2(fifo40_rd_addr_r[2]), .DPRA3(fifo40_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col1), .WE(fifo_40_wr_en));

   RAM16X1D fifo1_bit36  (.DPO (fifo_41_data_out[4]), .A0(fifo_41_wr_addr_1[0]), .A1(fifo_41_wr_addr_1[1]),
                          .A2(fifo_41_wr_addr_1[2]), .A3(fifo_41_wr_addr_1[3]), .D(ddr_dq_in[36]),
                          .DPRA0(fifo40_rd_addr_r[0]), .DPRA1(fifo40_rd_addr_r[1]), 
                          .DPRA2(fifo40_rd_addr_r[2]), .DPRA3(fifo40_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col1_n), .WE(fifo_41_wr_en_1));

   RAM16X1D fifo0_bit37  (.DPO (fifo_40_data_out[5]), .A0(fifo_40_wr_addr_1[0]), .A1(fifo_40_wr_addr_1[1]),
                          .A2(fifo_40_wr_addr_1[2]), .A3(fifo_40_wr_addr_1[3]), .D(ddr_dq_in[37]),
                          .DPRA0(fifo41_rd_addr_r[0]), .DPRA1(fifo41_rd_addr_r[1]), 
                          .DPRA2(fifo41_rd_addr_r[2]), .DPRA3(fifo41_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col0), .WE(fifo_40_wr_en_1));

   RAM16X1D fifo1_bit37  (.DPO (fifo_41_data_out[5]), .A0(fifo_41_wr_addr[0]), .A1(fifo_41_wr_addr[1]),
                          .A2(fifo_41_wr_addr[2]), .A3(fifo_41_wr_addr[3]), .D(ddr_dq_in[37]),
                          .DPRA0(fifo41_rd_addr_r[0]), .DPRA1(fifo41_rd_addr_r[1]), 
                          .DPRA2(fifo41_rd_addr_r[2]), .DPRA3(fifo41_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col0_n), .WE(fifo_41_wr_en));

   RAM16X1D fifo0_bit38  (.DPO (fifo_40_data_out[6]), .A0(fifo_40_wr_addr[0]), .A1(fifo_40_wr_addr[1]),
                          .A2(fifo_40_wr_addr[2]), .A3(fifo_40_wr_addr[3]), .D(ddr_dq_in[38]),
                          .DPRA0(fifo40_rd_addr_r[0]), .DPRA1(fifo40_rd_addr_r[1]), 
                          .DPRA2(fifo40_rd_addr_r[2]), .DPRA3(fifo40_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col1), .WE(fifo_40_wr_en));

   RAM16X1D fifo1_bit38  (.DPO (fifo_41_data_out[6]), .A0(fifo_41_wr_addr_1[0]), .A1(fifo_41_wr_addr_1[1]),
                          .A2(fifo_41_wr_addr_1[2]), .A3(fifo_41_wr_addr_1[3]), .D(ddr_dq_in[38]),
                          .DPRA0(fifo40_rd_addr_r[0]), .DPRA1(fifo40_rd_addr_r[1]), 
                          .DPRA2(fifo40_rd_addr_r[2]), .DPRA3(fifo40_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col1_n), .WE(fifo_41_wr_en_1));

   RAM16X1D fifo0_bit39  (.DPO (fifo_40_data_out[7]), .A0(fifo_40_wr_addr_1[0]), .A1(fifo_40_wr_addr_1[1]),
                          .A2(fifo_40_wr_addr_1[2]), .A3(fifo_40_wr_addr_1[3]), .D(ddr_dq_in[39]),
                          .DPRA0(fifo41_rd_addr_r[0]), .DPRA1(fifo41_rd_addr_r[1]), 
                          .DPRA2(fifo41_rd_addr_r[2]), .DPRA3(fifo41_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col0), .WE(fifo_40_wr_en_1));

   RAM16X1D fifo1_bit39  (.DPO (fifo_41_data_out[7]), .A0(fifo_41_wr_addr[0]), .A1(fifo_41_wr_addr[1]),
                          .A2(fifo_41_wr_addr[2]), .A3(fifo_41_wr_addr[3]), .D(ddr_dq_in[39]),
                          .DPRA0(fifo41_rd_addr_r[0]), .DPRA1(fifo41_rd_addr_r[1]), 
                          .DPRA2(fifo41_rd_addr_r[2]), .DPRA3(fifo41_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs4_delayed_col0_n), .WE(fifo_41_wr_en));



   RAM16X1D fifo0_bit40  (.DPO (fifo_50_data_out[0]), .A0(fifo_50_wr_addr[0]), .A1(fifo_50_wr_addr[1]),
                          .A2(fifo_50_wr_addr[2]), .A3(fifo_50_wr_addr[3]), .D(ddr_dq_in[40]),
                          .DPRA0(fifo50_rd_addr_r[0]), .DPRA1(fifo50_rd_addr_r[1]), 
                          .DPRA2(fifo50_rd_addr_r[2]), .DPRA3(fifo50_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col1), .WE(fifo_50_wr_en));

   RAM16X1D fifo1_bit40  (.DPO (fifo_51_data_out[0]), .A0(fifo_51_wr_addr_1[0]), .A1(fifo_51_wr_addr_1[1]),
                          .A2(fifo_51_wr_addr_1[2]), .A3(fifo_51_wr_addr_1[3]), .D(ddr_dq_in[40]),
                          .DPRA0(fifo50_rd_addr_r[0]), .DPRA1(fifo50_rd_addr_r[1]), 
                          .DPRA2(fifo50_rd_addr_r[2]), .DPRA3(fifo50_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col1_n), .WE(fifo_51_wr_en_1));

   RAM16X1D fifo0_bit41  (.DPO (fifo_50_data_out[1]), .A0(fifo_50_wr_addr_1[0]), .A1(fifo_50_wr_addr_1[1]),
                          .A2(fifo_50_wr_addr_1[2]), .A3(fifo_50_wr_addr_1[3]), .D(ddr_dq_in[41]),
                          .DPRA0(fifo51_rd_addr_r[0]), .DPRA1(fifo51_rd_addr_r[1]), 
                          .DPRA2(fifo51_rd_addr_r[2]), .DPRA3(fifo51_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col0), .WE(fifo_50_wr_en_1));

   RAM16X1D fifo1_bit41  (.DPO (fifo_51_data_out[1]), .A0(fifo_51_wr_addr[0]), .A1(fifo_51_wr_addr[1]),
                          .A2(fifo_51_wr_addr[2]), .A3(fifo_51_wr_addr[3]), .D(ddr_dq_in[41]),
                          .DPRA0(fifo51_rd_addr_r[0]), .DPRA1(fifo51_rd_addr_r[1]), 
                          .DPRA2(fifo51_rd_addr_r[2]), .DPRA3(fifo51_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col0_n), .WE(fifo_51_wr_en));

  RAM16X1D fifo0_bit42  (.DPO (fifo_50_data_out[2]), .A0(fifo_50_wr_addr[0]), .A1(fifo_50_wr_addr[1]),
                          .A2(fifo_50_wr_addr[2]), .A3(fifo_50_wr_addr[3]), .D(ddr_dq_in[42]),
                          .DPRA0(fifo50_rd_addr_r[0]), .DPRA1(fifo50_rd_addr_r[1]), 
                          .DPRA2(fifo50_rd_addr_r[2]), .DPRA3(fifo50_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col1), .WE(fifo_50_wr_en));

   RAM16X1D fifo1_bit42  (.DPO (fifo_51_data_out[2]), .A0(fifo_51_wr_addr_1[0]), .A1(fifo_51_wr_addr_1[1]),
                          .A2(fifo_51_wr_addr_1[2]), .A3(fifo_51_wr_addr_1[3]), .D(ddr_dq_in[42]),
                          .DPRA0(fifo50_rd_addr_r[0]), .DPRA1(fifo50_rd_addr_r[1]), 
                          .DPRA2(fifo50_rd_addr_r[2]), .DPRA3(fifo50_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col1_n), .WE(fifo_51_wr_en_1));

   RAM16X1D fifo0_bit43  (.DPO (fifo_50_data_out[3]), .A0(fifo_50_wr_addr_1[0]), .A1(fifo_50_wr_addr_1[1]),
                          .A2(fifo_50_wr_addr_1[2]), .A3(fifo_50_wr_addr_1[3]), .D(ddr_dq_in[43]),
                          .DPRA0(fifo51_rd_addr_r[0]), .DPRA1(fifo51_rd_addr_r[1]), 
                          .DPRA2(fifo51_rd_addr_r[2]), .DPRA3(fifo51_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col0), .WE(fifo_50_wr_en_1));

   RAM16X1D fifo1_bit43  (.DPO (fifo_51_data_out[3]), .A0(fifo_51_wr_addr[0]), .A1(fifo_51_wr_addr[1]),
                          .A2(fifo_51_wr_addr[2]), .A3(fifo_51_wr_addr[3]), .D(ddr_dq_in[43]),
                          .DPRA0(fifo51_rd_addr_r[0]), .DPRA1(fifo51_rd_addr_r[1]), 
                          .DPRA2(fifo51_rd_addr_r[2]), .DPRA3(fifo51_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col0_n), .WE(fifo_51_wr_en));


   RAM16X1D fifo0_bit44  (.DPO (fifo_50_data_out[4]), .A0(fifo_50_wr_addr[0]), .A1(fifo_50_wr_addr[1]),
                          .A2(fifo_50_wr_addr[2]), .A3(fifo_50_wr_addr[3]), .D(ddr_dq_in[44]),
                          .DPRA0(fifo50_rd_addr_r[0]), .DPRA1(fifo50_rd_addr_r[1]), 
                          .DPRA2(fifo50_rd_addr_r[2]), .DPRA3(fifo50_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col1), .WE(fifo_50_wr_en));

   RAM16X1D fifo1_bit44  (.DPO (fifo_51_data_out[4]), .A0(fifo_51_wr_addr_1[0]), .A1(fifo_51_wr_addr_1[1]),
                          .A2(fifo_51_wr_addr_1[2]), .A3(fifo_51_wr_addr_1[3]), .D(ddr_dq_in[44]),
                          .DPRA0(fifo50_rd_addr_r[0]), .DPRA1(fifo50_rd_addr_r[1]), 
                          .DPRA2(fifo50_rd_addr_r[2]), .DPRA3(fifo50_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col1_n), .WE(fifo_51_wr_en_1));

   RAM16X1D fifo0_bit45  (.DPO (fifo_50_data_out[5]), .A0(fifo_50_wr_addr_1[0]), .A1(fifo_50_wr_addr_1[1]),
                          .A2(fifo_50_wr_addr_1[2]), .A3(fifo_50_wr_addr_1[3]), .D(ddr_dq_in[45]),
                          .DPRA0(fifo51_rd_addr_r[0]), .DPRA1(fifo51_rd_addr_r[1]), 
                          .DPRA2(fifo51_rd_addr_r[2]), .DPRA3(fifo51_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col0), .WE(fifo_50_wr_en_1));

   RAM16X1D fifo1_bit45  (.DPO (fifo_51_data_out[5]), .A0(fifo_51_wr_addr[0]), .A1(fifo_51_wr_addr[1]),
                          .A2(fifo_51_wr_addr[2]), .A3(fifo_51_wr_addr[3]), .D(ddr_dq_in[45]),
                          .DPRA0(fifo51_rd_addr_r[0]), .DPRA1(fifo51_rd_addr_r[1]), 
                          .DPRA2(fifo51_rd_addr_r[2]), .DPRA3(fifo51_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col0_n), .WE(fifo_51_wr_en));

   RAM16X1D fifo0_bit46  (.DPO (fifo_50_data_out[6]), .A0(fifo_50_wr_addr[0]), .A1(fifo_50_wr_addr[1]),
                          .A2(fifo_50_wr_addr[2]), .A3(fifo_50_wr_addr[3]), .D(ddr_dq_in[46]),
                          .DPRA0(fifo50_rd_addr_r[0]), .DPRA1(fifo50_rd_addr_r[1]), 
                          .DPRA2(fifo50_rd_addr_r[2]), .DPRA3(fifo50_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col1), .WE(fifo_50_wr_en));

   RAM16X1D fifo1_bit46  (.DPO (fifo_51_data_out[6]), .A0(fifo_51_wr_addr_1[0]), .A1(fifo_51_wr_addr_1[1]),
                          .A2(fifo_51_wr_addr_1[2]), .A3(fifo_51_wr_addr_1[3]), .D(ddr_dq_in[46]),
                          .DPRA0(fifo50_rd_addr_r[0]), .DPRA1(fifo50_rd_addr_r[1]), 
                          .DPRA2(fifo50_rd_addr_r[2]), .DPRA3(fifo50_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col1_n), .WE(fifo_51_wr_en_1));

   RAM16X1D fifo0_bit47  (.DPO (fifo_50_data_out[7]), .A0(fifo_50_wr_addr_1[0]), .A1(fifo_50_wr_addr_1[1]),
                          .A2(fifo_50_wr_addr_1[2]), .A3(fifo_50_wr_addr_1[3]), .D(ddr_dq_in[47]),
                          .DPRA0(fifo51_rd_addr_r[0]), .DPRA1(fifo51_rd_addr_r[1]), 
                          .DPRA2(fifo51_rd_addr_r[2]), .DPRA3(fifo51_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col0), .WE(fifo_50_wr_en_1));

   RAM16X1D fifo1_bit47  (.DPO (fifo_51_data_out[7]), .A0(fifo_51_wr_addr[0]), .A1(fifo_51_wr_addr[1]),
                          .A2(fifo_51_wr_addr[2]), .A3(fifo_51_wr_addr[3]), .D(ddr_dq_in[47]),
                          .DPRA0(fifo51_rd_addr_r[0]), .DPRA1(fifo51_rd_addr_r[1]), 
                          .DPRA2(fifo51_rd_addr_r[2]), .DPRA3(fifo51_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs5_delayed_col0_n), .WE(fifo_51_wr_en));

   RAM16X1D fifo0_bit48  (.DPO (fifo_60_data_out[0]), .A0(fifo_60_wr_addr[0]), .A1(fifo_60_wr_addr[1]),
                          .A2(fifo_60_wr_addr[2]), .A3(fifo_60_wr_addr[3]), .D(ddr_dq_in[48]),
                          .DPRA0(fifo60_rd_addr_r[0]), .DPRA1(fifo60_rd_addr_r[1]), 
                          .DPRA2(fifo60_rd_addr_r[2]), .DPRA3(fifo60_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col1), .WE(fifo_60_wr_en));

   RAM16X1D fifo1_bit48  (.DPO (fifo_61_data_out[0]), .A0(fifo_61_wr_addr_1[0]), .A1(fifo_61_wr_addr_1[1]),
                          .A2(fifo_61_wr_addr_1[2]), .A3(fifo_61_wr_addr_1[3]), .D(ddr_dq_in[48]),
                          .DPRA0(fifo60_rd_addr_r[0]), .DPRA1(fifo60_rd_addr_r[1]), 
                          .DPRA2(fifo60_rd_addr_r[2]), .DPRA3(fifo60_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col1_n), .WE(fifo_61_wr_en_1));

   RAM16X1D fifo0_bit49  (.DPO (fifo_60_data_out[1]), .A0(fifo_60_wr_addr_1[0]), .A1(fifo_60_wr_addr_1[1]),
                          .A2(fifo_60_wr_addr_1[2]), .A3(fifo_60_wr_addr_1[3]), .D(ddr_dq_in[49]),
                          .DPRA0(fifo61_rd_addr_r[0]), .DPRA1(fifo61_rd_addr_r[1]), 
                          .DPRA2(fifo61_rd_addr_r[2]), .DPRA3(fifo61_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col0), .WE(fifo_60_wr_en_1));

   RAM16X1D fifo1_bit49  (.DPO (fifo_61_data_out[1]), .A0(fifo_61_wr_addr[0]), .A1(fifo_61_wr_addr[1]),
                          .A2(fifo_61_wr_addr[2]), .A3(fifo_61_wr_addr[3]), .D(ddr_dq_in[49]),
                          .DPRA0(fifo61_rd_addr_r[0]), .DPRA1(fifo61_rd_addr_r[1]), 
                          .DPRA2(fifo61_rd_addr_r[2]), .DPRA3(fifo61_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col0_n), .WE(fifo_61_wr_en));

  RAM16X1D fifo0_bit50  (.DPO (fifo_60_data_out[2]), .A0(fifo_60_wr_addr[0]), .A1(fifo_60_wr_addr[1]),
                          .A2(fifo_60_wr_addr[2]), .A3(fifo_60_wr_addr[3]), .D(ddr_dq_in[50]),
                          .DPRA0(fifo60_rd_addr_r[0]), .DPRA1(fifo60_rd_addr_r[1]), 
                          .DPRA2(fifo60_rd_addr_r[2]), .DPRA3(fifo60_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col1), .WE(fifo_60_wr_en));

   RAM16X1D fifo1_bit50  (.DPO (fifo_61_data_out[2]), .A0(fifo_61_wr_addr_1[0]), .A1(fifo_61_wr_addr_1[1]),
                          .A2(fifo_61_wr_addr_1[2]), .A3(fifo_61_wr_addr_1[3]), .D(ddr_dq_in[50]),
                          .DPRA0(fifo60_rd_addr_r[0]), .DPRA1(fifo60_rd_addr_r[1]), 
                          .DPRA2(fifo60_rd_addr_r[2]), .DPRA3(fifo60_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col1_n), .WE(fifo_61_wr_en_1));

   RAM16X1D fifo0_bit51  (.DPO (fifo_60_data_out[3]), .A0(fifo_60_wr_addr_1[0]), .A1(fifo_60_wr_addr_1[1]),
                          .A2(fifo_60_wr_addr_1[2]), .A3(fifo_60_wr_addr_1[3]), .D(ddr_dq_in[51]),
                          .DPRA0(fifo61_rd_addr_r[0]), .DPRA1(fifo61_rd_addr_r[1]), 
                          .DPRA2(fifo61_rd_addr_r[2]), .DPRA3(fifo61_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col0), .WE(fifo_60_wr_en_1));

   RAM16X1D fifo1_bit51  (.DPO (fifo_61_data_out[3]), .A0(fifo_61_wr_addr[0]), .A1(fifo_61_wr_addr[1]),
                          .A2(fifo_61_wr_addr[2]), .A3(fifo_61_wr_addr[3]), .D(ddr_dq_in[51]),
                          .DPRA0(fifo61_rd_addr_r[0]), .DPRA1(fifo61_rd_addr_r[1]), 
                          .DPRA2(fifo61_rd_addr_r[2]), .DPRA3(fifo61_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col0_n), .WE(fifo_61_wr_en));


   RAM16X1D fifo0_bit52  (.DPO (fifo_60_data_out[4]), .A0(fifo_60_wr_addr[0]), .A1(fifo_60_wr_addr[1]),
                          .A2(fifo_60_wr_addr[2]), .A3(fifo_60_wr_addr[3]), .D(ddr_dq_in[52]),
                          .DPRA0(fifo60_rd_addr_r[0]), .DPRA1(fifo60_rd_addr_r[1]), 
                          .DPRA2(fifo60_rd_addr_r[2]), .DPRA3(fifo60_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col1), .WE(fifo_60_wr_en));

   RAM16X1D fifo1_bit52  (.DPO (fifo_61_data_out[4]), .A0(fifo_61_wr_addr_1[0]), .A1(fifo_61_wr_addr_1[1]),
                          .A2(fifo_61_wr_addr_1[2]), .A3(fifo_61_wr_addr_1[3]), .D(ddr_dq_in[52]),
                          .DPRA0(fifo60_rd_addr_r[0]), .DPRA1(fifo60_rd_addr_r[1]), 
                          .DPRA2(fifo60_rd_addr_r[2]), .DPRA3(fifo60_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col1_n), .WE(fifo_61_wr_en_1));

   RAM16X1D fifo0_bit53  (.DPO (fifo_60_data_out[5]), .A0(fifo_60_wr_addr_1[0]), .A1(fifo_60_wr_addr_1[1]),
                          .A2(fifo_60_wr_addr_1[2]), .A3(fifo_60_wr_addr_1[3]), .D(ddr_dq_in[53]),
                          .DPRA0(fifo61_rd_addr_r[0]), .DPRA1(fifo61_rd_addr_r[1]), 
                          .DPRA2(fifo61_rd_addr_r[2]), .DPRA3(fifo61_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col0), .WE(fifo_60_wr_en_1));

   RAM16X1D fifo1_bit53  (.DPO (fifo_61_data_out[5]), .A0(fifo_61_wr_addr[0]), .A1(fifo_61_wr_addr[1]),
                          .A2(fifo_61_wr_addr[2]), .A3(fifo_61_wr_addr[3]), .D(ddr_dq_in[53]),
                          .DPRA0(fifo61_rd_addr_r[0]), .DPRA1(fifo61_rd_addr_r[1]), 
                          .DPRA2(fifo61_rd_addr_r[2]), .DPRA3(fifo61_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col0_n), .WE(fifo_61_wr_en));

   RAM16X1D fifo0_bit54  (.DPO (fifo_60_data_out[6]), .A0(fifo_60_wr_addr[0]), .A1(fifo_60_wr_addr[1]),
                          .A2(fifo_60_wr_addr[2]), .A3(fifo_60_wr_addr[3]), .D(ddr_dq_in[54]),
                          .DPRA0(fifo60_rd_addr_r[0]), .DPRA1(fifo60_rd_addr_r[1]), 
                          .DPRA2(fifo60_rd_addr_r[2]), .DPRA3(fifo60_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col1), .WE(fifo_60_wr_en));

   RAM16X1D fifo1_bit54  (.DPO (fifo_61_data_out[6]), .A0(fifo_61_wr_addr_1[0]), .A1(fifo_61_wr_addr_1[1]),
                          .A2(fifo_61_wr_addr_1[2]), .A3(fifo_61_wr_addr_1[3]), .D(ddr_dq_in[54]),
                          .DPRA0(fifo60_rd_addr_r[0]), .DPRA1(fifo60_rd_addr_r[1]), 
                          .DPRA2(fifo60_rd_addr_r[2]), .DPRA3(fifo60_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col1_n), .WE(fifo_61_wr_en_1));

   RAM16X1D fifo0_bit55  (.DPO (fifo_60_data_out[7]), .A0(fifo_60_wr_addr_1[0]), .A1(fifo_60_wr_addr_1[1]),
                          .A2(fifo_60_wr_addr_1[2]), .A3(fifo_60_wr_addr_1[3]), .D(ddr_dq_in[55]),
                          .DPRA0(fifo61_rd_addr_r[0]), .DPRA1(fifo61_rd_addr_r[1]), 
                          .DPRA2(fifo61_rd_addr_r[2]), .DPRA3(fifo61_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col0), .WE(fifo_60_wr_en_1));

   RAM16X1D fifo1_bit55  (.DPO (fifo_61_data_out[7]), .A0(fifo_61_wr_addr[0]), .A1(fifo_61_wr_addr[1]),
                          .A2(fifo_61_wr_addr[2]), .A3(fifo_61_wr_addr[3]), .D(ddr_dq_in[55]),
                          .DPRA0(fifo61_rd_addr_r[0]), .DPRA1(fifo61_rd_addr_r[1]), 
                          .DPRA2(fifo61_rd_addr_r[2]), .DPRA3(fifo61_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs6_delayed_col0_n), .WE(fifo_61_wr_en));


   RAM16X1D fifo0_bit56  (.DPO (fifo_70_data_out[0]), .A0(fifo_70_wr_addr[0]), .A1(fifo_70_wr_addr[1]),
                          .A2(fifo_70_wr_addr[2]), .A3(fifo_70_wr_addr[3]), .D(ddr_dq_in[56]),
                          .DPRA0(fifo70_rd_addr_r[0]), .DPRA1(fifo70_rd_addr_r[1]), 
                          .DPRA2(fifo70_rd_addr_r[2]), .DPRA3(fifo70_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col1), .WE(fifo_70_wr_en));

   RAM16X1D fifo1_bit56  (.DPO (fifo_71_data_out[0]), .A0(fifo_71_wr_addr_1[0]), .A1(fifo_71_wr_addr_1[1]),
                          .A2(fifo_71_wr_addr_1[2]), .A3(fifo_71_wr_addr_1[3]), .D(ddr_dq_in[56]),
                          .DPRA0(fifo70_rd_addr_r[0]), .DPRA1(fifo70_rd_addr_r[1]), 
                          .DPRA2(fifo70_rd_addr_r[2]), .DPRA3(fifo70_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col1_n), .WE(fifo_71_wr_en_1));

   RAM16X1D fifo0_bit57  (.DPO (fifo_70_data_out[1]), .A0(fifo_70_wr_addr_1[0]), .A1(fifo_70_wr_addr_1[1]),
                          .A2(fifo_70_wr_addr_1[2]), .A3(fifo_70_wr_addr_1[3]), .D(ddr_dq_in[57]),
                          .DPRA0(fifo71_rd_addr_r[0]), .DPRA1(fifo71_rd_addr_r[1]), 
                          .DPRA2(fifo71_rd_addr_r[2]), .DPRA3(fifo71_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col0), .WE(fifo_70_wr_en_1));

   RAM16X1D fifo1_bit57  (.DPO (fifo_71_data_out[1]), .A0(fifo_71_wr_addr[0]), .A1(fifo_71_wr_addr[1]),
                          .A2(fifo_71_wr_addr[2]), .A3(fifo_71_wr_addr[3]), .D(ddr_dq_in[57]),
                          .DPRA0(fifo71_rd_addr_r[0]), .DPRA1(fifo71_rd_addr_r[1]), 
                          .DPRA2(fifo71_rd_addr_r[2]), .DPRA3(fifo71_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col0_n), .WE(fifo_71_wr_en));

  RAM16X1D fifo0_bit58  (.DPO (fifo_70_data_out[2]), .A0(fifo_70_wr_addr[0]), .A1(fifo_70_wr_addr[1]),
                          .A2(fifo_70_wr_addr[2]), .A3(fifo_70_wr_addr[3]), .D(ddr_dq_in[58]),
                          .DPRA0(fifo70_rd_addr_r[0]), .DPRA1(fifo70_rd_addr_r[1]), 
                          .DPRA2(fifo70_rd_addr_r[2]), .DPRA3(fifo70_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col1), .WE(fifo_70_wr_en));

   RAM16X1D fifo1_bit58  (.DPO (fifo_71_data_out[2]), .A0(fifo_71_wr_addr_1[0]), .A1(fifo_71_wr_addr_1[1]),
                          .A2(fifo_71_wr_addr_1[2]), .A3(fifo_71_wr_addr_1[3]), .D(ddr_dq_in[58]),
                          .DPRA0(fifo70_rd_addr_r[0]), .DPRA1(fifo70_rd_addr_r[1]), 
                          .DPRA2(fifo70_rd_addr_r[2]), .DPRA3(fifo70_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col1_n), .WE(fifo_71_wr_en_1));

   RAM16X1D fifo0_bit59  (.DPO (fifo_70_data_out[3]), .A0(fifo_70_wr_addr_1[0]), .A1(fifo_70_wr_addr_1[1]),
                          .A2(fifo_70_wr_addr_1[2]), .A3(fifo_70_wr_addr_1[3]), .D(ddr_dq_in[59]),
                          .DPRA0(fifo71_rd_addr_r[0]), .DPRA1(fifo71_rd_addr_r[1]), 
                          .DPRA2(fifo71_rd_addr_r[2]), .DPRA3(fifo71_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col0), .WE(fifo_70_wr_en_1));

   RAM16X1D fifo1_bit59  (.DPO (fifo_71_data_out[3]), .A0(fifo_71_wr_addr[0]), .A1(fifo_71_wr_addr[1]),
                          .A2(fifo_71_wr_addr[2]), .A3(fifo_71_wr_addr[3]), .D(ddr_dq_in[59]),
                          .DPRA0(fifo71_rd_addr_r[0]), .DPRA1(fifo71_rd_addr_r[1]), 
                          .DPRA2(fifo71_rd_addr_r[2]), .DPRA3(fifo71_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col0_n), .WE(fifo_71_wr_en));


   RAM16X1D fifo0_bit60  (.DPO (fifo_70_data_out[4]), .A0(fifo_70_wr_addr[0]), .A1(fifo_70_wr_addr[1]),
                          .A2(fifo_70_wr_addr[2]), .A3(fifo_70_wr_addr[3]), .D(ddr_dq_in[60]),
                          .DPRA0(fifo70_rd_addr_r[0]), .DPRA1(fifo70_rd_addr_r[1]), 
                          .DPRA2(fifo70_rd_addr_r[2]), .DPRA3(fifo70_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col1), .WE(fifo_70_wr_en));

   RAM16X1D fifo1_bit60  (.DPO (fifo_71_data_out[4]), .A0(fifo_71_wr_addr_1[0]), .A1(fifo_71_wr_addr_1[1]),
                          .A2(fifo_71_wr_addr_1[2]), .A3(fifo_71_wr_addr_1[3]), .D(ddr_dq_in[60]),
                          .DPRA0(fifo70_rd_addr_r[0]), .DPRA1(fifo70_rd_addr_r[1]), 
                          .DPRA2(fifo70_rd_addr_r[2]), .DPRA3(fifo70_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col1_n), .WE(fifo_71_wr_en_1));

   RAM16X1D fifo0_bit61  (.DPO (fifo_70_data_out[5]), .A0(fifo_70_wr_addr_1[0]), .A1(fifo_70_wr_addr_1[1]),
                          .A2(fifo_70_wr_addr_1[2]), .A3(fifo_70_wr_addr_1[3]), .D(ddr_dq_in[61]),
                          .DPRA0(fifo71_rd_addr_r[0]), .DPRA1(fifo71_rd_addr_r[1]), 
                          .DPRA2(fifo71_rd_addr_r[2]), .DPRA3(fifo71_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col0), .WE(fifo_70_wr_en_1));

   RAM16X1D fifo1_bit61  (.DPO (fifo_71_data_out[5]), .A0(fifo_71_wr_addr[0]), .A1(fifo_71_wr_addr[1]),
                          .A2(fifo_71_wr_addr[2]), .A3(fifo_71_wr_addr[3]), .D(ddr_dq_in[61]),
                          .DPRA0(fifo71_rd_addr_r[0]), .DPRA1(fifo71_rd_addr_r[1]), 
                          .DPRA2(fifo71_rd_addr_r[2]), .DPRA3(fifo71_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col0_n), .WE(fifo_71_wr_en));

   RAM16X1D fifo0_bit62  (.DPO (fifo_70_data_out[6]), .A0(fifo_70_wr_addr[0]), .A1(fifo_70_wr_addr[1]),
                          .A2(fifo_70_wr_addr[2]), .A3(fifo_70_wr_addr[3]), .D(ddr_dq_in[62]),
                          .DPRA0(fifo70_rd_addr_r[0]), .DPRA1(fifo70_rd_addr_r[1]), 
                          .DPRA2(fifo70_rd_addr_r[2]), .DPRA3(fifo70_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col1), .WE(fifo_70_wr_en));

   RAM16X1D fifo1_bit62  (.DPO (fifo_71_data_out[6]), .A0(fifo_71_wr_addr_1[0]), .A1(fifo_71_wr_addr_1[1]),
                          .A2(fifo_71_wr_addr_1[2]), .A3(fifo_71_wr_addr_1[3]), .D(ddr_dq_in[62]),
                          .DPRA0(fifo70_rd_addr_r[0]), .DPRA1(fifo70_rd_addr_r[1]), 
                          .DPRA2(fifo70_rd_addr_r[2]), .DPRA3(fifo70_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col1_n), .WE(fifo_71_wr_en_1));

   RAM16X1D fifo0_bit63  (.DPO (fifo_70_data_out[7]), .A0(fifo_70_wr_addr_1[0]), .A1(fifo_70_wr_addr_1[1]),
                          .A2(fifo_70_wr_addr_1[2]), .A3(fifo_70_wr_addr_1[3]), .D(ddr_dq_in[63]),
                          .DPRA0(fifo71_rd_addr_r[0]), .DPRA1(fifo71_rd_addr_r[1]), 
                          .DPRA2(fifo71_rd_addr_r[2]), .DPRA3(fifo71_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col0), .WE(fifo_70_wr_en_1));

   RAM16X1D fifo1_bit63  (.DPO (fifo_71_data_out[7]), .A0(fifo_71_wr_addr[0]), .A1(fifo_71_wr_addr[1]),
                          .A2(fifo_71_wr_addr[2]), .A3(fifo_71_wr_addr[3]), .D(ddr_dq_in[63]),
                          .DPRA0(fifo71_rd_addr_r[0]), .DPRA1(fifo71_rd_addr_r[1]), 
                          .DPRA2(fifo71_rd_addr_r[2]), .DPRA3(fifo71_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs7_delayed_col0_n), .WE(fifo_71_wr_en));


   RAM16X1D fifo0_bit64  (.DPO (fifo_80_data_out[0]), .A0(fifo_80_wr_addr[0]), .A1(fifo_80_wr_addr[1]),
                          .A2(fifo_80_wr_addr[2]), .A3(fifo_80_wr_addr[3]), .D(ddr_dq_in[64]),
                          .DPRA0(fifo80_rd_addr_r[0]), .DPRA1(fifo80_rd_addr_r[1]), 
                          .DPRA2(fifo80_rd_addr_r[2]), .DPRA3(fifo80_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col1), .WE(fifo_80_wr_en));

   RAM16X1D fifo1_bit64  (.DPO (fifo_81_data_out[0]), .A0(fifo_81_wr_addr_1[0]), .A1(fifo_81_wr_addr_1[1]),
                          .A2(fifo_81_wr_addr_1[2]), .A3(fifo_81_wr_addr_1[3]), .D(ddr_dq_in[64]),
                          .DPRA0(fifo80_rd_addr_r[0]), .DPRA1(fifo80_rd_addr_r[1]), 
                          .DPRA2(fifo80_rd_addr_r[2]), .DPRA3(fifo80_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col1_n), .WE(fifo_81_wr_en_1));

   RAM16X1D fifo0_bit65  (.DPO (fifo_80_data_out[1]), .A0(fifo_80_wr_addr_1[0]), .A1(fifo_80_wr_addr_1[1]),
                          .A2(fifo_80_wr_addr_1[2]), .A3(fifo_80_wr_addr_1[3]), .D(ddr_dq_in[65]),
                          .DPRA0(fifo81_rd_addr_r[0]), .DPRA1(fifo81_rd_addr_r[1]), 
                          .DPRA2(fifo81_rd_addr_r[2]), .DPRA3(fifo81_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col0), .WE(fifo_80_wr_en_1));

   RAM16X1D fifo1_bit65  (.DPO (fifo_81_data_out[1]), .A0(fifo_81_wr_addr[0]), .A1(fifo_81_wr_addr[1]),
                          .A2(fifo_81_wr_addr[2]), .A3(fifo_81_wr_addr[3]), .D(ddr_dq_in[65]),
                          .DPRA0(fifo81_rd_addr_r[0]), .DPRA1(fifo81_rd_addr_r[1]), 
                          .DPRA2(fifo81_rd_addr_r[2]), .DPRA3(fifo81_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col0_n), .WE(fifo_81_wr_en));

  RAM16X1D fifo0_bit66  (.DPO (fifo_80_data_out[2]), .A0(fifo_80_wr_addr[0]), .A1(fifo_80_wr_addr[1]),
                          .A2(fifo_80_wr_addr[2]), .A3(fifo_80_wr_addr[3]), .D(ddr_dq_in[66]),
                          .DPRA0(fifo80_rd_addr_r[0]), .DPRA1(fifo80_rd_addr_r[1]), 
                          .DPRA2(fifo80_rd_addr_r[2]), .DPRA3(fifo80_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col1), .WE(fifo_80_wr_en));

   RAM16X1D fifo1_bit66  (.DPO (fifo_81_data_out[2]), .A0(fifo_81_wr_addr_1[0]), .A1(fifo_81_wr_addr_1[1]),
                          .A2(fifo_81_wr_addr_1[2]), .A3(fifo_81_wr_addr_1[3]), .D(ddr_dq_in[66]),
                          .DPRA0(fifo80_rd_addr_r[0]), .DPRA1(fifo80_rd_addr_r[1]), 
                          .DPRA2(fifo80_rd_addr_r[2]), .DPRA3(fifo80_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col1_n), .WE(fifo_81_wr_en_1));

   RAM16X1D fifo0_bit67  (.DPO (fifo_80_data_out[3]), .A0(fifo_80_wr_addr_1[0]), .A1(fifo_80_wr_addr_1[1]),
                          .A2(fifo_80_wr_addr_1[2]), .A3(fifo_80_wr_addr_1[3]), .D(ddr_dq_in[67]),
                          .DPRA0(fifo81_rd_addr_r[0]), .DPRA1(fifo81_rd_addr_r[1]), 
                          .DPRA2(fifo81_rd_addr_r[2]), .DPRA3(fifo81_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col0), .WE(fifo_80_wr_en_1));

   RAM16X1D fifo1_bit67  (.DPO (fifo_81_data_out[3]), .A0(fifo_81_wr_addr[0]), .A1(fifo_81_wr_addr[1]),
                          .A2(fifo_81_wr_addr[2]), .A3(fifo_81_wr_addr[3]), .D(ddr_dq_in[67]),
                          .DPRA0(fifo81_rd_addr_r[0]), .DPRA1(fifo81_rd_addr_r[1]), 
                          .DPRA2(fifo81_rd_addr_r[2]), .DPRA3(fifo81_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col0_n), .WE(fifo_81_wr_en));


   RAM16X1D fifo0_bit68  (.DPO (fifo_80_data_out[4]), .A0(fifo_80_wr_addr[0]), .A1(fifo_80_wr_addr[1]),
                          .A2(fifo_80_wr_addr[2]), .A3(fifo_80_wr_addr[3]), .D(ddr_dq_in[68]),
                          .DPRA0(fifo80_rd_addr_r[0]), .DPRA1(fifo80_rd_addr_r[1]), 
                          .DPRA2(fifo80_rd_addr_r[2]), .DPRA3(fifo80_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col1), .WE(fifo_80_wr_en));

   RAM16X1D fifo1_bit68  (.DPO (fifo_81_data_out[4]), .A0(fifo_81_wr_addr_1[0]), .A1(fifo_81_wr_addr_1[1]),
                          .A2(fifo_81_wr_addr_1[2]), .A3(fifo_81_wr_addr_1[3]), .D(ddr_dq_in[68]),
                          .DPRA0(fifo80_rd_addr_r[0]), .DPRA1(fifo80_rd_addr_r[1]), 
                          .DPRA2(fifo80_rd_addr_r[2]), .DPRA3(fifo80_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col1_n), .WE(fifo_81_wr_en_1));

   RAM16X1D fifo0_bit69  (.DPO (fifo_80_data_out[5]), .A0(fifo_80_wr_addr_1[0]), .A1(fifo_80_wr_addr_1[1]),
                          .A2(fifo_80_wr_addr_1[2]), .A3(fifo_80_wr_addr_1[3]), .D(ddr_dq_in[69]),
                          .DPRA0(fifo81_rd_addr_r[0]), .DPRA1(fifo81_rd_addr_r[1]), 
                          .DPRA2(fifo81_rd_addr_r[2]), .DPRA3(fifo81_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col0), .WE(fifo_80_wr_en_1));

   RAM16X1D fifo1_bit69  (.DPO (fifo_81_data_out[5]), .A0(fifo_81_wr_addr[0]), .A1(fifo_81_wr_addr[1]),
                          .A2(fifo_81_wr_addr[2]), .A3(fifo_81_wr_addr[3]), .D(ddr_dq_in[69]),
                          .DPRA0(fifo81_rd_addr_r[0]), .DPRA1(fifo81_rd_addr_r[1]), 
                          .DPRA2(fifo81_rd_addr_r[2]), .DPRA3(fifo81_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col0_n), .WE(fifo_81_wr_en));

   RAM16X1D fifo0_bit70  (.DPO (fifo_80_data_out[6]), .A0(fifo_80_wr_addr[0]), .A1(fifo_80_wr_addr[1]),
                          .A2(fifo_80_wr_addr[2]), .A3(fifo_80_wr_addr[3]), .D(ddr_dq_in[70]),
                          .DPRA0(fifo80_rd_addr_r[0]), .DPRA1(fifo80_rd_addr_r[1]), 
                          .DPRA2(fifo80_rd_addr_r[2]), .DPRA3(fifo80_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col1), .WE(fifo_80_wr_en));

   RAM16X1D fifo1_bit70  (.DPO (fifo_81_data_out[6]), .A0(fifo_81_wr_addr_1[0]), .A1(fifo_81_wr_addr_1[1]),
                          .A2(fifo_81_wr_addr_1[2]), .A3(fifo_81_wr_addr_1[3]), .D(ddr_dq_in[70]),
                          .DPRA0(fifo80_rd_addr_r[0]), .DPRA1(fifo80_rd_addr_r[1]), 
                          .DPRA2(fifo80_rd_addr_r[2]), .DPRA3(fifo80_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col1_n), .WE(fifo_81_wr_en_1));

   RAM16X1D fifo0_bit71  (.DPO (fifo_80_data_out[7]), .A0(fifo_80_wr_addr_1[0]), .A1(fifo_80_wr_addr_1[1]),
                          .A2(fifo_80_wr_addr_1[2]), .A3(fifo_80_wr_addr_1[3]), .D(ddr_dq_in[71]),
                          .DPRA0(fifo81_rd_addr_r[0]), .DPRA1(fifo81_rd_addr_r[1]), 
                          .DPRA2(fifo81_rd_addr_r[2]), .DPRA3(fifo81_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col0), .WE(fifo_80_wr_en_1));

   RAM16X1D fifo1_bit71  (.DPO (fifo_81_data_out[7]), .A0(fifo_81_wr_addr[0]), .A1(fifo_81_wr_addr[1]),
                          .A2(fifo_81_wr_addr[2]), .A3(fifo_81_wr_addr[3]), .D(ddr_dq_in[71]),
                          .DPRA0(fifo81_rd_addr_r[0]), .DPRA1(fifo81_rd_addr_r[1]), 
                          .DPRA2(fifo81_rd_addr_r[2]), .DPRA3(fifo81_rd_addr_r[3]), .SPO(open),
                          .WCLK(dqs8_delayed_col0_n), .WE(fifo_81_wr_en));





endmodule 

