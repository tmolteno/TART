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
module data_path_iobs (

    clk,               
    clk90,             
    reset90_r,         
    dqs_reset,         
    dqs_enable,        
    ddr_dqs,           
    ddr_dq,            
    write_data_falling,
    write_data_rising, 
    write_en_val,
    write_en_val1,      
    data_mask_f,       
    data_mask_r,       
    dqs_int_delay_in0, 
    dqs_int_delay_in1, 
    dqs_int_delay_in2, 
    dqs_int_delay_in3,
    dqs_int_delay_in4, 
    dqs_int_delay_in5, 
    dqs_int_delay_in6, 
    dqs_int_delay_in7, 
    dqs_int_delay_in8, 
    ddr_dq_val,         
    ddr_dm           
);

input    clk;             
input	 clk90;             
input    reset90_r;       
input	 dqs_reset;         
input    dqs_enable;        
inout    [8:0]ddr_dqs;          
inout    [71:0]ddr_dq;          
input	 [71:0]write_data_falling;
input    [71:0]write_data_rising; 
input    write_en_val;  
input    write_en_val1;    
input    [8:0]data_mask_f;   
input    [8:0]data_mask_r;   
output    dqs_int_delay_in0; 
output    dqs_int_delay_in1; 
output    dqs_int_delay_in2; 
output    dqs_int_delay_in3; 
output    dqs_int_delay_in4; 
output    dqs_int_delay_in5; 
output    dqs_int_delay_in6; 
output    dqs_int_delay_in7; 
output    dqs_int_delay_in8; 
output    [71:0]ddr_dq_val;
output    [8:0]ddr_dm;

//wire clk270 /* synthesis syn_keep =1 */;   
//wire clk180 /* synthesis syn_keep =1 */; 

wire clk270;
wire clk180;   

wire [71:0]ddr_dq_in;




assign clk270  =  ~ clk90;
assign clk180  =  ~ clk;

assign ddr_dq_val = ddr_dq_in;

ddr1_dm ddr1_dm0   (
                             .ddr_dm       (ddr_dm),
                             .mask_falling (data_mask_f),
                             .mask_rising  (data_mask_r),
                             .clk90        (clk90),
                             .clk270       (clk270)
                            );



//***********************************************************************
//    Read Data Capture Module Instantiations
//***********************************************************************
// DQS IOB instantiations
//***********************************************************************

  s3_dqs_iob  s3_dqs_iob0 (
                              .clk            (clk),
                              .clk180         (clk180),
                              .ddr_dqs_reset  (dqs_reset),
                              .ddr_dqs_enable (dqs_enable),
                              .ddr_dqs        (ddr_dqs[0]),
                              .dqs            (dqs_int_delay_in0)
			    );

 s3_dqs_iob  s3_dqs_iob1 (
                              .clk            (clk),
                              .clk180         (clk180),
                              .ddr_dqs_reset  (dqs_reset),
                              .ddr_dqs_enable (dqs_enable),
                              .ddr_dqs        (ddr_dqs[1]),
                              .dqs            (dqs_int_delay_in1)
			    );
                             
 s3_dqs_iob  s3_dqs_iob2 (
                              .clk            (clk),
                              .clk180         (clk180),
                              .ddr_dqs_reset  (dqs_reset),
                              .ddr_dqs_enable (dqs_enable),
                              .ddr_dqs        (ddr_dqs[2]),
                              .dqs            (dqs_int_delay_in2)
			    );
                             
 s3_dqs_iob  s3_dqs_iob3 (
                              .clk            (clk),
                              .clk180         (clk180),
                              .ddr_dqs_reset  (dqs_reset),
                              .ddr_dqs_enable (dqs_enable),
                              .ddr_dqs        (ddr_dqs[3]),
                              .dqs            (dqs_int_delay_in3)
			    );
			    
 s3_dqs_iob  s3_dqs_iob4 (
                              .clk            (clk),
                              .clk180         (clk180),
                              .ddr_dqs_reset  (dqs_reset),
                              .ddr_dqs_enable (dqs_enable),
                              .ddr_dqs        (ddr_dqs[4]),
                              .dqs            (dqs_int_delay_in4)
			    );
			    
  s3_dqs_iob  s3_dqs_iob5 (
                              .clk            (clk),
                              .clk180         (clk180),
                              .ddr_dqs_reset  (dqs_reset),
                              .ddr_dqs_enable (dqs_enable),
                              .ddr_dqs        (ddr_dqs[5]),
                              .dqs            (dqs_int_delay_in5)
			    );
                            
 s3_dqs_iob  s3_dqs_iob6 (
                              .clk            (clk),
                              .clk180         (clk180),
                              .ddr_dqs_reset  (dqs_reset),
                              .ddr_dqs_enable (dqs_enable),
                              .ddr_dqs        (ddr_dqs[6]),
                              .dqs            (dqs_int_delay_in6)
			    );
                            
 s3_dqs_iob  s3_dqs_iob7 (
                              .clk            (clk),
                              .clk180         (clk180),
                              .ddr_dqs_reset  (dqs_reset),
                              .ddr_dqs_enable (dqs_enable),
                              .ddr_dqs        (ddr_dqs[7]),
                              .dqs            (dqs_int_delay_in7)
			    );
			    
 s3_dqs_iob  s3_dqs_iob8 (
                              .clk            (clk),
                              .clk180         (clk180),
                              .ddr_dqs_reset  (dqs_reset),
                              .ddr_dqs_enable (dqs_enable),
                              .ddr_dqs        (ddr_dqs[8]),
                              .dqs            (dqs_int_delay_in8)
			    );
	


//******************************************************************************************************************************
// DDR Data bit instantiations (72-bits)
//******************************************************************************************************************************            
 
 s3_ddr_iob  s3_ddr_iob0 
				(
      				.ddr_dq_inout       (ddr_dq[0]), 
      				.write_data_falling (write_data_falling[0]), 
      				.write_data_rising  (write_data_rising[0]),
      				.read_data_in       (ddr_dq_in[0]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob1 
				(
      				.ddr_dq_inout       (ddr_dq[1]), 
      				.write_data_falling (write_data_falling[1]), 
      				.write_data_rising  (write_data_rising[1]),
      				.read_data_in       (ddr_dq_in[1]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob2 
				(
      				.ddr_dq_inout       (ddr_dq[2]), 
      				.write_data_falling (write_data_falling[2]), 
      				.write_data_rising  (write_data_rising[2]),
      				.read_data_in       (ddr_dq_in[2]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob3 
				(
      				.ddr_dq_inout       (ddr_dq[3]), 
      				.write_data_falling (write_data_falling[3]), 
      				.write_data_rising  (write_data_rising[3]),
      				.read_data_in       (ddr_dq_in[3]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob4 
				(
      				.ddr_dq_inout       (ddr_dq[4]), 
      				.write_data_falling (write_data_falling[4]), 
      				.write_data_rising  (write_data_rising[4]),
      				.read_data_in       (ddr_dq_in[4]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
  
 s3_ddr_iob  s3_ddr_iob5 
				(
      				.ddr_dq_inout       (ddr_dq[5]), 
      				.write_data_falling (write_data_falling[5]), 
      				.write_data_rising  (write_data_rising[5]),
      				.read_data_in       (ddr_dq_in[5]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob6 
				(
      				.ddr_dq_inout       (ddr_dq[6]), 
      				.write_data_falling (write_data_falling[6]), 
      				.write_data_rising  (write_data_rising[6]),
      				.read_data_in       (ddr_dq_in[6]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
 s3_ddr_iob  s3_ddr_iob7 
				(
      				.ddr_dq_inout       (ddr_dq[7]), 
      				.write_data_falling (write_data_falling[7]), 
      				.write_data_rising  (write_data_rising[7]),
      				.read_data_in       (ddr_dq_in[7]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob8 
				(
      				.ddr_dq_inout       (ddr_dq[8]), 
      				.write_data_falling (write_data_falling[8]), 
      				.write_data_rising  (write_data_rising[8]),
      				.read_data_in       (ddr_dq_in[8]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob9 
				(
      				.ddr_dq_inout       (ddr_dq[9]), 
      				.write_data_falling (write_data_falling[9]), 
      				.write_data_rising  (write_data_rising[9]),
      				.read_data_in       (ddr_dq_in[9]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );

 s3_ddr_iob  s3_ddr_iob10 
				(
      				.ddr_dq_inout       (ddr_dq[10]), 
      				.write_data_falling (write_data_falling[10]), 
      				.write_data_rising  (write_data_rising[10]),
      				.read_data_in       (ddr_dq_in[10]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob11 
				(
      				.ddr_dq_inout       (ddr_dq[11]), 
      				.write_data_falling (write_data_falling[11]), 
      				.write_data_rising  (write_data_rising[11]),
      				.read_data_in       (ddr_dq_in[11]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob12 
				(
      				.ddr_dq_inout       (ddr_dq[12]), 
      				.write_data_falling (write_data_falling[12]), 
      				.write_data_rising  (write_data_rising[12]),
      				.read_data_in       (ddr_dq_in[12]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob13 
				(
      				.ddr_dq_inout       (ddr_dq[13]), 
      				.write_data_falling (write_data_falling[13]), 
      				.write_data_rising  (write_data_rising[13]),
      				.read_data_in       (ddr_dq_in[13]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob14 
				(
      				.ddr_dq_inout       (ddr_dq[14]), 
      				.write_data_falling (write_data_falling[14]), 
      				.write_data_rising  (write_data_rising[14]),
      				.read_data_in       (ddr_dq_in[14]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
  
 s3_ddr_iob  s3_ddr_iob15 
				(
      				.ddr_dq_inout       (ddr_dq[15]), 
      				.write_data_falling (write_data_falling[15]), 
      				.write_data_rising  (write_data_rising[15]),
      				.read_data_in       (ddr_dq_in[15]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob16 
				(
      				.ddr_dq_inout       (ddr_dq[16]), 
      				.write_data_falling (write_data_falling[16]), 
      				.write_data_rising  (write_data_rising[16]),
      				.read_data_in       (ddr_dq_in[16]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
 s3_ddr_iob  s3_ddr_iob17 
				(
      				.ddr_dq_inout       (ddr_dq[17]), 
      				.write_data_falling (write_data_falling[17]), 
      				.write_data_rising  (write_data_rising[17]),
      				.read_data_in       (ddr_dq_in[17]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob18 
				(
      				.ddr_dq_inout       (ddr_dq[18]), 
      				.write_data_falling (write_data_falling[18]), 
      				.write_data_rising  (write_data_rising[18]),
      				.read_data_in       (ddr_dq_in[18]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob19 
				(
      				.ddr_dq_inout       (ddr_dq[19]), 
      				.write_data_falling (write_data_falling[19]), 
      				.write_data_rising  (write_data_rising[19]),
      				.read_data_in       (ddr_dq_in[19]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
 s3_ddr_iob  s3_ddr_iob20 
				(
      				.ddr_dq_inout       (ddr_dq[20]), 
      				.write_data_falling (write_data_falling[20]), 
      				.write_data_rising  (write_data_rising[20]),
      				.read_data_in       (ddr_dq_in[20]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob21 
				(
      				.ddr_dq_inout       (ddr_dq[21]), 
      				.write_data_falling (write_data_falling[21]), 
      				.write_data_rising  (write_data_rising[21]),
      				.read_data_in       (ddr_dq_in[21]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob22 
				(
      				.ddr_dq_inout       (ddr_dq[22]), 
      				.write_data_falling (write_data_falling[22]), 
      				.write_data_rising  (write_data_rising[22]),
      				.read_data_in       (ddr_dq_in[22]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob23 
				(
      				.ddr_dq_inout       (ddr_dq[23]), 
      				.write_data_falling (write_data_falling[23]), 
      				.write_data_rising  (write_data_rising[23]),
      				.read_data_in       (ddr_dq_in[23]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob24 
				(
      				.ddr_dq_inout       (ddr_dq[24]), 
      				.write_data_falling (write_data_falling[24]), 
      				.write_data_rising  (write_data_rising[24]),
      				.read_data_in       (ddr_dq_in[24]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
  
 s3_ddr_iob  s3_ddr_iob25 
				(
      				.ddr_dq_inout       (ddr_dq[25]), 
      				.write_data_falling (write_data_falling[25]), 
      				.write_data_rising  (write_data_rising[25]),
      				.read_data_in       (ddr_dq_in[25]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob26 
				(
      				.ddr_dq_inout       (ddr_dq[26]), 
      				.write_data_falling (write_data_falling[26]), 
      				.write_data_rising  (write_data_rising[26]),
      				.read_data_in       (ddr_dq_in[26]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
 s3_ddr_iob  s3_ddr_iob27 
				(
      				.ddr_dq_inout       (ddr_dq[27]), 
      				.write_data_falling (write_data_falling[27]), 
      				.write_data_rising  (write_data_rising[27]),
      				.read_data_in       (ddr_dq_in[27]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob28 
				(
      				.ddr_dq_inout       (ddr_dq[28]), 
      				.write_data_falling (write_data_falling[28]), 
      				.write_data_rising  (write_data_rising[28]),
      				.read_data_in       (ddr_dq_in[28]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob29 
				(
      				.ddr_dq_inout       (ddr_dq[29]), 
      				.write_data_falling (write_data_falling[29]), 
      				.write_data_rising  (write_data_rising[29]),
      				.read_data_in       (ddr_dq_in[29]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );

 s3_ddr_iob  s3_ddr_iob30 
				(
      				.ddr_dq_inout       (ddr_dq[30]), 
      				.write_data_falling (write_data_falling[30]), 
      				.write_data_rising  (write_data_rising[30]),
      				.read_data_in       (ddr_dq_in[30]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob31 
				(
      				.ddr_dq_inout       (ddr_dq[31]), 
      				.write_data_falling (write_data_falling[31]), 
      				.write_data_rising  (write_data_rising[31]),
      				.read_data_in       (ddr_dq_in[31]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     

 s3_ddr_iob  s3_ddr_iob32 
				(
      				.ddr_dq_inout       (ddr_dq[32]), 
      				.write_data_falling (write_data_falling[32]), 
      				.write_data_rising  (write_data_rising[32]),
      				.read_data_in       (ddr_dq_in[32]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob33 
				(
      				.ddr_dq_inout       (ddr_dq[33]), 
      				.write_data_falling (write_data_falling[33]), 
      				.write_data_rising  (write_data_rising[33]),
      				.read_data_in       (ddr_dq_in[33]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob34 
				(
      				.ddr_dq_inout       (ddr_dq[34]), 
      				.write_data_falling (write_data_falling[34]), 
      				.write_data_rising  (write_data_rising[34]),
      				.read_data_in       (ddr_dq_in[34]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
  
                        		     
 s3_ddr_iob  s3_ddr_iob35 
				(
      				.ddr_dq_inout       (ddr_dq[35]), 
      				.write_data_falling (write_data_falling[35]), 
      				.write_data_rising  (write_data_rising[35]),
      				.read_data_in       (ddr_dq_in[35]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
 


 s3_ddr_iob  s3_ddr_iob36 
				(
      				.ddr_dq_inout       (ddr_dq[36]), 
      				.write_data_falling (write_data_falling[36]), 
      				.write_data_rising  (write_data_rising[36]),
      				.read_data_in       (ddr_dq_in[36]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
 s3_ddr_iob  s3_ddr_iob37 
				(
      				.ddr_dq_inout       (ddr_dq[37]), 
      				.write_data_falling (write_data_falling[37]), 
      				.write_data_rising  (write_data_rising[37]),
      				.read_data_in       (ddr_dq_in[37]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob38 
				(
      				.ddr_dq_inout       (ddr_dq[38]), 
      				.write_data_falling (write_data_falling[38]), 
      				.write_data_rising  (write_data_rising[38]),
      				.read_data_in       (ddr_dq_in[38]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob39 
				(
      				.ddr_dq_inout       (ddr_dq[39]), 
      				.write_data_falling (write_data_falling[39]), 
      				.write_data_rising  (write_data_rising[39]),
      				.read_data_in       (ddr_dq_in[39]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob40 
				(
      				.ddr_dq_inout       (ddr_dq[40]), 
      				.write_data_falling (write_data_falling[40]), 
      				.write_data_rising  (write_data_rising[40]),
      				.read_data_in       (ddr_dq_in[40]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob41 
				(
      				.ddr_dq_inout       (ddr_dq[41]), 
      				.write_data_falling (write_data_falling[41]), 
      				.write_data_rising  (write_data_rising[41]),
      				.read_data_in       (ddr_dq_in[41]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob42 
				(
      				.ddr_dq_inout       (ddr_dq[42]), 
      				.write_data_falling (write_data_falling[42]), 
      				.write_data_rising  (write_data_rising[42]),
      				.read_data_in       (ddr_dq_in[42]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob43 
				(
      				.ddr_dq_inout       (ddr_dq[43]), 
      				.write_data_falling (write_data_falling[43]), 
      				.write_data_rising  (write_data_rising[43]),
      				.read_data_in       (ddr_dq_in[43]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob44 
				(
      				.ddr_dq_inout       (ddr_dq[44]), 
      				.write_data_falling (write_data_falling[44]), 
      				.write_data_rising  (write_data_rising[44]),
      				.read_data_in       (ddr_dq_in[44]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
  
 s3_ddr_iob  s3_ddr_iob45 
				(
      				.ddr_dq_inout       (ddr_dq[45]), 
      				.write_data_falling (write_data_falling[45]), 
      				.write_data_rising  (write_data_rising[45]),
      				.read_data_in       (ddr_dq_in[45]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob46 
				(
      				.ddr_dq_inout       (ddr_dq[46]), 
      				.write_data_falling (write_data_falling[46]), 
      				.write_data_rising  (write_data_rising[46]),
      				.read_data_in       (ddr_dq_in[46]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
 s3_ddr_iob  s3_ddr_iob47 
				(
      				.ddr_dq_inout       (ddr_dq[47]), 
      				.write_data_falling (write_data_falling[47]), 
      				.write_data_rising  (write_data_rising[47]),
      				.read_data_in       (ddr_dq_in[47]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob48 
				(
      				.ddr_dq_inout       (ddr_dq[48]), 
      				.write_data_falling (write_data_falling[48]), 
      				.write_data_rising  (write_data_rising[48]),
      				.read_data_in       (ddr_dq_in[48]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob49 
				(
      				.ddr_dq_inout       (ddr_dq[49]), 
      				.write_data_falling (write_data_falling[49]), 
      				.write_data_rising  (write_data_rising[49]),
      				.read_data_in       (ddr_dq_in[49]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
 
  s3_ddr_iob  s3_ddr_iob50 
				(
      				.ddr_dq_inout       (ddr_dq[50]), 
      				.write_data_falling (write_data_falling[50]), 
      				.write_data_rising  (write_data_rising[50]),
      				.read_data_in       (ddr_dq_in[50]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob51 
				(
      				.ddr_dq_inout       (ddr_dq[51]), 
      				.write_data_falling (write_data_falling[51]), 
      				.write_data_rising  (write_data_rising[51]),
      				.read_data_in       (ddr_dq_in[51]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob52 
				(
      				.ddr_dq_inout       (ddr_dq[52]), 
      				.write_data_falling (write_data_falling[52]), 
      				.write_data_rising  (write_data_rising[52]),
      				.read_data_in       (ddr_dq_in[52]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob53 
				(
      				.ddr_dq_inout       (ddr_dq[53]), 
      				.write_data_falling (write_data_falling[53]), 
      				.write_data_rising  (write_data_rising[53]),
      				.read_data_in       (ddr_dq_in[53]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob54 
				(
      				.ddr_dq_inout       (ddr_dq[54]), 
      				.write_data_falling (write_data_falling[54]), 
      				.write_data_rising  (write_data_rising[54]),
      				.read_data_in       (ddr_dq_in[54]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
  
 s3_ddr_iob  s3_ddr_iob55 
				(
      				.ddr_dq_inout       (ddr_dq[55]), 
      				.write_data_falling (write_data_falling[55]), 
      				.write_data_rising  (write_data_rising[55]),
      				.read_data_in       (ddr_dq_in[55]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob56 
				(
      				.ddr_dq_inout       (ddr_dq[56]), 
      				.write_data_falling (write_data_falling[56]), 
      				.write_data_rising  (write_data_rising[56]),
      				.read_data_in       (ddr_dq_in[56]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
 s3_ddr_iob  s3_ddr_iob57 
				(
      				.ddr_dq_inout       (ddr_dq[57]), 
      				.write_data_falling (write_data_falling[57]), 
      				.write_data_rising  (write_data_rising[57]),
      				.read_data_in       (ddr_dq_in[57]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob58 
				(
      				.ddr_dq_inout       (ddr_dq[58]), 
      				.write_data_falling (write_data_falling[58]), 
      				.write_data_rising  (write_data_rising[58]),
      				.read_data_in       (ddr_dq_in[58]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob59 
				(
      				.ddr_dq_inout       (ddr_dq[59]), 
      				.write_data_falling (write_data_falling[59]), 
      				.write_data_rising  (write_data_rising[59]),
      				.read_data_in       (ddr_dq_in[59]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
 s3_ddr_iob  s3_ddr_iob60 
				(
      				.ddr_dq_inout       (ddr_dq[60]), 
      				.write_data_falling (write_data_falling[60]), 
      				.write_data_rising  (write_data_rising[60]),
      				.read_data_in       (ddr_dq_in[60]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob61 
				(
      				.ddr_dq_inout       (ddr_dq[61]), 
      				.write_data_falling (write_data_falling[61]), 
      				.write_data_rising  (write_data_rising[61]),
      				.read_data_in       (ddr_dq_in[61]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob62 
				(
      				.ddr_dq_inout       (ddr_dq[62]), 
      				.write_data_falling (write_data_falling[62]), 
      				.write_data_rising  (write_data_rising[62]),
      				.read_data_in       (ddr_dq_in[62]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob63 
				(
      				.ddr_dq_inout       (ddr_dq[63]), 
      				.write_data_falling (write_data_falling[63]), 
      				.write_data_rising  (write_data_rising[63]),
      				.read_data_in       (ddr_dq_in[63]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob64 
				(
      				.ddr_dq_inout       (ddr_dq[64]), 
      				.write_data_falling (write_data_falling[64]), 
      				.write_data_rising  (write_data_rising[64]),
      				.read_data_in       (ddr_dq_in[64]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
  
 s3_ddr_iob  s3_ddr_iob65 
				(
      				.ddr_dq_inout       (ddr_dq[65]), 
      				.write_data_falling (write_data_falling[65]), 
      				.write_data_rising  (write_data_rising[65]),
      				.read_data_in       (ddr_dq_in[65]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob66 
				(
      				.ddr_dq_inout       (ddr_dq[66]), 
      				.write_data_falling (write_data_falling[66]), 
      				.write_data_rising  (write_data_rising[66]),
      				.read_data_in       (ddr_dq_in[66]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
 s3_ddr_iob  s3_ddr_iob67 
				(
      				.ddr_dq_inout       (ddr_dq[67]), 
      				.write_data_falling (write_data_falling[67]), 
      				.write_data_rising  (write_data_rising[67]),
      				.read_data_in       (ddr_dq_in[67]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob68 
				(
      				.ddr_dq_inout       (ddr_dq[68]), 
      				.write_data_falling (write_data_falling[68]), 
      				.write_data_rising  (write_data_rising[68]),
      				.read_data_in       (ddr_dq_in[68]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                        		     
 s3_ddr_iob  s3_ddr_iob69 
				(
      				.ddr_dq_inout       (ddr_dq[69]), 
      				.write_data_falling (write_data_falling[69]), 
      				.write_data_rising  (write_data_rising[69]),
      				.read_data_in       (ddr_dq_in[69]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                       		     
 s3_ddr_iob  s3_ddr_iob70 
				(
      				.ddr_dq_inout       (ddr_dq[70]), 
      				.write_data_falling (write_data_falling[70]), 
      				.write_data_rising  (write_data_rising[70]),
      				.read_data_in       (ddr_dq_in[70]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                       		     
 s3_ddr_iob  s3_ddr_iob71 
				(
      				.ddr_dq_inout       (ddr_dq[71]), 
      				.write_data_falling (write_data_falling[71]), 
      				.write_data_rising  (write_data_rising[71]),
      				.read_data_in       (ddr_dq_in[71]),
      				.clk90              (clk90),
      				.clk270             (clk270),
      				.write_en_val       (write_en_val1),
      				.reset              (reset90_r)
                        		     );
                       		     
             
endmodule
