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
module iobs (

     SYS_CLK,
     SYS_CLKb,
     clk,     
     clk90,  
     JP1_header_infra,
     JP2_header_infra, 
     ddr_rasb_cntrl,   
     ddr_casb_cntrl,   
     ddr_web_cntrl,    
     ddr_cke_cntrl,    
     ddr_csb_cntrl,    
     ddr_address_cntrl,
     ddr_ba_cntrl,     
     rst_dqs_div_int,
     rst_dqs_div_int1,  
     dqs_reset,        
     dqs_enable,       
     ddr_dqs,          
     ddr_dq,           
     write_data_falling,
     write_data_rising,
     write_en_val, 
     write_en_val1,    
     reset90_r,        
     data_mask_f,      
     data_mask_r,      
     sys_clk_ibuf,    

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
     ddr_rasb,         
     ddr_casb,         
     ddr_web,         
     ddr_ba,          
     ddr_address,     
     ddr_cke,         
     ddr_csb,  
     JP1_header,
     JP2_header,        
     rst_dqs_div,     
     rst_dqs_div_in,   
     rst_dqs_div_out,  
     rst_dqs_div2,     
     rst_dqs_div_in2,   
     rst_dqs_div_out2,  
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
     ddr_dm           
);

input     SYS_CLK;           
input     SYS_CLKb;          
input     clk;               
input     clk90;       
input     [7:0] JP1_header_infra;
input     [7:0] JP2_header_infra;      
input     ddr_rasb_cntrl;    
input     ddr_casb_cntrl;    
input     ddr_web_cntrl;     
input     ddr_cke_cntrl;     
input     ddr_csb_cntrl;     
input     [12:0]ddr_address_cntrl; 
input     [1:0]ddr_ba_cntrl;      
input     rst_dqs_div_int;  
input     rst_dqs_div_int1;   
input     dqs_reset;         
input     dqs_enable;        
inout     [8:0]ddr_dqs;      
inout     [71:0]ddr_dq;      
input     [71:0]write_data_falling;
input     [71:0]write_data_rising;
input     write_en_val;   
input     write_en_val1;   
input     reset90_r;        
input     [8:0]data_mask_f;
input     [8:0]data_mask_r; 
output     sys_clk_ibuf;
   

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
    
output     ddr_rasb;          
output     ddr_casb;         
output     ddr_web;          
output     [1:0]ddr_ba;      
output     [12:0]ddr_address;
output     ddr_cke;          
output     ddr_csb;          
output     rst_dqs_div;      
input	   rst_dqs_div_in;  
output     rst_dqs_div_out;
output     rst_dqs_div2;      
input	   rst_dqs_div_in2;  
output     rst_dqs_div_out2;
output     dqs_int_delay_in0;  
output     dqs_int_delay_in1; 
output     dqs_int_delay_in2; 
output     dqs_int_delay_in3;
output     dqs_int_delay_in4; 
output     dqs_int_delay_in5; 
output     dqs_int_delay_in6; 
output     dqs_int_delay_in7; 
output     dqs_int_delay_in8; 
output     [71:0]dq;           
output     [8:0]ddr_dm; 
output     [7:0] JP1_header;
output     [7:0] JP2_header; 
       

infrastructure_iobs infrastructure_iobs0   (
                                                     .SYS_CLK          (SYS_CLK),
                                                     .SYS_CLKb         (SYS_CLKb),
                                                     .clk0             (clk),
                                                     .clk90            (clk90),
                                                     .JP1_header_infra (JP1_header_infra),
                                                     .JP2_header_infra (JP2_header_infra),
                                                     .sys_clk_ibuf     (sys_clk_ibuf),
                                                     

                                                     .JP1_header       (JP1_header),
                                                     .JP2_header       (JP2_header),
                                                     .dimm_clk0        (dimm_clk0),
                                                     .dimm_clk0b       (dimm_clk0b),
                                                     .dimm_clk1        (dimm_clk1),
                                                     .dimm_clk1b       (dimm_clk1b),
                                                     .dimm_clk2        (dimm_clk2),
                                                     .dimm_clk2b       (dimm_clk2b),
                                                     .ddr1_clk0        (ddr1_clk0),
                                                     .ddr1_clk0b       (ddr1_clk0b),
                                                     .ddr1_clk1        (ddr1_clk1),
                                                     .ddr1_clk1b       (ddr1_clk1b),
                                                     .ddr1_clk2        (ddr1_clk2),
                                                     .ddr1_clk2b       (ddr1_clk2b),
                                                     .ddr1_clk3        (ddr1_clk3),
                                                     .ddr1_clk3b       (ddr1_clk3b),
                                                     .ddr1_clk4        (ddr1_clk4),
                                                     .ddr1_clk4b       (ddr1_clk4b)
                                                     );

 controller_iobs controller_iobs0  (
                                             .clk0              (clk),
                                             .ddr_rasb_cntrl    (ddr_rasb_cntrl),
                                             .ddr_casb_cntrl    (ddr_casb_cntrl),
                                             .ddr_web_cntrl     (ddr_web_cntrl), 
                                             .ddr_cke_cntrl     (ddr_cke_cntrl),
                                             .ddr_csb_cntrl     (ddr_csb_cntrl),
                                             .ddr_address_cntrl (ddr_address_cntrl),
                                             .ddr_ba_cntrl      (ddr_ba_cntrl),
                                             .rst_dqs_div_int   (rst_dqs_div_int),
                                             .rst_dqs_div_int1  (rst_dqs_div_int1),
                                             .ddr_rasb          (ddr_rasb),
                                             .ddr_casb          (ddr_casb),
                                             .ddr_web           (ddr_web),
                                             .ddr_ba            (ddr_ba),
                                             .ddr_address       (ddr_address),
                                             .ddr_cke           (ddr_cke),
                                             .ddr_csb           (ddr_csb), 
                                             .rst_dqs_div2       (rst_dqs_div2),
                                             .rst_dqs_div_in2	(rst_dqs_div_in2),
                   		             .rst_dqs_div_out2	 (rst_dqs_div_out2),
                                             .rst_dqs_div       (rst_dqs_div),
                                             .rst_dqs_div_in	   (rst_dqs_div_in),
                   		              .rst_dqs_div_out	  (rst_dqs_div_out)
                   		               );         	
											                                            

 data_path_iobs datapath_iobs0  (
                                         .clk                (clk),
				         .clk90              (clk90),
				         .reset90_r          (reset90_r),
                                         .dqs_reset          (dqs_reset),
                                         .dqs_enable         (dqs_enable),
                                         .ddr_dqs            (ddr_dqs),
                                         .ddr_dq             (ddr_dq),
                                         .write_data_falling (write_data_falling),
                                         .write_data_rising  (write_data_rising),
                                         .write_en_val       (write_en_val),
                                         .write_en_val1      (write_en_val1),
                                         .data_mask_f        (data_mask_f),
                                         .data_mask_r        (data_mask_r),
                                         .dqs_int_delay_in0  (dqs_int_delay_in0),
                                         .dqs_int_delay_in1  (dqs_int_delay_in1),
                                         .dqs_int_delay_in2  (dqs_int_delay_in2),
                                         .dqs_int_delay_in3  (dqs_int_delay_in3),
                                         .dqs_int_delay_in4  (dqs_int_delay_in4),
                                         .dqs_int_delay_in5  (dqs_int_delay_in5),
                                         .dqs_int_delay_in6  (dqs_int_delay_in6),
                                         .dqs_int_delay_in7  (dqs_int_delay_in7),
                                         .dqs_int_delay_in8  (dqs_int_delay_in8), 
                                         .ddr_dq_val         (dq),
                                         .ddr_dm             (ddr_dm)
                                        );

   
endmodule

