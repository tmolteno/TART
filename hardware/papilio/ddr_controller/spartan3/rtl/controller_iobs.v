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
module controller_iobs (

    clk0,             
    ddr_rasb_cntrl,   
    ddr_casb_cntrl,   
    ddr_web_cntrl,    
    ddr_cke_cntrl,    
    ddr_csb_cntrl,    
    ddr_address_cntrl,
    ddr_ba_cntrl,     
    rst_dqs_div_int,
    rst_dqs_div_int1,  
    ddr_rasb,       
    ddr_casb,       
    ddr_web,        
    ddr_ba,         
    ddr_address,    
    ddr_cke,        
    ddr_csb,        
    rst_dqs_div,
    rst_dqs_div2,    
    rst_dqs_div_in,
    rst_dqs_div_in2, 
    rst_dqs_div_out2, 
    rst_dqs_div_out
   );
   
   
input    clk0;           
input    ddr_rasb_cntrl; 
input    ddr_casb_cntrl; 
input    ddr_web_cntrl;  
input    ddr_cke_cntrl;  
input    ddr_csb_cntrl;  
input    [12:0]ddr_address_cntrl;
input    [1:0]ddr_ba_cntrl;    
input    rst_dqs_div_int; 
input    rst_dqs_div_int1;  
output    ddr_rasb;        
output    ddr_casb;        
output    ddr_web;         
output    [1:0]ddr_ba;     
output    [12:0]ddr_address; 
output    ddr_cke;         
output    ddr_csb;         
output    rst_dqs_div;     
input	  rst_dqs_div_in; 
output	  rst_dqs_div_out;
output    rst_dqs_div2;     
input	  rst_dqs_div_in2; 
output	  rst_dqs_div_out2;
   
wire clk180 ;

wire ddr_web_q;
wire ddr_rasb_q;
wire ddr_casb_q;

assign clk180 = ~ clk0;

  

           
 FD iob_web  (
                .Q( ddr_web_q),
                .D( ddr_web_cntrl),
                .C( clk180)
              );
                         
 FD iob_rasb  (
                .Q( ddr_rasb_q),
                .D( ddr_rasb_cntrl),
                .C( clk180)
              );
              
              
 FD iob_casb  (
                .Q( ddr_casb_q),
                .D( ddr_casb_cntrl),
                .C( clk180)
              );



//---- ************************************* ----
//----  Output buffers for control signals   ----
//---- ************************************* ----

 OBUF r0  (
            .I( ddr_web_q),
            .O( ddr_web)
          );

 OBUF r1  (
            .I( ddr_rasb_q),
            .O( ddr_rasb)
          );
          
 OBUF r2  (
            .I( ddr_casb_q),
            .O( ddr_casb)
          );
          
 OBUF r3  (
            .I( ddr_cke_cntrl),
            .O( ddr_cke)
          );
          
 OBUF r4  (
            .I( ddr_csb_cntrl),
            .O( ddr_csb)
          );


//---- ************************************* ----
//----  Output buffers for address signals   ----
//---- ************************************* ----

OBUF r5   (
           .I(ddr_address_cntrl[0]),
           .O(ddr_address[0]));

OBUF r6   (
           .I(ddr_address_cntrl[1]),
           .O(ddr_address[1]));

OBUF r7   (
           .I(ddr_address_cntrl[2]),
           .O(ddr_address[2]));

OBUF r8   (
           .I(ddr_address_cntrl[3]),
           .O(ddr_address[3]));

OBUF r9   (
           .I(ddr_address_cntrl[4]),
           .O(ddr_address[4]));

OBUF r10   (
           .I(ddr_address_cntrl[5]),
           .O(ddr_address[5]));

OBUF r11  (
           .I(ddr_address_cntrl[6]),
           .O(ddr_address[6]));

OBUF r12   (
           .I(ddr_address_cntrl[7]),
           .O(ddr_address[7]));


OBUF r13   (
           .I(ddr_address_cntrl[8]),
           .O(ddr_address[8]));

OBUF r14   (
           .I(ddr_address_cntrl[9]),
           .O(ddr_address[9]));

OBUF r15   (
           .I(ddr_address_cntrl[10]),
           .O(ddr_address[10]));

OBUF r16   (
           .I(ddr_address_cntrl[11]),
           .O(ddr_address[11]));

OBUF r17   (
           .I(ddr_address_cntrl[12]),
           .O(ddr_address[12]));





 OBUF r18  (
               .I(ddr_ba_cntrl[0]),
               .O(ddr_ba[0]));

 OBUF r19  (
               .I(ddr_ba_cntrl[1]),
               .O(ddr_ba[1]));


                        
//IBUF rst_iob_inbuf  
IBUF_SSTL2_II rst_iob_inbuf 
                            ( .I(rst_dqs_div_in),
                              .O(rst_dqs_div));
  

                            
//OBUF rst_iob_outbuf 
OBUF_SSTL2_II rst_iob_inbuf1  
                            ( .I(rst_dqs_div_int),
                              .O(rst_dqs_div_out));

//IBUF rst_iob_inbuf2
IBUF_SSTL2_II rst_iob_inbuf2  
                            ( .I(rst_dqs_div_in2),
                              .O(rst_dqs_div2));
  

                            
//OBUF rst_iob_outbuf2
OBUF_SSTL2_II rst_iob_outbuf  
                            ( .I(rst_dqs_div_int1),
                              .O(rst_dqs_div_out2));


endmodule




