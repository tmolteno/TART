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
module infrastructure_iobs (

      SYS_CLK,           
      SYS_CLKb,          
      clk0,              
      clk90,             
      sys_clk_ibuf, 
      JP1_header_infra,
      JP2_header_infra,      
      
      JP1_header,
      JP2_header,
      dimm_clk0,         
      dimm_clk0b,        
      dimm_clk1,         
      dimm_clk1b,        
      dimm_clk2,         
      dimm_clk2b,    
      ddr1_clk0,         
      ddr1_clk0b,        
      ddr1_clk1,         
      ddr1_clk1b,        
      ddr1_clk2,         
      ddr1_clk2b,        
      ddr1_clk3,         
      ddr1_clk3b,        
      ddr1_clk4,         
      ddr1_clk4b        
      );
      
input      SYS_CLK;           
input      SYS_CLKb;          
input      clk0;              
input      clk90;     
input      [7:0] JP1_header_infra;
input      [7:0] JP2_header_infra;        
output      sys_clk_ibuf;  
  
output      dimm_clk0;         
output      dimm_clk0b;        
output      dimm_clk1;         
output      dimm_clk1b;        
output      dimm_clk2;         
output      dimm_clk2b;        
output      ddr1_clk0;         
output      ddr1_clk0b;        
output      ddr1_clk1;         
output      ddr1_clk1b;        
output      ddr1_clk2;         
output      ddr1_clk2b;        
output      ddr1_clk3;         
output      ddr1_clk3b;        
output      ddr1_clk4;         
output      ddr1_clk4b;
output      [7:0] JP1_header;
output      [7:0] JP2_header;
        
   
   
wire ddr1_clk0_q; 
wire ddr1_clk0b_q;
wire ddr1_clk1_q; 
wire ddr1_clk1b_q;
wire ddr1_clk2_q; 
wire ddr1_clk2b_q;
wire ddr1_clk3_q; 
wire ddr1_clk3b_q;
wire ddr1_clk4_q; 
wire ddr1_clk4b_q;
wire vcc;   
wire gnd;   
wire clk180;
wire clk270;
   
   
assign clk180 = ~ clk0;
assign  clk270 = ~ clk90;
assign  gnd = 1'b0;
assign  vcc = 1'b1;   





//----  Component instantiations  ----

//--- ***********************************
//---  DCI Input buffer for System clock
//---   
//--- ***********************************

IBUFGDS_LVDS_25 lvds_clk_input   ( 
                                           .I  (SYS_CLK),      
                                           .IB (SYS_CLKb),     
                                           .O  (sys_clk_ibuf)
                                           );

//---- ***********************************************************
//----     Output DDR generation
//----     This includes instantiation of the output DDR flip flop
//----     for ddr clk's and dimm clk's
//---- ***********************************************************


FDDRRSE DDRCLK0_INST    ( .Q  (ddr1_clk0_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (vcc), 
                          .D1 (gnd), 
                          .R  (gnd), 
                          .S  (gnd)
                          );

FDDRRSE DDRCLK0B_INST    ( .Q  (ddr1_clk0b_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (gnd), 
                          .D1 (vcc), 
                          .R  (gnd), 
                          .S  (gnd)
                          );
                                   
                                   
FDDRRSE DDRCLK1_INST    ( .Q  (ddr1_clk1_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (vcc), 
                          .D1 (gnd), 
                          .R  (gnd), 
                          .S  (gnd)
                          );

FDDRRSE DDRCLK1B_INST    ( .Q  (ddr1_clk1b_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (gnd), 
                          .D1 (vcc), 
                          .R  (gnd), 
                          .S  (gnd)
                          );
                                   
FDDRRSE DDRCLK2_INST    ( .Q  (ddr1_clk2_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (vcc), 
                          .D1 (gnd), 
                          .R  (gnd), 
                          .S  (gnd)
                          );

FDDRRSE DDRCLK2B_INST    ( .Q  (ddr1_clk2b_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (gnd), 
                          .D1 (vcc), 
                          .R  (gnd), 
                          .S  (gnd)
                          );


FDDRRSE DDRCLK3_INST    ( .Q  (ddr1_clk3_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (vcc), 
                          .D1 (gnd), 
                          .R  (gnd), 
                          .S  (gnd)
                          );
                                   
FDDRRSE DDRCLK3B_INST    ( .Q  (ddr1_clk3b_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (gnd), 
                          .D1 (vcc), 
                          .R  (gnd), 
                          .S  (gnd)
                          );

FDDRRSE DDRCLK4_INST    ( .Q  (ddr1_clk4_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (vcc), 
                          .D1 (gnd), 
                          .R  (gnd), 
                          .S  (gnd)
                          );

FDDRRSE DDRCLK4B_INST    ( .Q  (ddr1_clk4b_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (gnd), 
                          .D1 (vcc), 
                          .R  (gnd), 
                          .S  (gnd)
                          );
                                  

FDDRRSE DDRDIMM0_INST    ( .Q  (dimm_clk0_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (vcc), 
                          .D1 (gnd), 
                          .R  (gnd), 
                          .S  (gnd)
                          );

FDDRRSE DDRDIMM0B_INST   ( .Q  (dimm_clk0b_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (gnd), 
                          .D1 (vcc), 
                          .R  (gnd), 
                          .S  (gnd)
                          ); 

FDDRRSE DDRDIMM1_INST    ( .Q  (dimm_clk1_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (vcc), 
                          .D1 (gnd), 
                          .R  (gnd), 
                          .S  (gnd)
                          );

FDDRRSE DDRDIMM1B_INST   ( .Q  (dimm_clk1b_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (gnd), 
                          .D1 (vcc), 
                          .R  (gnd), 
                          .S  (gnd)
                          ); 

FDDRRSE DDRDIMM2_INST    ( .Q  (dimm_clk2_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (vcc), 
                          .D1 (gnd), 
                          .R  (gnd), 
                          .S  (gnd)
                          );

FDDRRSE DDRDIMM2B_INST   ( .Q  (dimm_clk2b_q), 
                          .C0 (clk0), 
                          .C1 (clk180), 
                          .CE (vcc), 
                          .D0 (gnd), 
                          .D1 (vcc), 
                          .R  (gnd), 
                          .S  (gnd)
                          ); 



//---- ******************************************
//---- Ouput BUffers for ddr clk's and dimm clk's
//---- ******************************************


OBUF r1  ( .I(ddr1_clk0_q),  .O(ddr1_clk0));

OBUF r2  ( .I(ddr1_clk0b_q),  .O(ddr1_clk0b));

OBUF r3  ( .I(ddr1_clk1_q),  .O(ddr1_clk1));

OBUF r4  ( .I(ddr1_clk1b_q),  .O(ddr1_clk1b));

OBUF r5  ( .I(ddr1_clk2_q),  .O(ddr1_clk2));

OBUF r6  ( .I(ddr1_clk2b_q),  .O(ddr1_clk2b));

OBUF r7  ( .I(ddr1_clk3_q),  .O(ddr1_clk3));

OBUF r8  ( .I(ddr1_clk3b_q),  .O(ddr1_clk3b));

OBUF r9  ( .I(ddr1_clk4_q),  .O(ddr1_clk4));

OBUF r10  ( .I(ddr1_clk4b_q),  .O(ddr1_clk4b));

OBUF r11  ( .I(dimm_clk0_q),  .O(dimm_clk0));

OBUF r12  ( .I(dimm_clk0b_q),  .O(dimm_clk0b));

OBUF r13  ( .I(dimm_clk1_q),  .O(dimm_clk1));

OBUF r14  ( .I(dimm_clk1b_q),  .O(dimm_clk1b));

OBUF r15  ( .I(dimm_clk2_q),  .O(dimm_clk2));

OBUF r16  ( .I(dimm_clk2b_q),  .O(dimm_clk2b));

OBUF r17  ( .I(JP1_header_infra[0]),  .O(JP1_header[0]));

OBUF r18  ( .I(JP1_header_infra[1]),  .O(JP1_header[1]));

OBUF r19  ( .I(JP1_header_infra[2]),  .O(JP1_header[2]));

OBUF r20  ( .I(JP1_header_infra[3]),  .O(JP1_header[3]));

OBUF r21  ( .I(JP1_header_infra[4]),  .O(JP1_header[4]));

OBUF r22  ( .I(JP1_header_infra[5]),  .O(JP1_header[5]));

OBUF r23  ( .I(JP1_header_infra[6]),  .O(JP1_header[6]));

OBUF r24  ( .I(JP1_header_infra[7]),  .O(JP1_header[7]));


OBUF r25  ( .I(JP2_header_infra[0]),  .O(JP2_header[0]));

OBUF r26  ( .I(JP2_header_infra[1]),  .O(JP2_header[1]));

OBUF r27  ( .I(JP2_header_infra[2]),  .O(JP2_header[2]));

OBUF r28  ( .I(JP2_header_infra[3]),  .O(JP2_header[3]));

OBUF r29  ( .I(JP2_header_infra[4]),  .O(JP2_header[4]));

OBUF r30  ( .I(JP2_header_infra[5]),  .O(JP2_header[5]));

OBUF r31  ( .I(JP2_header_infra[6]),  .O(JP2_header[6]));

OBUF r32  ( .I(JP2_header_infra[7]),  .O(JP2_header[7]));


endmodule

