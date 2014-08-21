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
module infrastructure (

       reset_in,         
       sys_clk_ibuf,  
       read_out_data1, 
       pass1,   
       rst_calib1,       
       delay_sel_val1_val,  
       sys_rst_val,         
       sys_rst90_val,    
       sys_rst180_val,   
       sys_rst270_val,   
       clk_int_val,  
       JP1_header_infra,
       JP2_header_infra,    
       clk90_int_val          
  );
  
input        reset_in;
input        sys_clk_ibuf;
input        rst_calib1;  
input        [143:0] read_out_data1;  
input        pass1;    
output       [4:0]delay_sel_val1_val;   
output       sys_rst_val;          
output       sys_rst90_val;        
output       sys_rst180_val;       
output       sys_rst270_val;       
output       clk_int_val;          
output       clk90_int_val;
output       [7:0] JP1_header_infra;
output       [7:0] JP2_header_infra;         

wire user_rst;
wire clk_int; 
wire clk90_int;
wire dcm_lock; 

reg sys_rst_o;              
reg sys_rst_1;              
reg sys_rst;                
reg sys_rst90_o;            
reg sys_rst90_1;            
reg sys_rst90;              
reg sys_rst180_o;           
reg sys_rst180_1;           
reg sys_rst180;             
reg sys_rst270_o;          
reg sys_rst270_1;          
reg sys_rst270;            
wire [4:0]delay_sel_val;    
wire [4:0]delay_sel_val1;   
reg [4:0]delay_sel_val1_r; 
reg rst_calib1_r1;
reg rst_calib1_r2;

wire stuck_at1;  
wire vcc;

wire clk_int_val1;
wire clk_int_val2;
wire clk90_int_val1;
wire clk90_int_val2;


assign clk_int_val = clk_int;
assign clk90_int_val = clk90_int;
assign stuck_at1 = 1'b0; 

assign sys_rst_val = sys_rst;
assign sys_rst90_val = sys_rst90;
assign sys_rst180_val = sys_rst180;
assign sys_rst270_val = sys_rst270;

assign delay_sel_val1_val = delay_sel_val1;


//-----   To remove delta delays in the clock signals observed during simulation ,Following signals are used 

assign clk_int_val1 = clk_int;
assign clk90_int_val1 = clk90_int;
assign clk_int_val2 = clk_int_val1;
assign clk90_int_val2 = clk90_int_val1;
assign vcc       = 1'b1;
assign user_rst  = ~ reset_in; 
assign JP1_header_infra = {read_out_data1[71:66] , stuck_at1 , pass1};
assign JP2_header_infra = {read_out_data1[65:63] , delay_sel_val};                                       

assign delay_sel_val1 = (rst_calib1 == 1'b0 && rst_calib1_r2 == 1'b0) ? delay_sel_val :delay_sel_val1_r;


always@(posedge clk_int_val2)
begin
    if(user_rst == 1'b1 || dcm_lock == 1'b0)
      begin
      sys_rst_o <= 1'b1;
      sys_rst_1 <= 1'b1;
      sys_rst   <= 1'b1;
      end  
   else
      begin
      sys_rst_o <= 1'b0;
      sys_rst_1 <= sys_rst_o;
      sys_rst   <= sys_rst_1;
      end
end      

always@(posedge clk_int_val2)
begin
   if (sys_rst == 1'b1)
     begin  
     delay_sel_val1_r <= 5'b00000;
     rst_calib1_r1    <= 1'b0;
     rst_calib1_r2    <= 1'b0;
     end
   else
     begin
     delay_sel_val1_r <= delay_sel_val1;
     rst_calib1_r1    <= rst_calib1;
     rst_calib1_r2    <= rst_calib1_r1;
     end
end

always@(posedge clk90_int_val2)
begin
  if (user_rst == 1'b1 || dcm_lock == 1'b0)
      begin
      sys_rst90_o <= 1'b1;
      sys_rst90_1 <= 1'b1;
      sys_rst90   <= 1'b1;
      end
  else
      begin
      sys_rst90_o <= 1'b0;
      sys_rst90_1 <= sys_rst90_o;
      sys_rst90   <= sys_rst90_1;
      end
end


always@(negedge clk_int_val2)
begin
  if (user_rst == 1'b1 || dcm_lock == 1'b0)
      begin 
      sys_rst180_o <= 1'b1;
      sys_rst180_1 <= 1'b1;
      sys_rst180   <= 1'b1;
      end
  else
      begin
      sys_rst180_o <= 1'b0;
      sys_rst180_1 <= sys_rst180_o;
      sys_rst180   <= sys_rst180_1;
      end
end      


always@(negedge clk90_int_val2)
begin
  if (user_rst == 1'b1 || dcm_lock == 1'b0)
      begin
      sys_rst270_o <= 1'b1;
      sys_rst270_1 <= 1'b1;
      sys_rst270   <= 1'b1;
      end
  else
      begin
      sys_rst270_o <= 1'b0;
      sys_rst270_1 <= sys_rst270_o;
      sys_rst270   <= sys_rst270_1;
      end

end 
///----  Component instantiations  ----

                                   
 clk_dcm clk_dcm0  (
                             .input_clk   ( sys_clk_ibuf),
                             .rst         ( user_rst),                            
                             .clk         ( clk_int),
                             .clk90       ( clk90_int),
                             .dcm_lock    ( dcm_lock)
                            ); 
                            
 cal_top cal_top0 (                                                 
                             
                             .clk0        ( clk_int_val2),          
                             .clk0dcmlock ( dcm_lock),  
                             .reset       ( reset_in),      
                             //.okToSelTap  ( vcc),
                             .tapForDqs   ( delay_sel_val)
                             );       
                                          
endmodule


