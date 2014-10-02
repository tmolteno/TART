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
module data_write (

     user_input_data,    
     clk90,              
     reset90_r,          
     reset270_r,         
     write_enable,       
     write_en_val,
     write_en_val1,       
     write_data_falling, 
     write_data_rising,  
     data_mask_f,
     data_mask_r
     );


input     [143:0]user_input_data;
input     clk90;              
input     reset90_r;          
input     reset270_r;         
input     write_enable;       
output    write_en_val;  
output    write_en_val1;      
output    [71:0]write_data_falling; 
output    [71:0]write_data_rising;  
output    [8:0]data_mask_f;        
output    [8:0]data_mask_r;   

reg write_en_val;  /* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
reg write_en_val1;  /* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */

reg write_en_P1;          
reg write_en_P2;          
reg write_en_P3;          
reg write_en_int;         
reg [143:0]write_data;    
reg [143:0]write_data1;   
reg [143:0]write_data2;   
reg [143:0]write_data3;   
reg [143:0]write_data4;   
reg [143:0]write_data5;   
reg [143:0]write_data6;   
reg [143:0]write_data_int;
reg [71:0]write_data270_1;
reg [71:0]write_data270_2;
  
  
  
assign data_mask_f = 9'b000000000;
assign data_mask_r = 9'b000000000;
     
always@(posedge clk90)
begin
    if (reset90_r == 1'b1)
    begin
       write_data_int   <= 144'd0;
       write_data1      <= 144'd0;
       write_data2      <= 144'd0;
       write_data3      <= 144'd0;
       write_data4      <= 144'd0;
       write_data5      <= 144'd0;
       write_data6      <= 144'd0;
       write_data       <= 144'd0;
    end
    else
    begin
       write_data_int         <= user_input_data;
       write_data1            <= write_data_int;
       write_data2            <= write_data1;
       write_data3            <= write_data2;
       write_data4            <= write_data3;
       write_data5            <= write_data4;
       write_data6            <= write_data5;
       write_data             <= write_data6;
     end
end

always@(negedge clk90)
begin
   if (reset270_r == 1'b1)
   	begin
        write_data270_1  <= 72'd0;
        write_data270_2  <= 72'd0;
        end
   else
   	begin 
        write_data270_1  <= write_data5[143:72];
        write_data270_2  <= write_data270_1;
         end 
end

assign write_data_rising  = write_data270_2;
assign write_data_falling = write_data[71:0];


////--------------------------------------------------------------------------------
// data path for write enable
always@(posedge clk90)
begin
   if(reset90_r== 1'b1)
   begin
    	write_en_P1 <= 1'b0;
    	write_en_P2 <= 1'b0;
    	write_en_P3 <= 1'b0;
   end
   else
   begin 
        write_en_P1 <= write_enable;
        write_en_P2 <= write_en_P1;
        write_en_P3 <= write_en_P2;
    end    
end

always@(negedge clk90)
begin
  if (reset90_r == 1'b1)
  	begin
        write_en_int    <= 1'b0;
        write_en_val    <= 1'b0;
        write_en_val1  <= 1'b0;
        end
  else
  	begin
     	write_en_int   <= write_en_P2;//P2
     	write_en_val   <= write_en_int; //int;
     	write_en_val1 <= write_en_int;
 	end
end

endmodule
