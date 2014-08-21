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
module cmp_data(
     clk,            
     data_valid,     
     lfsr_data,      
     read_data,      
     rst,
     read_data_out,
     lfsr_data_out,             
     led_error_output, 
     data_valid_out 
     );

input  clk;
input  data_valid;
input  [143:0]lfsr_data/* synthesis syn_keep=1 */; 
input  [143:0]read_data;
input  rst;
output [143:0] read_data_out;
output [143:0] lfsr_data_out; 
output led_error_output;
output data_valid_out;


reg  led_state;     
reg valid;
reg valid_1; 
wire error;
reg [3:0]byte_err;
reg [143:0] read_data_reg; 


 


always @ (posedge clk)
begin
  if (rst == 1'b1)
     byte_err <= 4'd0;
  else begin
     if(read_data_reg[35:0] != lfsr_data[35:0])
        byte_err[0] <= 1'b1;
     else
        byte_err[0] <= 1'b0;
        
     if(read_data_reg[71:36] != lfsr_data[71:36])
        byte_err[1] <= 1'b1;
     else
        byte_err[1] <= 1'b0;
  
       if(read_data_reg[107:72] != lfsr_data[107:72])
        byte_err[2] <= 1'b1;
     else
        byte_err[2] <= 1'b0;
        
      if(read_data_reg[143:108] != lfsr_data[143:108])
        byte_err[3] <= 1'b1;
     else
        byte_err[3] <= 1'b0;
       
   end 
end

always @ (posedge clk)
begin
  if (rst == 1'b1)
    begin
      valid_1 <= 1'b0;
      valid   <= 1'b0;
      read_data_reg <= 144'd0;
    end
  else
    begin      
      valid   <= data_valid;
      valid_1 <= valid;
      read_data_reg <= read_data; 
    end
end
assign data_valid_out = valid;
assign read_data_out = read_data_reg;
assign lfsr_data_out = lfsr_data; 
//assign error = (valid && data_valid && (byte_err[0]  || byte_err[1] || byte_err[2] || byte_err[3]));  
assign error = ( valid_1  && (|byte_err[3:0])); 

 // LED error output
always @ (posedge clk)
begin
      if (rst == 1'b1)
          led_state <= 1'b0;  // no error
      else
       begin
         case(led_state)
         1'b0 : begin
                if (error == 1'b1)
                     led_state <= 1'b1;  // Error
                else
                     led_state <= 1'b0;  // No Error
                end
          1'b1 : led_state <= 1'b1;
          default : led_state <= 1'b0;
         endcase
       end
end                   
assign led_error_output = (led_state == 1'b1) ? 1'b1 : 1'b0;
                                

endmodule

