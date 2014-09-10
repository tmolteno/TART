//******************************************************************************
//
//  Xilinx, Inc. 2002                 www.xilinx.com
//
//
//*******************************************************************************
//
//  File name :       addr_gen.v
//
//  Description :     This module generates the row address, column address,
//                    and bank address for the write and read bursts. 
// 
//                    
//  Date - revision : 12/17/2003
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

module addr_gen(
                clk,
	        rst,
	        addr_rst,  
	        addr_inc, 
	        addr_out,  
	        column_address, 
	        test_cnt_ena,  
	        test_out, 
	        burst_done,  
	        cnt_roll 
                );

   input        clk;               
   input        rst;
   input        addr_rst;
   input        addr_inc;
   input        test_cnt_ena;
   
   output[23:0] addr_out;
   output[7:0]  column_address;
   output[4:0]  test_out;
   output       burst_done;
   output       cnt_roll;
   
   reg          cnt_roll;
   reg [7:0]    column_counter;
   reg [1:0]    cnt;
   reg          burst_done_reg;
   reg          burst_done_1_reg;
   reg          cnt_roll_p;
   reg          cnt_roll_p2;
   
   wire [1:0]   ba_count;
   
assign ba_count       = 2'b00; 
assign column_address = column_counter;
assign addr_out       = {14'b00000000000100, column_counter, ba_count};
assign burst_done     = burst_done_1_reg;
assign test_out = 5'd0; 


always @ (posedge clk)
begin
   if (rst == 1'b1 || addr_rst == 1'b1)
     begin
        column_counter <= 8'h00;
        cnt <= 2'b00;
     end
   else if (addr_inc == 1'b1)
     begin
        if (cnt == 2'b01)
          cnt <= 2'b00;
        else
          cnt <= cnt + 1'b1;
        if ((test_cnt_ena == 1'b1) && (cnt == 2'b01))
          begin
           if (column_counter == 8'hf0)
//           if (column_counter == 8'h08)
//           if (column_counter == 8'h04)
              column_counter <= 8'h00;
           else 
              column_counter <= column_counter + 4'h4;
          end
        else 
            column_counter <= column_counter;
     end
end

always @ (posedge clk)
begin
  if (rst == 1'b1)
    begin
     burst_done_reg   <= 1'b0;
     burst_done_1_reg <= 1'b0;
    end
  else if (column_counter == 8'b11101100)
//  else if (column_counter == 8'b0100)//4
//   else if (column_counter == 8'b0)//0
    begin
     burst_done_reg <= 1'b1;
     burst_done_1_reg <= burst_done_reg;
    end
  else
    begin
     burst_done_reg <= 1'b0;
     burst_done_1_reg <= burst_done_reg;
    end
end

always @ (posedge clk)
begin
  if (rst == 1'b1)
     cnt_roll_p <= 1'b0;
  else if (column_counter == 8'b11101100)//236
//  else if (column_counter == 8'b0100)//4
//   else if (column_counter == 8'b0)//0
     cnt_roll_p <= 1'b1;
  else
     cnt_roll_p <= 1'b0;
end

always @ (posedge clk)
begin
  if (rst == 1'b1)
    begin
     cnt_roll_p2 <= 1'b0;
     cnt_roll    <= 1'b0;
    end
  else
    begin
     cnt_roll_p2  <= cnt_roll_p;
     cnt_roll     <= cnt_roll_p2;
    end
end


/*
// To TEST SIngle BUSRT
always @ (posedge clk)
begin
  if (rst == 1'b1)
     cnt_roll_p <= 1'b0;
  else
  begin   
     if(addr_inc == 1'b1)
     begin
//        cnt_roll_p <= 1'b1;
        cnt_roll <= 1'b1;
     end  
     else
     begin
//       cnt_roll_p <= 1'b0;
        cnt_roll <= 1'b0;
     end  
//     cnt_roll   <= cnt_roll_p;
  end   
end

always @ (posedge clk)
begin
  if (rst == 1'b1)
    begin
     burst_done_reg   <= 1'b0;
     burst_done_1_reg <= 1'b0;
     column_counter <= 4'd4;
    end
   else if (addr_inc == 1'b1)
    begin
     burst_done_reg <= 1'b1;
     burst_done_1_reg <= burst_done_reg;
    end
  else
    begin
     burst_done_reg <= 1'b0;
     burst_done_1_reg <= burst_done_reg;
    end
end
*/

endmodule   
