//******************************************************************************
//
//  Xilinx, Inc. 2002                 www.xilinx.com
//
//
//*******************************************************************************
//
//  File name :       cmd_fsm.v
//
//  Description :     This module generates the error signal in case of bit errors.
//                    It compares the read data witht the write data. 
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

module cmd_fsm(
                clk,
                clk90,
	        rst,
	        rst180,
	        rst90,
	        cmd_ack,  
	        cnt_roll,
	        dip2, 
	        dly_tc,  
	        r_w, 
	        refresh_done,  
	        init_val, 
	        u_data_val,  
	        addr_inc,
	        addr_rst,
	        u_cmd,
	        dly_inc,
	        init_counter,
	        lfsr_rst
                );

   input         clk;               
   input         clk90;
   input         rst;
   input         rst180;
   input         rst90;
   input         cmd_ack;
   input         cnt_roll;   
   input         dip2;
   input         dly_tc; 
   input         r_w; 
   input         refresh_done; 
   input         init_val; 
   input         u_data_val;                           
   
   output        addr_inc;
   output        addr_rst;
   output[2:0]   u_cmd;
   output        dly_inc;   
   output[6:0]   init_counter;
   output        lfsr_rst;
   
       parameter [3:0] rst_state  = 0,
                       init_start = 1,
		       init       = 2,
		       wr         = 3,
		       rlfsr      = 4,
		       dly        = 5,
		       auto_ref_start = 6,
		       auto_ref   = 7,
		       rd         = 8,
		       wait_state = 9,
		       load_mode_wr = 10,      //A
		       lmd_wait_state = 11;      //B
   
   reg[2:0]    u_cmd;
   reg         addr_inc;
   reg         addr_rst;
   reg         dly_inc;
   reg[3:0]    next_state;
   reg[3:0]    next_state1;
   reg[3:0]    current_state;
   reg[6:0]    cmd;
   reg[5:0]    init_dly;
   reg[6:0]    state_bits;
   reg         lfsr_rst_180;
   reg         lfsr_rst_90;
   reg[3:0]    num_burst;
   reg         init_done;
   
   wire[6:0]   cmd_p;   
   wire[5:0]   init_dly_p;
   wire        init_chek;
   wire[2:0]   u_cmd_p;
   wire        addr_inc_p;
   wire        addr_rst_p;
   wire        dly_inc_p;
   wire        lfsr_rst_p;
   wire[3:0]   num_bursts_max;
   wire[3:0]   num_burst_done;
   wire[4:0]   LMD_WAIT_COUNT_value;
   reg[4:0]    LMD_WAIT_COUNT;
   
   

assign lfsr_rst       = lfsr_rst_90;
assign init_counter   = next_state;
assign num_bursts_max = 4'hf;

assign LMD_WAIT_COUNT_value = (next_state == lmd_wait_state) ? 5'b10101 :
                              (LMD_WAIT_COUNT != 5'b00001) ? (LMD_WAIT_COUNT - 5'b00001) :
                               LMD_WAIT_COUNT;



assign num_burst_done = (next_state == init_start) ? num_bursts_max :
                        (next_state == rlfsr) ? num_burst - 1'b1 :
                        num_burst;


assign cmd_p          = (next_state == init_start )? 7'b0001000 :  
                        (next_state == wr) ? 7'b0010000 :
                        (next_state == rd) ? 7'b0100000 :
                        (next_state == auto_ref_start) ? 7'b1000000 :
                        (next_state == load_mode_wr) ? 7'b0000001 :
                        7'b0000000;
          
assign u_cmd_p        = (cmd == 7'b0100000) ? 3'b110 :       // read
                        (cmd == 7'b0010000) ? 3'b100 :       // write
                        (cmd == 7'b0001000) ? 3'b010 :       // init
                        (cmd == 7'b1000000) ? 3'b011 :       // auto_refresh
                        (cmd == 7'b0000001) ? 3'b101 :       // load_mode_wr
                        3'b000;
         
//assign addr_inc_p     = ((cnt_roll == 1'b0 &&  cmd_ack == 1'b1) && (next_state == wr || next_state == rd)) ? 1'b1 : 1'b0;              
assign addr_inc_p     = ((cmd_ack == 1'b1) && (next_state1 == wr || next_state1 == rd)) ? 1'b1 : 1'b0;              
                     
assign addr_rst_p     = (next_state == rlfsr) ? 1'b1 : 1'b0;
              
assign dly_inc_p      = (next_state == dly) ? 1'b1 : 1'b0;         
              
assign lfsr_rst_p     = (refresh_done == 1'b1) ? 1'b1 : 1'b0;
                                 
assign init_dly_p     = (next_state == init_start) ? 6'b111111 :
                        (init_dly != 6'b000000) ? init_dly - 1'b1 :
                        6'b000000; 
              
assign init_chek      = init_dly[5] || init_dly[4] || init_dly[3] || init_dly[2] || init_dly[1] || init_dly[0];

always @ (negedge clk)
begin
  if (rst180 == 1'b1)
    lfsr_rst_180  <= 1'b0;
  else
    lfsr_rst_180  <= lfsr_rst_p;
end

always @ (posedge clk90)
begin
  if (rst90 == 1'b1)
    lfsr_rst_90  <= 1'b0;
  else
    lfsr_rst_90  <= lfsr_rst_180;
end

always @ (posedge clk)
begin
  if (rst == 1'b1)
    begin
      u_cmd <= 3'b000;
      cmd   <= 7'b0000000;
      LMD_WAIT_COUNT <= 5'b0; 
    end
  else
    begin
      u_cmd <= u_cmd_p;
      cmd   <= cmd_p;
      LMD_WAIT_COUNT <= LMD_WAIT_COUNT_value;
    end
end

always @ (posedge clk)
begin
   if (rst == 1'b1)
     begin
       addr_inc  <= 1'b0;
       addr_rst  <= 1'b0;
       init_dly  <= 6'b000000;
       num_burst <= 4'b1111;
     end
    else
     begin
       addr_inc  <= addr_inc_p;
       addr_rst  <= addr_rst_p;
       init_dly  <= init_dly_p;
       num_burst <= num_burst_done;
     end
end


always @ (posedge clk90)
begin
  if (rst90 == 1'b1)
    dly_inc   <= 1'b0;
  else
    dly_inc   <= dly_inc_p;
end




always @ (posedge clk)
begin
   if (rst == 1'b1)
       init_done <= 1'b0;
   else
       init_done <= init_val;
end


always @ (next_state or rst or cnt_roll or r_w or dly_tc or refresh_done or dip2 or init_done or LMD_WAIT_COUNT)
//always @ (posedge clk)
begin
  if (rst == 1'b1)
      next_state1 <= rst_state;
  else
    begin
      case (next_state)
         //rst
         4'b0000 : begin
                     state_bits <= 7'b0000001;   //  01 
                     if (dip2 == 1'b1)
                        next_state1 <= init_start;
                     else
                        next_state1 <= rst_state;
                   end
         //init_start            
         4'b0001 : begin
                      state_bits <= 7'b0000011;   //  03
                      next_state1 <= init;
                   end   
         //init  
         4'b0010 : begin
                     state_bits <= 7'b0000010;   //  02
                     if (init_done == 1'b1)
                         next_state1 <= wr;
                     else
                         next_state1 <= init;
                   end
          //wr
          4'b0011 : begin
                     state_bits <= 7'b0000100;   //  04
                     if (cnt_roll == 1'b0)
                        next_state1 <= wr;
                     else
                        next_state1 <= dly;
                    end
          //lfsr     
          4'b0100 : begin
                     state_bits <= 7'b0001000;   //  08
                     if (r_w == 1'b0)
                         next_state1 <= wr;
                     else if (r_w == 1'b1)
                        next_state1 <= rd;
                    end
          //dly          
          4'b0101 : begin
                     state_bits <= 7'b0010000;   //  10
                     if (dly_tc == 1'b1)
                        next_state1 <= auto_ref_start;
                     else
                        next_state1 <= dly;
                    end
          //auot_ref_start                     
          4'b0110 : begin
                     next_state1 <= auto_ref;           
                    end
          //auoto_ref           
          4'b0111 : begin
                     if (refresh_done == 1'b1)
                         next_state1 <= rlfsr;
                     else
                         next_state1 <= auto_ref;
                    end
          //rd                    
          4'b1000 : begin
                     state_bits <= 7'b0100000;   //  20
                     if (cnt_roll == 1'b0)
                         next_state1 <= rd;
                     else
                         next_state1 <= lmd_wait_state;
//                         next_state1 <= dly;
                    end
         //lmd_wait_state
          4'b1011 : begin
                     state_bits <= 7'b1000010;   //  42
//                     next_state1 <= load_mode_wr;
                     next_state1 <= wait_state;
                    end

         //wait_state
          4'b1001 : begin
                     state_bits <= 7'b1000001;   //  41
                     if(LMD_WAIT_COUNT == 5'b00001)
                         next_state1 <= load_mode_wr;
                     else   
                        next_state1 <= wait_state;           
                    end

          //load_mode_wr          
          4'b1010 : begin
                     state_bits <= 7'b1000000;   //  40
                        next_state1 <= dly;           
                    end


          default : begin
                     next_state1 <= rst_state;
                     state_bits <= 7'b0000001;
                    end
      endcase
    end
end


always @ (posedge clk)
begin
  if (rst == 1'b1)
   begin
    current_state <= rst_state;
    next_state    <= rst_state;
   end
  else
   begin
    next_state    <= next_state1;
    current_state <= next_state;
   end
end

endmodule
