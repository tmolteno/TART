//******************************************************************************
//
//  Xilinx, Inc. 2002                 www.xilinx.com
//
//
//*******************************************************************************
//
//  File name :       controller.v
//
//  Description :         
//                    Main DDR SDRAM controller block. This includes the following
//                    features:
//                    - The controller state machine that controls the 
//                    initialization process upon power up, as well as the 
//                    read, write, and refresh commands. 
//                    - Accepts and decodes the user commands.
//                    - Generates the address and Bank address signals
//                    - Generates control signals for other modules, including
//                    the control signals for the dqs_en block.
// 
//                    
//  Date - revision : 12/9/2003
//
//  Author :          Maria George (verilog conversion)
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
module controller(
                 dip1,
	         clk,
		 rst0,
	         rst180,
	         address,
	         bank_address,
	         config_register,
	         command_register,
	         burst_done,
		 ddr_rasb_cntrl,
	         ddr_casb_cntrl,
	         ddr_web_cntrl,
	         ddr_ba_cntrl,
	         ddr_address_cntrl,
	         ddr_cke_cntrl,
	         ddr_csb_cntrl,
	         dqs_enable,
	         dqs_reset,
	         write_enable,
	         rst_calib,
	         rst_dqs_div_int,
                 rst_dqs_div_int1,
	         cmd_ack,
	         init,
	         ar_done
                 );

   input          dip1;                         
   input          clk;             
  
   input          rst0;            
   input 	        rst180;          
   input[21:0] 	  address;         
   input[1:0] 	  bank_address;    
   input[9:0] 	  config_register; 
   input[2:0] 	  command_register;
   input          burst_done;
  
   output         ddr_rasb_cntrl;    
   output         ddr_casb_cntrl;    
   output         ddr_web_cntrl;    
   output[1:0]    ddr_ba_cntrl;     
   output[12:0]   ddr_address_cntrl;
   output         ddr_cke_cntrl; 
   output         ddr_csb_cntrl;     
   output         dqs_enable;  
   output         dqs_reset;  
   output         write_enable;
   output         rst_calib;   
   output         rst_dqs_div_int;
   output         rst_dqs_div_int1;
   output         cmd_ack;
   output         init;  
   output         ar_done;
   
   reg  [12:0]ddr_address_cntrl;
   reg  [1:0]ddr_ba_cntrl;   
   
parameter [3:0] IDLE = 0,
                PRECHARGE = 1,
                LOAD_MODE_REG = 2,
                AUTO_REFRESH =3,
                ACTIVE = 4,
                FIRST_WRITE =5,
                WRITE_WAIT = 6,
                BURST_WRITE = 7,
                READ_AFTER_WRITE = 8,
                PRECHARGE_AFTER_WRITE = 9,
                PRECHARGE_AFTER_WRITE_2 = 10,
                READ_WAIT =11,
                BURST_READ = 12,
                BURST_STOP = 13;

reg          ar_done;
reg          write_enable;
reg [3:0]    current_state; 
reg [3:0]    next_state;
reg [3:0]    next_state1;

wire         ack_reg;
wire         ack_o;
reg [21:0]   address_reg;
wire [12:0]  address_config;
reg          auto_ref;
wire         auto_ref1;
wire         AUTOREF_value;
reg          AUTO_REF_detect;
reg          AUTO_REF_detect1;
reg          AUTO_REF_pulse_end;
reg [11:0]   AUTOREF_COUNT;
wire [11:0]  AUTOREF_CNT_val;
reg          auto_refresh_cmd;
wire         AR_done_p;
reg          Burst_terminate;
reg [1:0]    BA_address_active;
reg          BA_address_conflict;
reg [1:0]    BA_address_reg;
reg [2:0]    burst_length;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
reg [2:0]    burst_length_cp;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
wire [2:0]   burst_cnt_max;
reg [1:0]    CAS_COUNT;
wire [1:0]   cas_count_value;
reg [2:0]    cas_latency;
reg [8:0]    column_address_reg;
reg [8:0]    column_address_reg1;
reg [8:0]    column_address_reg2;
reg [8:0]    column_address_reg3;
reg [8:0]    column_address_reg4;
reg [8:0]    column_address_reg5;
reg [8:0]    column_address_reg6;
wire [8:0]   column_address;
reg [2:0]    command_reg;
reg [9:0]    config_reg;
reg          CONFLICT;
wire         CONFLICT_value;
wire         ddr_rasb1;
wire         ddr_casb1;
wire         ddr_web1;
reg          ddr_rasb2;
reg          ddr_casb2;
reg          ddr_web2;
reg          ddr_rasb3;
reg          ddr_casb3;
reg          ddr_web3;
reg          ddr_rasb4;
reg          ddr_casb4;
reg          ddr_web4;
reg          ddr_rst_dqs_rasb4;
reg          ddr_rst_dqs_casb4;
reg          ddr_rst_dqs_web4;
reg          ddr_rasb5;
reg          ddr_casb5;
reg          ddr_web5;
wire [1:0]   ddr_ba1;
reg [1:0]    ddr_ba2;
reg [1:0]    ddr_ba3;
reg [1:0]    ddr_ba4;
reg [1:0]    ddr_ba5;
wire [12:0]  ddr_address1;
reg [12:0]   ddr_address2;
reg [12:0]   ddr_address3;
reg [12:0]   ddr_address4;
reg [12:0]   ddr_address5;
wire         DQS_enable_out;
wire         DQS_reset_out;
wire [2:0]   INIT_COUNT_value;
reg [2:0]    INIT_COUNT;
wire [7:0]   DLL_RST_COUNT_value;
reg [7:0]    DLL_RST_COUNT;
reg          INIT_DONE;
wire         init_done_value;
reg          init_memory;
wire         init_mem;
reg          initialize_memory;
reg          ld_mode;
wire         LOAD_RCD_read;
wire         LOAD_RCD_write;
/*
reg          LOAD_READ;
reg          LOAD_WRITE;
wire         LOAD_READ_value;
wire         LOAD_WRITE_value;
*/
wire [1:0]   MRD_COUNT_value;
wire [11:0]  max_ref_cnt;
reg [1:0]    MRD_COUNT;
reg          PRECHARGE_CMD;
wire [3:0]   ras_count_value;
reg [3:0]    RAS_COUNT;
wire         rdburst_chk;
reg          read_cmd;
wire         read_cmd_issued;
reg          read_cmd1;
reg          read_cmd2;
reg          read_cmd3;
reg          read_cmd4;
reg          read_cmd5;
reg          read_cmd6;
reg          read_cmd7;
reg          read_cmd8;
reg          read_rcd_end;
reg          read_cmd_reg;
wire         read_write_state;
reg [1:0]    RRD_COUNT;
reg [2:0]    RCDR_COUNT;
reg [1:0]    RCDW_COUNT;
wire [2:0]   rp_cnt_value;
wire [3:0]   RFC_COUNT_value;
wire [1:0]   RRD_COUNT_value;
wire [2:0]   RCDR_COUNT_value;
wire [1:0]   RCDW_COUNT_value;
wire [3:0]   RC_COUNT_value;
wire [2:0]   rdburst_end_cnt_value;
reg [2:0]    RDBURST_END_CNT;
reg          rdburst_end_1;
reg          rdburst_end_2;
reg          rdburst_end_3;
reg          rdburst_end_4;
reg          rdburst_end_5;
reg          rdburst_end_6;
reg          rdburst_end_7;
reg          rdburst_end_8;
wire         rdburst_end_r;
wire         read_enable_out_r;
wire         rdburst_end;
reg [2:0]    RP_COUNT;
reg [3:0]    RC_COUNT;
reg [3:0]    RFC_COUNT;
wire         read_enable_out;
wire [12:0]  ROW_ADDRESS;
reg [12:0]   row_address_reg;
reg [12:0]   row_address_active_reg;
reg          row_address_conflict;
reg          rst_dqs_div_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
reg          rst_dqs_div1_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
reg          rst_dqs_div_r2;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
reg          rst_dqs_div1_r2;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
reg          rst_dqs_div2_r;/* synthesis syn_preserve=1 */ /* synthesis syn_noprune=1 */
wire [2:0]   wrburst_end_cnt_value;
reg [2:0]    wrburst_end_cnt;
wire         wrburst_end;
reg          wrburst_end_1;
reg          wrburst_end_2;
reg          wrburst_end_3;
reg          wrburst_end_4;
reg          wrburst_end_5;
reg          wrburst_end_6;
reg          wrburst_end_7;
reg          wrburst_end_8;
reg          wrburst_end_9;
wire         wrburst_chk;
reg [1:0]    WR_COUNT;
wire [1:0]   WR_COUNT_value;
wire         write_enable_out;
reg          write_cmd;
reg          write_cmd_in;
reg          write_cmd2;
reg          write_cmd3;
reg          write_cmd4;
reg          write_cmd5;
reg          write_cmd1;
reg          write_cmd6;
reg          write_cmd7;
reg          write_cmd8;
wire         write_cmd_issued;
wire         GND;
reg [2:0]    dqs_div_cascount;
reg [2:0]    dqs_div_rdburstcount;
reg          DQS_enable1;
reg          DQS_enable2;
reg          DQS_enable3;
reg          DQS_enable4;
reg          DQS_reset1_clk0;
reg          DQS_reset2_clk0;
reg          DQS_reset3_clk0;
reg          DQS_reset4_clk0;
reg          DQS_enable_int;
reg          DQS_reset_int;
reg          rst180_r;
reg          rst0_r;
wire         clk180 ;
wire         GO_TO_ACTIVE_value;
reg          GO_TO_ACTIVE;


//  Input : CONFIG REGISTER FORMAT 
// config_register = {   EMR(Enable/Disable DLL),
//                       BMR (Normal operation/Normal Operation with Reset DLL),
//                       BMR/EMR,
//                       CAS_latency (3),
//                       Burst type ,
//                       Burst_length (3) }
//
// Input : COMMAND REGISTER FORMAT
//          000  - NOP
//          001  - Precharge 
//          010  - Auto Refresh
//          011  - SElf REfresh
//          100  - Write Request
//          101  - Load Mode Register
//          110  - Read request
//          111  - Burst terminate
//
// Input : Address format
//   row address = input address(19 downto 8)
//   column addrs = input address( 7 downto 0)
//

assign clk180 =~clk;
assign ddr_csb_cntrl = 1'b0; // dip3;
assign ddr_cke_cntrl = dip1;
assign ROW_ADDRESS = address_reg[21:9]; 
assign column_address = address_reg[8:0];
assign init = INIT_DONE;
assign GND = 1'b0;



assign ddr_rasb_cntrl = ddr_rasb4;
assign ddr_casb_cntrl = ddr_casb4;
assign ddr_web_cntrl = ddr_web4;


always @ (posedge clk180)
begin
  rst180_r <= rst180;
end

always @ (posedge clk)
begin
  rst0_r <= rst0;
end

//********************************************************************************************
// register input commands from the user
// 
//********************************************************************************************

  always @ (posedge clk180)
  begin
    if (rst180_r == 1'b1)
      begin
        config_reg <= 10'b0000000000;
        command_reg <= 3'b000;
        row_address_reg <= 13'b0000000000000;
        column_address_reg <= 9'b000000000;
        BA_address_reg <= 2'b00;
        address_reg <= 22'b0000000000000000000000;
      end
    else
      begin
        config_reg <= config_register;
        command_reg <= command_register;
        row_address_reg <= ROW_ADDRESS;
        column_address_reg <= column_address;
        BA_address_reg <= bank_address;
        address_reg <= address;
      end
  end
  
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
     burst_length <= 3'b000;
     cas_latency  <= 3'b000;
    end
  else
    begin
     burst_length <= config_reg[2:0];    
     cas_latency  <= config_reg[6:4];
    end
end

always @ (posedge clk)
begin
  if (rst0_r == 1'b1)
    begin
     burst_length_cp <= 3'b000;
    end
  else
    begin
     burst_length_cp <= config_reg[2:0];    
    end
end


always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
      PRECHARGE_CMD     <= 1'b0;
      read_cmd          <= 1'b0;
      Burst_terminate   <= 1'b0;
      initialize_memory <= 1'b0;
      ld_mode           <= 1'b0;
      write_cmd_in      <= 1'b0; 
      auto_refresh_cmd  <= 1'b0;
    end
  else
    begin
      case (command_reg)
       3'b001 : 
             PRECHARGE_CMD <= 1'b1;
       3'b010 :
             initialize_memory <= 1'b1;
       3'b011 :
             auto_refresh_cmd <= 1'b1;        
       3'b100 :
              write_cmd_in <= 1'b1;    
       3'b101 :
              ld_mode <= 1'b1;    
       3'b110 :
             read_cmd <= 1'b1;
       3'b111 :
             Burst_terminate <= 1'b1;
       default : begin
                   PRECHARGE_CMD     <= 1'b0;
                   read_cmd          <= 1'b0;
                   Burst_terminate   <= 1'b0;
                   initialize_memory <= 1'b0;
                   ld_mode           <= 1'b0;
                   write_cmd_in      <= 1'b0;
                   auto_refresh_cmd  <= 1'b0;
                 end  
      endcase
    end
end

//**************************************************************************
// write_cmd is used to determine when there is a new write request
//**************************************************************************

// register write_cmd until WRITE command needs to be issued

  always @ (posedge clk180)
  begin
     if (rst180_r == 1'b1)
      begin
       write_cmd1  <= 1'b0;
       write_cmd2  <= 1'b0;
       write_cmd3  <= 1'b0;
       write_cmd4  <= 1'b0;
       write_cmd5  <= 1'b0;
       write_cmd6  <= 1'b0;
       write_cmd7  <= 1'b0;
       write_cmd8  <= 1'b0;
       write_cmd   <= 1'b0;
      end
     else
      begin
       write_cmd1 <= write_cmd_in;
       write_cmd2 <= write_cmd1;
       write_cmd3 <= write_cmd2;
       write_cmd4 <= write_cmd3;
       write_cmd5 <= write_cmd4; 
       write_cmd6 <= write_cmd5; 
       write_cmd7 <= write_cmd6; 
       write_cmd8  <= write_cmd7;   
       write_cmd  <= write_cmd6;
      end               
  end
  
//************************************************************************
// register read cmd until READ command needs to be issued 
//************************************************************************

  always @ (posedge clk180)
  begin
     if (rst180_r == 1'b1)
      begin
       read_cmd1      <= 1'b0;
       read_cmd2      <= 1'b0;
       read_cmd3      <= 1'b0;
       read_cmd4      <= 1'b0;
       read_cmd5      <= 1'b0;
       read_cmd6      <= 1'b0;
       read_cmd7      <= 1'b0;
       read_cmd8      <= 1'b0;
       read_cmd_reg   <= 1'b0;
       read_rcd_end   <= 1'b0;
      end
     else
      begin
       read_cmd1       <= read_cmd;
       read_cmd2       <= read_cmd1;
       read_cmd3       <= read_cmd2;
       read_cmd4       <= read_cmd3;
       read_cmd5       <= read_cmd4; 
       read_cmd6       <= read_cmd5;  
       read_cmd7       <= read_cmd6;
       read_cmd_reg    <= read_cmd7; 
       read_cmd8       <= read_cmd7; 
       read_rcd_end    <= read_cmd8; // read_cmd6;
      end
  end
  
//********************************************************************************************                
// MRD Counter
// an executable command can be issued only after Tmrd(2 cycles) after a LMR command is issued
//********************************************************************************************
assign MRD_COUNT_value = (next_state == LOAD_MODE_REG) ? 2'b11 :
                          (MRD_COUNT != 2'b00) ? (MRD_COUNT - 1'b1) :
                          MRD_COUNT;
                          
//********************************************************************************************                
// RFC Counter
// an executable command can be issued only after Trfc(60 ns => 60/5 = 12 cycles) 
//                                after a AUTOREFRESH command is issued
//********************************************************************************************
assign RFC_COUNT_value = (next_state == AUTO_REFRESH) ? 4'b1111 :
                          (RFC_COUNT != 4'b0000) ? (RFC_COUNT - 1'b1) :
                          RFC_COUNT;                          

//********************************************************************************************                
// RP Counter
// an executable command can be issued only after Trp(20 ns for a -5 device => 4 cycles) 
//                        after a PRECHARGE command is issued
//********************************************************************************************
assign rp_cnt_value = (next_state == PRECHARGE) ? 3'b100 :
                       (RP_COUNT != 3'b000) ? (RP_COUNT - 1'b1) :
                       RP_COUNT;
                       
//********************************************************************************************                
// RRD Counter
// minimum interval between successive ACTIVE commands to different banks - Trrd
// 2 clock cycles
//********************************************************************************************
assign RRD_COUNT_value = (next_state == ACTIVE) ? 2'b10 :
                          (RRD_COUNT != 2'b00) ? (RRD_COUNT - 1'b1) :
                           2'b00;
                           
//*********************************************************************************************
// ACTIVE to READ/WRITE counter
// RCDr counter
// ACTIVE to READ delay - (-5) device requires 20 ns of delay => 4 clock cycles
//
// RCDW counter
// ACTIVE to WRITE delay - (-5) device requires 10 ns of delay => 2 clock cycles
//
//*********************************************************************************************

assign LOAD_RCD_read = (command_reg == 3'b110) ? 1'b1 : 1'b0;

assign LOAD_RCD_write = (command_reg == 3'b100) ? 1'b1 : 1'b0;

assign RCDR_COUNT_value =  (next_state == ACTIVE) ? 3'b011 :
                            (RCDR_COUNT != 3'b000) ? (RCDR_COUNT - 1'b1) :
                            RCDR_COUNT;

assign RCDW_COUNT_value = (next_state == ACTIVE) ? 2'b10 :
                           (RCDW_COUNT != 2'b00) ? (RCDW_COUNT - 1'b1) :
                           RCDW_COUNT;  
                           
//*********************************************************************************************
// ACTIVE to PRECHARGE counter
// RAS counter
// ACTIVE to PRECHARGE delay -
// the memory device require 40 ns (8 clock cycles)after issuing an ACTIVE command before issuing a
// PRECHARGE command
//
//*********************************************************************************************
assign ras_count_value = (next_state == ACTIVE) ? 4'b1000 :
                          (RAS_COUNT != 4'b0000) ? (RAS_COUNT - 1'b1) :
                          RAS_COUNT;

//**********************************************************************************************
// RC counter
// an ACTIVE command to a different row can be issued only after the previous row has been 
// precharged & after Trc is met 
// Trc = 60 ns = 12 clock cycles
//**********************************************************************************************
assign RC_COUNT_value = (next_state == ACTIVE) ? 4'b1100 :
                         (RC_COUNT != 4'b0000) ? (RC_COUNT - 1'b1) :
                         RC_COUNT;


//********************************************************************************************                
// WR Counter
// a PRECHARGE command can be applied only after 2 cycles after a WRITE command has finished 
// executing
//********************************************************************************************
assign WR_COUNT_value = (next_state == PRECHARGE_AFTER_WRITE) ? 2'b11 :
                         (WR_COUNT != 2'b00) ? (WR_COUNT - 1'b1) :
                         WR_COUNT;

//*********************************************************************************************
// Auto refresh counter - the design uses AUTO REFRESH
// 
// the DDR SDRAM requires AUTO REFRESH cycles at an average interval of 15.6 us
// the minimum frequency obtained from the system clock = 200 MHz/16 = 12.5 MHz = 80 ns
// Hence, a counter value of 195 should be used to obtain a 15.6 us clock for Auto Refresh
// Binary value of 195 = 11000011
//*********************************************************************************************
assign max_ref_cnt     = 12'b100000100000;    // AUTOREF_COUNT clocked with 200 MHz (15.4 us) // 110000110000 (15.6 us)

assign AUTOREF_value   = ((AUTOREF_COUNT == max_ref_cnt) && (next_state == IDLE)) ? 1'b1 : 1'b0;
                  
assign AUTOREF_CNT_val = (AUTOREF_COUNT == max_ref_cnt) ? 12'b000000000000 :
                          AUTOREF_COUNT + 1'b1;
                          
always @ (posedge clk180)   
begin
  if (rst180_r == 1'b1)
     AUTOREF_COUNT <= 12'b000000000000;
  else
     AUTOREF_COUNT <= AUTOREF_CNT_val;
end

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
      AUTO_REF_pulse_end <= 1'b0;
      AUTO_REF_detect1   <= 1'b0;
      AUTO_REF_detect    <= 1'b0; 
    end
  else
    begin
      AUTO_REF_detect1   <= AUTOREF_value;
      AUTO_REF_detect    <= AUTO_REF_detect1;
      AUTO_REF_pulse_end <= AUTO_REF_detect;
    end
end
    
assign auto_ref1 = ((AUTO_REF_detect == 1'b1) && (AUTO_REF_pulse_end != 1'b1)) ? 1'b1 : 1'b0;
           
assign AR_done_p = (RFC_COUNT == 4'b0001) ? 1'b1 : 1'b0;     
           
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
     auto_ref <= 1'b0;
    end
  else
    begin
     auto_ref <= auto_ref1;
    end
end


always @ (posedge clk)
begin
  if (rst0_r == 1'b1)
    begin
     ar_done  <= 1'b0;
    end
  else
    begin
     ar_done <= AR_done_p;
    end
end
//**********************************************************************************************
// Burst count value counter when there are cosecutive READs or WRITEs
// While doing consecutive READs or WRITEs, the Burst_count value determines when the next 
// READ or WRITE command should be issued. The burst length is determined while loading the 
// Load Mode Register
// burst_cnt_max shows the number of clock cycles for each burst
// burst_cnt_max = 1 for a burst length of 2, since it is ddr
//**********************************************************************************************
assign burst_cnt_max = (burst_length == 3'b001) ? 3'b001 :
                       (burst_length == 3'b010) ? 3'b010 :
                       (burst_length == 3'b011) ? 3'b100 :
                       (burst_length == 3'b111) ? 3'b100 :
                       3'b000;

//********************************************************************************************
// CAS latency counter
// CAS latencies of 2,3,4 can be set using Mode register bits M(6:4)
//
//      M6  M5  M4   CAS latency
//      0    1   0 -    2
//      0    1   1 -    3
//      1    0   0 -    4
//      others     -   reserved
// This design uses a CAS latency of 3 for a clock rate of 200 MHz
//
//********************************************************************************************
assign cas_count_value = (next_state == BURST_READ) ? 2'b10 :
                         (CAS_COUNT != 2'b00) ? (CAS_COUNT - 1'b1) :
                         CAS_COUNT;



assign rdburst_end_cnt_value = (CAS_COUNT == 2'b01) ? burst_cnt_max :
                               (RDBURST_END_CNT != 3'b000) ? (RDBURST_END_CNT - 1'b1) :
                               3'b000;



assign wrburst_end_cnt_value = ((next_state == FIRST_WRITE) || (next_state == BURST_WRITE)) ? burst_cnt_max :
                               (wrburst_end_cnt != 3'b000) ? (wrburst_end_cnt - 1'b1) :
                               3'b000;


assign wrburst_chk = ((next_state == BURST_WRITE) || (next_state == WRITE_WAIT)) ? 1'b1 : 1'b0;
                      
assign rdburst_chk  = ((next_state == BURST_READ) || (next_state == READ_WAIT)) ? 1'b1 : 1'b0;

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
      rdburst_end_1 <= 1'b0;
      rdburst_end_2 <= 1'b0;
      rdburst_end_3 <= 1'b0;
      rdburst_end_4 <= 1'b0;
      rdburst_end_5 <= 1'b0; 
    end
  else
    begin
      rdburst_end_2 <= rdburst_end_1;
      rdburst_end_3 <= rdburst_end_2;
      rdburst_end_4 <= rdburst_end_3;
      rdburst_end_5 <= rdburst_end_4;
      rdburst_end_6 <= rdburst_end_5;
      rdburst_end_7 <= rdburst_end_6;
      rdburst_end_8 <= rdburst_end_7;
      if (((CAS_COUNT[1] == 1'b1) || (CAS_COUNT[0] == 1'b1) || (burst_cnt_max == 3'b010)) && (burst_done == 1'b1))
        rdburst_end_1 <= 1'b1;
      else
        rdburst_end_1 <= 1'b0;
    end
end

assign rdburst_end_r = rdburst_end_3 || rdburst_end_4 || rdburst_end_5 || rdburst_end_6 || rdburst_end_7 || rdburst_end_8;
assign rdburst_end = rdburst_end_1 || rdburst_end_2 ;

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
     wrburst_end_1 <= 1'b0;
     wrburst_end_2 <= 1'b0;
     wrburst_end_3 <= 1'b0;
     wrburst_end_4 <= 1'b0;
     wrburst_end_5 <= 1'b0;
     wrburst_end_6 <= 1'b0;
     wrburst_end_7 <= 1'b0;
     wrburst_end_8 <= 1'b0;
     wrburst_end_9 <= 1'b0;
    end
  else
    begin
     wrburst_end_2  <= wrburst_end_1;  
     wrburst_end_3  <= wrburst_end_2;
     wrburst_end_4  <= wrburst_end_3; 
     wrburst_end_5  <= wrburst_end_4; 
     wrburst_end_6  <= wrburst_end_5; 
     wrburst_end_7  <= wrburst_end_6; 
     wrburst_end_8  <= wrburst_end_7; 
     if (((wrburst_end_cnt[1] == 1'b1) || (wrburst_end_cnt[0] == 1'b1) || (burst_cnt_max == 3'b010)) && (burst_done == 1'b1))
         wrburst_end_1 <= 1'b1;
     else 
         wrburst_end_1 <= 1'b0;
    end
end

assign wrburst_end = wrburst_end_1 || wrburst_end_2 || wrburst_end_3;

//**********************************************************************************************
// to generate the Data Strobe enable and reset signal
// The DQS signal needs to be generated center aligned with the data. 
// The controller generates the DQS enable signal when the state machine is in the FIRST_WRITE
// state,to take care of the write preamble
//**********************************************************************************************

assign DQS_enable_out = ((next_state == FIRST_WRITE) || (next_state == BURST_WRITE)  || (wrburst_end_cnt != 3'b000)) ? 1'b1 : 1'b0;

assign DQS_reset_out  = (next_state == FIRST_WRITE) ? 1'b1 : 1'b0;
                 
assign dqs_enable = DQS_enable4;
assign dqs_reset  = DQS_reset4_clk0;

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
      DQS_enable_int <= 1'b0;
      DQS_reset_int  <= 1'b0;
    end
  else
    begin
      DQS_enable_int <= DQS_enable_out;
      DQS_reset_int  <= DQS_reset_out;
    end
end

always @ (posedge clk)
begin
 if (rst0_r == 1'b1)
   begin
     DQS_enable1     <= 1'b0;
     DQS_enable2     <= 1'b0;
     DQS_enable3     <= 1'b0;
     DQS_enable4     <= 1'b0; 
     DQS_reset1_clk0 <= 1'b0;
     DQS_reset2_clk0 <= 1'b0;
     DQS_reset3_clk0 <= 1'b0;
     DQS_reset4_clk0 <= 1'b0;
   end
 else
   begin                 
     DQS_enable1     <= DQS_enable_int;
     DQS_enable2     <= DQS_enable1;
     DQS_enable3     <= DQS_enable2;
     DQS_enable4     <= DQS_enable3;
     DQS_reset1_clk0 <= DQS_reset_int;
     DQS_reset2_clk0 <= DQS_reset1_clk0;
     DQS_reset3_clk0 <= DQS_reset2_clk0;
     DQS_reset4_clk0 <= DQS_reset3_clk0;
   end
end
                       
//****************************************************************************************************
//Generating WRITE and READ enable signals
//*****************************************************************************************************

assign write_enable_out = ((wrburst_chk == 1'b1) || (wrburst_end_cnt != 3'b000)) ? 1'b1 : 1'b0; 

assign read_enable_out = ((CAS_COUNT != 2'b00) || (rdburst_chk == 1'b1)) ? 1'b1 : 1'b0; 
                    
assign read_enable_out_r = read_enable_out || rdburst_end_r;

// WRITE or READ command has already been issued

assign write_cmd_issued = write_cmd1 || write_cmd2 || write_cmd3 || write_cmd4 || write_cmd5 || write_cmd || write_enable_out;

assign read_cmd_issued  = read_cmd1 || read_cmd2 || read_cmd3 || read_cmd4 || read_cmd5 || read_cmd6 || read_cmd7 || read_cmd_reg || read_enable_out;
                        
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
   write_enable <= 1'b0;
  else
   write_enable <= write_enable_out;
end

assign cmd_ack = ack_reg;

FD ACK_REG_INST1 (  
                  .Q(ack_reg),
                  .D(ack_o),                             
                  .C(clk)
                  );                            

//assign ack_o = ((write_cmd6 == 1'b1) || (read_cmd7 == 1'b1)) ? 1'b1 : 1'b0;  // (write_cmd3 == 1'b1) || (read_cmd3 == 1'b1) 
assign ack_o = ((write_cmd2 == 1'b1) || (read_cmd3 == 1'b1)) ? 1'b1 : 1'b0;  // (write_cmd3 == 1'b1) || (read_cmd3 == 1'b1) 

assign read_write_state = ((next_state == FIRST_WRITE) || (next_state == BURST_WRITE) || (next_state == BURST_READ)) ? 1'b1 : 1'b0;

//*********************************************************************************************
//  to initialize memory
//*********************************************************************************************
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
     init_memory <= 1'b0;
     INIT_DONE   <= 1'b0;
    end
  else
    begin
     init_memory <= init_mem;
     INIT_DONE   <= init_done_value;
    end
end

assign init_mem = (initialize_memory == 1'b1) ? 1'b1 :
                  ((INIT_COUNT == 3'b111) && (MRD_COUNT == 2'b00)) ? 1'b0 :
                  init_memory;

// counter for Memory Initialization sequence

assign INIT_COUNT_value = (((next_state == PRECHARGE) || (next_state == LOAD_MODE_REG) || (next_state == AUTO_REFRESH)) && init_memory == 1'b1) ? (INIT_COUNT + 1'b1) :
                          INIT_COUNT;
                    
//assign init_done_value =  ((INIT_COUNT == 3'b111) && (MRD_COUNT == 2'b01) && (init_memory == 1'b1)) ? 1'b1 : 1'b0;                    
assign init_done_value =  ((INIT_COUNT == 3'b111) && (MRD_COUNT == 2'b00) && (DLL_RST_COUNT == 8'b0000_0001)) ? 1'b1 : 1'b0;                    

//Counter for DLL Reset complete
assign DLL_RST_COUNT_value = ((init_memory == 1'b1) && (INIT_COUNT == 3'b010)) ? 8'b1000_0111 :      //135
                             (DLL_RST_COUNT != 8'b0000_0001)? (DLL_RST_COUNT - 8'b0000_0001):
                              DLL_RST_COUNT;   

// LOAD MODE REGISTER when a WRITE command / READ command has been issued
/*
assign LOAD_WRITE_value =  ((write_cmd_in == 1'b1) && (RP_COUNT == 3'b000) && (write_cmd_issued == 1'b0)) ? 1'b1 : 1'b0;

assign LOAD_READ_value = ((read_cmd == 1'b1) && (RP_COUNT == 3'b000) && (read_cmd_issued == 1'b0)) ? 1'b1 : 1'b0;
*/
 //Signal to go directly to ACTIVE state instead of LMR state after Write/Read cmd_in from user 
//assign GO_TO_ACTIVE_value =( ((write_cmd4 == 1'b1) && (write_cmd5 != 1'b1))|| ((read_cmd4 == 1'b1) && (read_cmd5 != 1'b1)) )? 1'b1 : 1'b0;
assign GO_TO_ACTIVE_value =( ((write_cmd_in == 1'b1) && (write_cmd1 != 1'b1))|| ((read_cmd == 1'b1) && (read_cmd1 != 1'b1)) )? 1'b1 : 1'b0;

// To check if there is a bank conflict after an ACTIVE command has been issued for a particular bank

assign CONFLICT_value  = ((RRD_COUNT == 2'b01) && (BA_address_conflict == 1'b1)) ? 1'b1 : 1'b0;
    
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
   begin
/*     LOAD_WRITE <= 1'b0;
     LOAD_READ  <= 1'b0;*/
     CONFLICT   <= 1'b0;
     GO_TO_ACTIVE <= 1'b0;
   end
  else
   begin
/*     LOAD_WRITE <= LOAD_WRITE_value;
     LOAD_READ  <= LOAD_READ_value;*/
     CONFLICT   <= CONFLICT_value;
     GO_TO_ACTIVE <= GO_TO_ACTIVE_value;
   end
end

//**********************************************************************************************
// Register counter values
//**********************************************************************************************
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
     INIT_COUNT     <= 3'b000;
     DLL_RST_COUNT  <= 8'b0000_0000;
     RP_COUNT       <= 3'b000;
     MRD_COUNT      <= 2'b00;
     RFC_COUNT      <= 4'b0000;
     RAS_COUNT      <= 4'b0000;
     CAS_COUNT      <= 2'b00;
     RRD_COUNT      <= 2'b00;
     RCDR_COUNT     <= 3'b000;
     RCDW_COUNT     <= 2'b00;
     RC_COUNT       <= 4'b0000;
     RDBURST_END_CNT <= 3'b000;
     wrburst_end_cnt <= 3'b000;  
     WR_COUNT        <= 2'b00;
    end
  else
    begin
     INIT_COUNT     <= INIT_COUNT_value;
     DLL_RST_COUNT  <= DLL_RST_COUNT_value;
     RP_COUNT       <= rp_cnt_value;
     MRD_COUNT      <= MRD_COUNT_value;
     RFC_COUNT      <= RFC_COUNT_value;
     RAS_COUNT      <= ras_count_value;
     CAS_COUNT      <= cas_count_value;
     RRD_COUNT      <= RRD_COUNT_value;
     RCDR_COUNT     <= RCDR_COUNT_value;
     RCDW_COUNT     <= RCDW_COUNT_value;
     RC_COUNT       <= RC_COUNT_value;
     wrburst_end_cnt <= wrburst_end_cnt_value;
     RDBURST_END_CNT <= rdburst_end_cnt_value;
     WR_COUNT        <= WR_COUNT_value;
    end
end

//*********************************************************************************************
// to check current state for the address bus
//*********************************************************************************************
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
   begin
   current_state <= IDLE;
   next_state    <= IDLE;
   end
  else
   begin
   next_state    <= next_state1;
   current_state <= next_state;
   end
end

//*********************************************************************************************
// main state machine
//*********************************************************************************************

always @ (next_state or rst180_r or init_memory or RP_COUNT or INIT_COUNT or MRD_COUNT or RFC_COUNT or PRECHARGE_CMD or CONFLICT or
           auto_refresh_cmd or RCDW_COUNT or write_cmd or RCDR_COUNT or read_rcd_end or burst_length or wrburst_end or wrburst_end_cnt or 
          rdburst_end or CAS_COUNT  or WR_COUNT or GO_TO_ACTIVE or ld_mode or write_cmd3 or read_cmd5)

begin

    next_state1 <= next_state; 
    case (next_state)
        IDLE : begin
               if (init_memory == 1'b1)
                 begin
                  case (INIT_COUNT)
                   // this state is for NOP/Deselect
                   3'b000 :
                           next_state1 <= PRECHARGE;
                   3'b001 : 
                         begin
                         if (RP_COUNT == 3'b001)
                           next_state1 <= LOAD_MODE_REG;
                         else
                           next_state1 <= IDLE;
                         end
                   3'b010 : 
                         begin
                         if (MRD_COUNT == 2'b01)
                            next_state1 <= LOAD_MODE_REG;  // for reseting DLL in Base Mode register
                         else
                            next_state1 <= IDLE;
                         end
                   3'b011 :
                         begin
                         if (MRD_COUNT == 2'b01)
                            next_state1 <= PRECHARGE;
                         else
                            next_state1 <= IDLE;
                         end
                   3'b100 :
                         begin
                         if (RP_COUNT == 3'b001)  // wait for 4 clock cycles (Trp)
                            next_state1 <= AUTO_REFRESH;
                         else
                            next_state1 <= IDLE;
                         end
                   3'b101 :
                         begin
                         if (RFC_COUNT == 4'b0001)
                           next_state1 <= AUTO_REFRESH;
                         else 
                           next_state1 <= IDLE;
                         end
                   3'b110 :
                         begin
                         if (RFC_COUNT == 4'b0001)
                            next_state1 <= LOAD_MODE_REG;  // to deactivate the rst DLL bit in the LMR
                         else
                            next_state1 <= IDLE;
                         end
                   3'b111 :
                         begin
                         if (MRD_COUNT != 2'b00)
                           next_state1 <= IDLE;
                         end
                   default :
                        next_state1 <= IDLE;
                   endcase 
                 end
               else if (PRECHARGE_CMD == 1'b1)
                  next_state1 <= PRECHARGE;
               else if (ld_mode == 1'b1)
                  next_state1 <= LOAD_MODE_REG;
               else if (auto_refresh_cmd == 1'b1 && RP_COUNT == 3'b000)
                  next_state1 <= AUTO_REFRESH; // normal Refresh in the IDLE state 
                else if (GO_TO_ACTIVE == 1'b1 || CONFLICT == 1'b1)
                  next_state1 <= ACTIVE; 
               else if ((RCDW_COUNT == 2'b01) && (write_cmd3 == 1'b1))
                  next_state1 <= FIRST_WRITE;
               else if ((RCDR_COUNT == 3'b001) && (read_cmd5 == 1'b1))
                  next_state1 <= BURST_READ;
               else
                  next_state1 <= IDLE;
               end

         PRECHARGE :
              next_state1 <= IDLE;

         LOAD_MODE_REG :
              next_state1 <= IDLE;
      
         AUTO_REFRESH :
              next_state1 <= IDLE; 
         
         ACTIVE :
              next_state1 <= IDLE;
         
         FIRST_WRITE :
              begin
              // to meet the write preamble   
              if (burst_length == 3'b001)
                  next_state1 <= BURST_WRITE;
              else
                   next_state1 <= WRITE_WAIT;
              end
    
         WRITE_WAIT :
                     begin
                     case(wrburst_end)
                       1'b1 :
                         next_state1 <= PRECHARGE_AFTER_WRITE;  
                       1'b0 :
                         begin
                            if (wrburst_end_cnt == 3'b010)  // (wrburst_end_cnt == 3'b001)  
                                 next_state1 <= BURST_WRITE;
                            else
                                 next_state1 <= WRITE_WAIT;
                         end
                       default :
                         next_state1 <= WRITE_WAIT;
                     endcase
                     end
          BURST_WRITE :
                       begin
                       if (burst_length == 3'b001)
                          next_state1 <= BURST_WRITE;
                       else
                         next_state1 <= WRITE_WAIT;
                       end
          READ_AFTER_WRITE :
                     next_state1 <= BURST_READ;
          PRECHARGE_AFTER_WRITE :
                     next_state1 <= PRECHARGE_AFTER_WRITE_2;
          PRECHARGE_AFTER_WRITE_2 : 
          		begin  
                     if(WR_COUNT == 2'd0)   
                     next_state1 <= PRECHARGE;
                     else
                     next_state1 <= PRECHARGE_AFTER_WRITE_2;
                     end   
                    

          READ_WAIT : begin
                     case(rdburst_end)
                     1'b1 :
                      next_state1 <= PRECHARGE_AFTER_WRITE;
                     1'b0 :
                         begin
                         if (CAS_COUNT == 2'b10)
                             next_state1 <= BURST_READ;
                         else
                             next_state1 <= READ_WAIT;
                         end
                     default :
                         next_state1 <= READ_WAIT;
                    endcase
                     end
          BURST_READ :
                     begin
                     if (burst_length == 3'b001)
                          next_state1 <= BURST_READ;
                     else
                         next_state1 <= READ_WAIT;
                     end
          BURST_STOP :
                    next_state1 <= READ_WAIT;

    endcase
end

//************************************************************************************************
// address generation logic
//************************************************************************************************

assign address_config[12:7] = (INIT_COUNT == 3'b010) ? 6'b000000 :
                              (INIT_COUNT == 3'b011) ? 6'b000010 :
                              (current_state == PRECHARGE) ? 6'b001000 :
                              6'b000000;

assign address_config[6:4] =  (INIT_COUNT == 3'b010) ? 3'b000 :
                              (current_state == LOAD_MODE_REG) ? cas_latency :
                              3'b000;
                              
assign address_config[3] = 1'b0; // design uses sequential burst

assign address_config[2:0] =  (INIT_COUNT == 3'b010) ? 3'b000 :
                              (current_state == LOAD_MODE_REG) ? burst_length :
                              3'b000;


assign ddr_address1 = (current_state == LOAD_MODE_REG || current_state == PRECHARGE) ? address_config :
                      (current_state == ACTIVE) ? row_address_reg :
                      ((current_state == BURST_WRITE) || (current_state == FIRST_WRITE) || (current_state == BURST_READ)) ? {4'b0000, column_address_reg} : 
                      13'b0000000000000;
                      
assign ddr_ba1 =  ((current_state == LOAD_MODE_REG) && (INIT_COUNT == 3'b010)) ? 2'b01 :
                  ((current_state == ACTIVE) || (current_state == FIRST_WRITE) || (current_state == BURST_WRITE) || (current_state == BURST_READ)) ? BA_address_reg :
                  2'b00;

//********************************************************************************************************
//  register row address
//********************************************************************************************************
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
     row_address_active_reg <= 13'b0000000000000;
  else
   begin
     if (next_state == ACTIVE)
         row_address_active_reg <= row_address_reg;
     else
         row_address_active_reg <= row_address_active_reg;
   end
end

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
     row_address_conflict <= 1'b0;
  else
   begin
     if (row_address_reg != row_address_active_reg)
         row_address_conflict <= 1'b1;
     else
         row_address_conflict <= 1'b0;
   end
end

//********************************************************************************************************
//  register bank address
//********************************************************************************************************

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
      BA_address_active <= 2'b00;
  else
    begin
      if (next_state == ACTIVE)
          BA_address_active <= BA_address_reg;
      else
          BA_address_active <= BA_address_active;
    end
end

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
      BA_address_conflict <= 1'b0;
  else
    begin
      if (BA_address_reg != BA_address_active)
          BA_address_conflict <= 1'b1;
      else
          BA_address_conflict <= 1'b0;
    end
end

//********************************************************************************************************
//  register column address
//********************************************************************************************************
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
     column_address_reg1 <= 9'b000000000;
     column_address_reg2 <= 9'b000000000;
     column_address_reg3 <= 9'b000000000;
     column_address_reg4 <= 9'b000000000;
     column_address_reg5 <= 9'b000000000;
     column_address_reg6 <= 9'b000000000;
    end
  else
    begin
     column_address_reg1 <= column_address_reg;
     column_address_reg2 <= column_address_reg1;
     column_address_reg3 <= column_address_reg2;
     column_address_reg4 <= column_address_reg3;
     column_address_reg5 <= column_address_reg4;
     column_address_reg6 <= column_address_reg5;
    end
end



//**************************************************************************************************
//Pipeline stages for ddr_address and ddr_ba
//**************************************************************************************************

always @ (posedge clk180)
begin
if (rst180_r == 1'b1)
  begin
   ddr_address2  <= 13'b0000000000000;
   ddr_address3  <= 13'b0000000000000;
   ddr_ba2       <= 2'b00;
   ddr_ba3       <= 2'b00;
  end
else
  begin
    ddr_address2 <= ddr_address1;
    ddr_address3 <= ddr_address2;
    ddr_ba2      <= ddr_ba1;
    ddr_ba3      <= ddr_ba2;
  end
end

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
      ddr_address4  <= 13'b0000000000000;
      ddr_address5  <= 13'b0000000000000;
      ddr_address_cntrl   <= 13'b0000000000000;
    end
  else
    begin
      ddr_address4  <= ddr_address3;
      ddr_address5  <= ddr_address4;
      ddr_address_cntrl   <= ddr_address4;
    end
end

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
     ddr_ba4 <= 2'b00;
     ddr_ba5 <= 2'b00;
     ddr_ba_cntrl  <= 2'b00;
    end
  else
    begin
     ddr_ba4 <= ddr_ba3;
     ddr_ba5 <= ddr_ba4;
     ddr_ba_cntrl  <= ddr_ba4;
    end
end

//************************************************************************************************
// control signals to the Memory
//************************************************************************************************

assign ddr_rasb1 = ((current_state == ACTIVE) || (current_state == PRECHARGE) || (current_state == AUTO_REFRESH) || (current_state == LOAD_MODE_REG)) ? 1'b0 : 1'b1;

assign ddr_casb1 = ((current_state == BURST_READ) || (current_state == BURST_WRITE) || (current_state == FIRST_WRITE) || 
                   (current_state == AUTO_REFRESH) || (current_state == LOAD_MODE_REG)) ? 1'b0 : 1'b1;

assign ddr_web1  = ((current_state == BURST_WRITE) || (current_state == FIRST_WRITE) || (current_state == BURST_STOP) || 
                   (current_state == PRECHARGE) || (current_state == LOAD_MODE_REG)) ? 1'b0 : 1'b1;


//*************************************************************************************************
// register CONTROL SIGNALS outputs
//**************************************************************************************************
always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
      ddr_rasb3 <= 1'b1;
      ddr_casb3 <= 1'b1;
      ddr_web3  <= 1'b1;
      ddr_rasb2 <= 1'b1;
      ddr_casb2 <= 1'b1;
      ddr_web2  <= 1'b1;
    end
  else
    begin
      ddr_rasb2    <= ddr_rasb1;
      ddr_casb2    <= ddr_casb1;
      ddr_web2     <= ddr_web1;
      ddr_rasb3     <= ddr_rasb2;
      ddr_casb3    <= ddr_casb2;
      ddr_web3     <= ddr_web2;
    end
end

always @ (posedge clk180)
begin
  if (rst180_r == 1'b1)
    begin
      ddr_rasb4  <= 1'b1;
      ddr_casb4  <= 1'b1;
      ddr_web4   <= 1'b1; 
      ddr_rst_dqs_rasb4  <= 1'b1;
      ddr_rst_dqs_casb4  <= 1'b1;
      ddr_rst_dqs_web4   <= 1'b1; 
      ddr_rasb5 <= 1'b1;
      ddr_casb5 <= 1'b1;
      ddr_web5  <= 1'b1;
    end
  else
    begin
      ddr_rasb4  <= ddr_rasb3;
      ddr_casb4  <= ddr_casb3;
      ddr_web4   <= ddr_web3;
      ddr_rasb5   <= ddr_rasb4;
      ddr_casb5   <= ddr_casb4;
      ddr_web5    <= ddr_web4;
    

	if(cas_latency == 3'b010) /// --CL2
		begin
		ddr_rst_dqs_rasb4  <= ddr_rasb2;
   		ddr_rst_dqs_casb4  <= ddr_casb2;
   		ddr_rst_dqs_web4   <= ddr_web2; 
		end
	 else if(cas_latency == 3'b011) // --CL3
	 begin
		ddr_rst_dqs_rasb4  <= ddr_rasb3;
   		ddr_rst_dqs_casb4  <= ddr_casb3;
   		ddr_rst_dqs_web4   <= ddr_web3; 
	 end
	else
	begin
	  ddr_rst_dqs_rasb4  <= ddr_rst_dqs_rasb4;
   	ddr_rst_dqs_casb4  <= ddr_rst_dqs_casb4;
   	ddr_rst_dqs_web4   <= ddr_rst_dqs_web4; 
	end
	end

end
  
always @ (posedge clk)
begin
    if (rst0_r == 1'b1)
        dqs_div_cascount <= 3'b000;
    else
      begin
        if ((ddr_rst_dqs_rasb4 == 1'b1) && (ddr_rst_dqs_casb4 == 1'b0) && (ddr_rst_dqs_web4 == 1'b1) && (burst_length_cp == 3'b010) )
             dqs_div_cascount <= 3'b010;
        else if ((ddr_rst_dqs_rasb4 == 1'b1) && (ddr_rst_dqs_casb4 == 1'b0) && (ddr_rst_dqs_web4 == 1'b1) && (burst_length_cp == 3'b011) )
             dqs_div_cascount <= 3'b100;

        else
          begin
             if (dqs_div_cascount != 3'b000)
                 dqs_div_cascount <= dqs_div_cascount - 1'b1;
             else
                 dqs_div_cascount <= dqs_div_cascount;
          end
      end
end

always @ (posedge clk)
begin
    if (rst0_r == 1'b1)
        dqs_div_rdburstcount <= 3'b000;
    else
      begin
        if (dqs_div_cascount == 3'b001  && burst_length_cp == 3'b010)
            dqs_div_rdburstcount <= 3'b010; 
        else if (dqs_div_cascount == 3'b011  && burst_length_cp == 3'b011)
            dqs_div_rdburstcount <= 3'b100; 

        else 
          begin
            if (dqs_div_rdburstcount != 3'b000)
               dqs_div_rdburstcount <= dqs_div_rdburstcount - 1'b1;
            else
               dqs_div_rdburstcount <= dqs_div_rdburstcount;
          end
      end
end

always @ (posedge clk)
begin
    if (rst0_r == 1'b1)
    begin
        rst_dqs_div_r <= 1'b0;
        rst_dqs_div1_r <= 1'b0;
        rst_dqs_div2_r <= 1'b0;

    end 
    else
      begin
        if (dqs_div_cascount == 3'b001  && burst_length_cp == 3'b010)begin
            rst_dqs_div_r <= 1'b1;
            rst_dqs_div1_r <= 1'b1;
            rst_dqs_div2_r <= 1'b1;
        end
        else if (dqs_div_cascount == 3'b011  && burst_length_cp == 3'b011)begin
            rst_dqs_div_r <= 1'b1;
            rst_dqs_div1_r <= 1'b1;
            rst_dqs_div2_r <= 1'b1;
        end
        else if (dqs_div_rdburstcount == 3'b001 && dqs_div_cascount == 3'b000)begin
            rst_dqs_div_r <= 1'b0;
            rst_dqs_div1_r <= 1'b0;
            rst_dqs_div2_r <= 1'b0;
        end
        else begin
            rst_dqs_div_r <= rst_dqs_div_r;
            rst_dqs_div1_r <= rst_dqs_div1_r;
            rst_dqs_div2_r <= rst_dqs_div2_r;
        end 

      end
end

always @ (posedge clk)
begin
    rst_dqs_div_r2 <= rst_dqs_div_r;
    rst_dqs_div1_r2 <= rst_dqs_div1_r;
end 

assign rst_dqs_div_int =   rst_dqs_div_r2;
assign rst_dqs_div_int1 =   rst_dqs_div1_r2;  


FD  rst_calib0  (
                 .Q(rst_calib),     
                 .D(rst_dqs_div2_r),                         
                 .C(clk)
                 );         
         

                         

 
                            
                            
                                 
                                                         
                            
endmodule   
