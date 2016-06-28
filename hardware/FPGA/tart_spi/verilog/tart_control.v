`timescale 1ns/100ps
/*
 * 
 * TART's control registers module, connected via a Wishbone-like
 * interconnect.
 * 
 * Has system registers for:
 *   00  --  status register;
 *   01  --  extra status-flags;
 *   10  --  miscellaneous register;
 *   11  --  reset register;
 * 
 * NOTE:
 *  + supports both classic and pipelined transfers;
 * 
 * TODO:
 * 
 */

`include "tartcfg.v"

module tart_control
  #(parameter WIDTH = 8,
    parameter MSB   = WIDTH-1,
    parameter RTIME = 4,
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input          clk_i,
    input          rst_i,
    input          cyc_i,
    input          stb_i,
    input          we_i,
    output         ack_o,
    input [1:0]    adr_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    input          status_i,
    input          overflow_i,
    input          underrun_i,
    input          reset_ni,
    output         reset_o
    );

   wire [7:0]      extraflags = {6'b110011, overflow_i, underrun_i};
   reg [MSB:0]     c_dat, misc = 8'h3e;
   reg             c_ack = 0;                 

   assign ack_o = c_ack;
   assign dat_o = c_dat;


   always @(posedge clk_i)
     if (rst_i) c_ack <= #DELAY 0;
     else       c_ack <= #DELAY cyc_i && stb_i && !c_ack;

   always @(posedge clk_i)
     if (cyc_i && stb_i && !we_i)
       case (adr_i)
         0: c_dat <= #DELAY status_i;
         1: c_dat <= #DELAY extraflags;
         2: c_dat <= #DELAY misc;
         3: c_dat <= #DELAY 8'h5c;
       endcase // case (adr_i)

   always @(posedge clk_i)
     if (rst_i)
       misc <= #DELAY 8'h3e;
     else if (cyc_i && stb_i && we_i && adr_i == 2'b10)
       misc <= #DELAY dat_i;


   //-------------------------------------------------------------------------
   //     RESET HANDLER
   //-------------------------------------------------------------------------
   wire            r_stb = stb_i && adr_i == 2'b11;

   wb_reset #( .WIDTH(WIDTH), .RTIME(RTIME) ) WB_RESET0
     ( .clk_i(clk_i),
       .rst_i(rst_i),
       .cyc_i(cyc_i),
       .stb_i(r_stb),
       .we_i (we_i),
       .ack_o(),
       .dat_i(dat_i),
       .dat_o(),

       .reset_ni(reset_ni),
       .reset_o (reset_o)
       );


endmodule // tart_control
