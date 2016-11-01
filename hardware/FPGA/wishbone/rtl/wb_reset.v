`timescale 1ns/100ps
/*
 * 
 * Simple reset circuit, connected via a Wishbone-like interconnect.
 * 
 * NOTE:
 *  + supports both classic and pipelined transfers;
 * 
 * TODO:
 * 
 */

module wb_reset
  #(parameter WIDTH = 8,
    parameter MSB   = WIDTH-1,
    parameter RTIME = 4,
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input              clk_i,
    input              rst_i,
    input              cyc_i,
    input              stb_i,
    input              we_i,
    output reg         ack_o = 0,
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o = 0,

    input              reset_ni, // from optional external source
    (* ASYNC_REG = "TRUE" *)
    output reg         reset_o = 0
    );

    (* ASYNC_REG = "TRUE" *)
   reg                 reset  = 0;
   reg [RTIME-1:0]     resets = 0;
   wire                reset_w = cyc_i && stb_i && we_i && !ack_o && dat_i[0];

   //-------------------------------------------------------------------------
   //  Logic for the Wishbone-like interconnect.
   always @(posedge clk_i)
     if (rst_i) ack_o <= #DELAY 0;
     else       ack_o <= #DELAY cyc_i && stb_i && !ack_o;

   always @(posedge clk_i)
     dat_o <= #DELAY cyc_i && stb_i && !we_i ? {{MSB{1'b0}},reset_o} : dat_o;

   //-------------------------------------------------------------------------
   //  Reset logic.
   always @(posedge clk_i)
     if (rst_i) reset <= #DELAY 0;
     else       reset <= #DELAY reset ? ~reset_ni : reset_w;
//      else       reset <= #DELAY reset || reset_o ? 1'b0 : (!reset_ni || reset_w);

   always @(posedge clk_i) begin
      reset_o <= #DELAY |resets;
      resets  <= #DELAY {resets[RTIME-2:0], reset};
   end


endmodule // wb_reset
