`timescale 1ns/100ps
/*
 * 
 * Single-port SRAM with parameterised size and bit-width.
 * 
 * NOTE:
 *  + doesn't support "classic" Wishbone cycles, but does support pipelined
 *    transactions;
 *  + supports burst transfers -- these are indicated by `bst_i` being
 *    asserted at the beginning of a transaction, and deasserted one cycle
 *    before the final (pipelined) `ack_o`;
 *  + OBSOLETE (better to use 'wb_sram_port', Pat @ 08/11/2016);
 * 
 * TODO:
 * 
 */

module wb_sram
  #(parameter WIDTH = 32,
    parameter MSB   = WIDTH-1,
    parameter SBITS = 10,
    parameter SIZE  = 1 << SBITS,
    parameter ASB   = SBITS-1,
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input              clk_i,
    input              rst_i,
    input              cyc_i,
    input              stb_i,
    input              we_i,
    input              bst_i, // Bulk Sequential Transfer?
    output reg         ack_o = 0,
    input [ASB:0]      adr_i,
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o
    );


   //-------------------------------------------------------------------------
   //  SRAM.
   //-------------------------------------------------------------------------
   reg [MSB:0]         sram [0:SIZE-1];

   integer             i;
   initial begin : SRAM_INIT
      for (i = 0; i < SIZE; i = i+1)
        sram[i] <= 0;
   end

   //-------------------------------------------------------------------------
   //  Wishbone bus interface.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i) ack_o <= #DELAY 0;
     else       ack_o <= #DELAY cyc_i && stb_i;

   // SRAM reads & writes.
   always @(posedge clk_i)
     if (!rst_i && cyc_i && stb_i) begin
        dat_o <= #DELAY sram[adr_i];
        if (we_i)
          sram[adr_i] <= #DELAY dat_i;
     end


endmodule // wb_sram
