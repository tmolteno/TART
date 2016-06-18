`timescale 1ns/100ps
/*
 * 
 * Dual-port SRAM with both 32-bit, and 8-bit ports.
 * 
 * NOTE:
 *  + doesn't support "classic" Wishbone cycles, but does support pipelined
 *    transactions;
 *  + supports burst transfers -- these are indicated by `bst_i` being
 *    asserted at the beginning of a transaction, and deasserted one cycle
 *    before the final (pipelined) `ack_o`;
 * 
 * TODO:
 *  + this is not currently synthesisable, so use RAMBxx primitives;
 * 
 */

module wb_sram_dual_port
  #(parameter SBITS = 10,
    parameter SIZE  = 1 << SBITS,
    parameter ASB   = SBITS-1,
    parameter BSB   = SBITS+1,
    parameter DELAY = 3)
   (
    input             rst_i,

    // 32-bit Wishbone-like bus interface:
    input             a_clk_i, // bus clock A
    input             a_cyc_i,
    input             a_stb_i,
    input             a_we_i,
    input             a_bst_i, // Bulk Sequential Transfer?
    output reg        a_ack_o = 0,
    input [ASB:0]     a_adr_i,
    input [31:0]      a_dat_i,
    output reg [31:0] a_dat_o,

    // 8-bit Wishbone-like bus interface:
    input             b_clk_i, // bus clock B
    input             b_cyc_i,
    input             b_stb_i,
    input             b_we_i,
    input             b_bst_i, // Bulk Sequential Transfer?
    output reg        b_ack_o = 0,
    input [BSB:0]     b_adr_i,
    input [7:0]       b_dat_i,
    output reg [7:0]  b_dat_o
    );


   //-------------------------------------------------------------------------
   //  Dual-port SRAM's, with different widths for each port.
   //-------------------------------------------------------------------------
   reg [7:0]           sram0 [0:SIZE-1];
   reg [7:0]           sram1 [0:SIZE-1];
   reg [7:0]           sram2 [0:SIZE-1];
   reg [7:0]           sram3 [0:SIZE-1];

   integer             i;
   initial begin : SRAM_INIT
      for (i = 0; i < SIZE; i = i+1) begin
        sram0[i] <= 0;
        sram1[i] <= 0;
        sram2[i] <= 0;
        sram3[i] <= 0;
      end
   end


   //-------------------------------------------------------------------------
   //  Port A Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   always @(posedge a_clk_i)
     if (rst_i) a_ack_o <= #DELAY 0;
     else       a_ack_o <= #DELAY a_cyc_i && a_stb_i;

   // SRAM reads & writes.
   always @(posedge a_clk_i)
     if (!rst_i && a_cyc_i && a_stb_i) begin
        a_dat_o <= #DELAY {sram3[a_adr_i], sram2[a_adr_i], sram1[a_adr_i], sram0[a_adr_i]};
        if (a_we_i)
          {sram3[a_adr_i], sram2[a_adr_i], sram1[a_adr_i], sram0[a_adr_i]} <= #DELAY a_dat_i;
     end


   //-------------------------------------------------------------------------
   //  Port B Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   wire [ASB:0] b_adr = b_adr_i[BSB:2];

   always @(posedge b_clk_i)
     if (rst_i) b_ack_o <= #DELAY 0;
     else       b_ack_o <= #DELAY b_cyc_i && b_stb_i;

   // SRAM reads & writes.
   always @(posedge b_clk_i)
     if (!rst_i && b_cyc_i && b_stb_i) begin
        case (b_adr_i[1:0])
          0: b_dat_o <= #DELAY sram0[b_adr];
          1: b_dat_o <= #DELAY sram1[b_adr];
          2: b_dat_o <= #DELAY sram2[b_adr];
          3: b_dat_o <= #DELAY sram3[b_adr];
        endcase // case (adr_i[1:0])
        if (b_we_i)
          case (b_adr_i[1:0])
            0: sram0[b_adr] <= #DELAY b_dat_i;
            1: sram1[b_adr] <= #DELAY b_dat_i;
            2: sram2[b_adr] <= #DELAY b_dat_i;
            3: sram3[b_adr] <= #DELAY b_dat_i;
          endcase // case (adr_i[1:0])
     end


endmodule // wb_sram_dual_port
