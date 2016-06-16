`timescale 1ns/100ps
/*
 * 
 * Dual-port SRAM with both 32-bit, and 8-bit ports.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

// Bus transaction states.
`define BUS_IDLE  0
`define BUS_WAIT  1
`define BUS_READ  2
`define BUS_WRITE 4

module wb_sram_dual_port
  #(parameter SIZE  = 1024,
    parameter SBITS = 10,
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
    output reg [7:0]  b_dat_o,
    );


   //-------------------------------------------------------------------------
   //  Dual-port SRAM's for all visibilities from the same "window."
   //-------------------------------------------------------------------------
   reg [7:0]           sram0 [0:COUNT-1];
   reg [7:0]           sram1 [0:COUNT-1];
   reg [7:0]           sram2 [0:COUNT-1];
   reg [7:0]           sram3 [0:COUNT-1];


   //-------------------------------------------------------------------------
   //  Port A bus interface.
   //-------------------------------------------------------------------------

   //-------------------------------------------------------------------------
   //  Address and data update logic for SRAM writes.
   always @(posedge a_clk_i)
     if (rst_i || switching && !cyc_o)
       vis_adr <= #DELAY 0;
     else if (cyc_o && stb_o && !we_o && ack_i)
       vis_adr <= #DELAY vis_adr + 1;
     else
       vis_adr <= #DELAY vis_adr;

   always @(posedge clk_i)
     if (cyc_o && stb_o && !we_o && ack_i) begin
        sram0[vis_adr] <= #DELAY dat_i[7:0];
        sram1[vis_adr] <= #DELAY dat_i[15:8];
        sram2[vis_adr] <= #DELAY dat_i[23:16];
        sram3[vis_adr] <= #DELAY dat_i[31:24];
     end


   //-------------------------------------------------------------------------
   //  Port B bus interface.
   //-------------------------------------------------------------------------
   wire [CSB-1:0] sram_adr = adr_i[CBITS+1:2];
   reg            bst_r = 0;

   always @(posedge clk_i)
     if (rst_i) ack_o <= #DELAY 0;
     else       ack_o <= #DELAY cyc_i && stb_i && (bst_r || !ack_o);

   always @(posedge clk_i)
     if (rst_i) bst_r <= #DELAY 0;
     else       bst_r <= #DELAY cyc_i && stb_i && bst_i;

   // SRAM reads & writes.
   always @(posedge clk_i)
     if (!rst_i && cyc_i && stb_i && !we_i)
       case (adr_i[1:0])
         0: byt_o <= #DELAY sram0[sram_adr];
         1: byt_o <= #DELAY sram1[sram_adr];
         2: byt_o <= #DELAY sram2[sram_adr];
         3: byt_o <= #DELAY sram3[sram_adr];
       endcase // case (adr_i[1:0])
     else if (!rst_i && cyc_i && stb_i && we_i)
       case (adr_i[1:0])
         0: sram0[sram_adr] <= #DELAY byt_i;
         1: sram1[sram_adr] <= #DELAY byt_i;
         2: sram2[sram_adr] <= #DELAY byt_i;
         3: sram3[sram_adr] <= #DELAY byt_i;
       endcase // case (adr_i[1:0])

endmodule // wb_sram_dual_port
