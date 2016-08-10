`timescale 1ns/100ps
/*
 * 
 * Manages fetching visibilities data from the correlators -- transferring
 * visibilities from the correlators to a SRAM after each bank-swap.
 * 
 * NOTE:
 *  + 8-bit bus to the SPI interface, and 32-bit bus to the correlators;
 * 
 * TODO:
 *  + redirection ROM?
 * 
 */

`include "tartcfg.v"

module tart_visibilities
  #(parameter BLOCK = 32,
    parameter MSB   = BLOCK-1,
    parameter COUNT = 576,     // TODO: correlators and averages
`ifdef __USE_SDP_DSRAM
    parameter ABITS = 14,
`else
    parameter ABITS = 10,
`endif
    parameter ASB   = ABITS-1,
    parameter CBITS = 10,
    parameter CSB   = CBITS-1,
    parameter TRATE = 12,       // time-multiplexing rate
    parameter TBITS = 4,
    parameter MSKIP = (1 << TBITS) - TRATE + 1,
    parameter RSB   = TBITS-1,
    parameter DELAY = 3)
   (
    input              clk_i, // bus clock
    input              rst_i,

    // Wishbone-like (slave) bus interface that connects to `tart_spi`:
    input              cyc_i,
    input              stb_i,
    input              we_i, // writes only work for system registers
    input              bst_i, // Bulk Sequential Transfer?
    output             ack_o,
    output             wat_o,
    input [ASB+2:0]    adr_i, // upper address-space for registers
    input [7:0]        byt_i,
    output [7:0]       byt_o,

    // Wishbone-like (master) bus interface that connects to the correlators:
    output             cyc_o,
    output             stb_o,
    output             we_o, // writes only work for system registers
    output             bst_o, // Bulk Sequential Transfer?
    input              ack_i,
    input              wat_i,
    output [ASB:0]     adr_o, // upper address-space for registers
    input [MSB:0]      dat_i,
    output [MSB:0]     dat_o,

    // Status flags for the correlators and visibilities.
    input              switching, // inicates that banks have switched
    output             available, // asserted when a window is accessed
    output reg [MSB:0] checksum = 0
    );

   //-------------------------------------------------------------------------
   //  The time-multiplexing ratio determines how many values are stored
   //  within each correlator. E.g., for `TRATE = 12`, there are 12 real and
   //  12 complex values within each of the correlator's SRAM's.
   parameter BSIZE = TRATE*2-1;
   parameter BBITS = TBITS+1;
   parameter BSB   = BBITS-1;


   //-------------------------------------------------------------------------
   //  Dual-port SRAM's for all visibilities from the same "window."
   //-------------------------------------------------------------------------
   reg [7:0]           sram0 [0:COUNT-1];
   reg [7:0]           sram1 [0:COUNT-1];
   reg [7:0]           sram2 [0:COUNT-1];
   reg [7:0]           sram3 [0:COUNT-1];


   //-------------------------------------------------------------------------
   //  Compute checksums, to be used to verify off-chip transfers.
   //-------------------------------------------------------------------------
   //  TODO: Just use XOR?
   always @(posedge clk_i)
     if (switching)
       checksum <= #DELAY 0;
     else if (cyc_o && ack_i)
       checksum <= #DELAY checksum + dat_i;


   //-------------------------------------------------------------------------
   //  Cores that prefetch and store visibility data, to be transferred off-
   //  board via SPI.
   //-------------------------------------------------------------------------
   wire [MSB:0] p_val, p_dat;
   wire [CSB:0] p_adr, adr_w;
   wire         p_cyc, p_stb, p_we, p_bst, p_ack, p_wat;

   //  Swap the LSB with the real/complex bank-select signal, so that real +
   //  complex pairs are read out together.
`ifdef __USE_SDP_DSRAM
   assign adr_o = {adr_i[ASB+2:CBITS+2], adr_w};
`else
   assign adr_o = {adr_w[CSB:BBITS], adr_w[0], adr_w[BSB:1]};
`endif
   assign wat_o = 1'b0;

   //  Prefetches data from the various correlators after each bank-switch,
   //  and then sends it on to a block SRAM.
   wb_prefetch #( .WIDTH(BLOCK), .SBITS(CBITS), .COUNT(COUNT-1),
                  .BSIZE(BSIZE), .BBITS(BBITS) ) PREFETCH0
     ( .rst_i(rst_i),
       .clk_i(clk_i),

       .begin_i(switching),
       .ready_o(available),

       .a_cyc_o(cyc_o),         // prefetch interface (from the correlators)
       .a_stb_o(stb_o),
       .a_we_o (we_o),
       .a_bst_o(bst_o),
       .a_ack_i(ack_i),
       .a_wat_i(wat_i),
       .a_adr_o(adr_w),
       .a_dat_i(dat_i),
       .a_dat_o(dat_o),

       .b_cyc_o(p_cyc),         // interface between prefetcher & SRAM
       .b_stb_o(p_stb),
       .b_we_o (p_we),
       .b_bst_o(p_bst),
       .b_ack_i(p_ack),
       .b_wat_i(p_wat),
       .b_adr_o(p_adr),
       .b_dat_i(p_val),
       .b_dat_o(p_dat)
       );

   //  Buffers the visibility data until ready via the TART SPI interface.
   wire [31:0]  w_dat_o;
   assign p_val = w_dat_o[MSB:0];

   wb_sram_dual_port #( .SBITS(CBITS) ) SRAM0
     ( .rst_i(rst_i),

       .a_clk_i(clk_i),    // this port is filled by the prefetch unit
       .a_cyc_i(p_cyc),    // whenever the correlators perform a bank-
       .a_stb_i(p_stb),    // switch
       .a_we_i (p_we),
       .a_bst_i(p_bst),
       .a_ack_o(p_ack),
       .a_wat_o(p_wat),
       .a_adr_i(p_adr),
       .a_dat_i({{32-BLOCK{1'b0}}, p_dat}),
       .a_dat_o(w_dat_o),

       .b_clk_i(clk_i),    // this port is driven by the TART SPI unit
       .b_cyc_i(cyc_i),
       .b_stb_i(stb_i),
       .b_we_i (we_i),
       .b_bst_i(bst_i),
       .b_ack_o(ack_o),
       .b_wat_o(),
       .b_adr_i(adr_i[CSB+2:0]),
       .b_dat_i(byt_i),
       .b_dat_o(byt_o)
       );


endmodule // tart_visibilities
