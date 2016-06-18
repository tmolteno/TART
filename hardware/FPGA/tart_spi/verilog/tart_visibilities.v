`timescale 1ns/100ps
/*
 * 
 * Manages the correlators, including its flags and settings.
 * Transfers visibilities from the correlators to a SRAM after each bank-swap.
 * 
 * NOTE:
 *  + 8-bit bus to the SPI interface, and 32-bit bus to the correlators;
 * 
 * TODO:
 *  + redirection ROM;
 * 
 */

module tart_visibilities
  #(parameter BLOCK = 32,
    parameter MSB   = BLOCK-1,
    parameter COUNT = 576,     // TODO: correlators and averages
    parameter CBITS = 10,
    parameter CSB   = CBITS-1,
    parameter MRATE = 12,       // time-multiplexing rate
    parameter MBITS = 4,
    parameter MSKIP = (1 << MBITS) - MRATE + 1,
    parameter RSB   = MBITS-1,
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
    input [CSB+2:0]    adr_i, // upper address-space for registers
    input [7:0]        byt_i,
    output [7:0]       byt_o,

    // Wishbone-like (master) bus interface that connects to the correlators:
    output             cyc_o,
    output             stb_o,
    output             we_o, // writes only work for system registers
    output             bst_o, // Bulk Sequential Transfer?
    input              ack_i,
    output [CSB:0]     adr_o, // upper address-space for registers
    input [MSB:0]      dat_i,
    output [MSB:0]     dat_o,

    // Status flags for the correlators and visibilities.
    input              switching, // inicates that banks have switched
    output reg [MSB:0] blocksize = 0, // block size - 1
    output             available // asserted when a window is accessed
    );

   //-------------------------------------------------------------------------
   //  The time-multiplexing ratio determines how many values are stored
   //  within each correlator. E.g., for `MRATE = 12`, there are 12 real and
   //  12 complex values within each of the correlator's SRAM's.
   parameter BSIZE = MRATE*2-1;
   parameter BBITS = MBITS+1;
   parameter BSB   = BBITS-1;


   //-------------------------------------------------------------------------
   //  Dual-port SRAM's for all visibilities from the same "window."
   //-------------------------------------------------------------------------
   reg [7:0]           sram0 [0:COUNT-1];
   reg [7:0]           sram1 [0:COUNT-1];
   reg [7:0]           sram2 [0:COUNT-1];
   reg [7:0]           sram3 [0:COUNT-1];


   //-------------------------------------------------------------------------
   //  System registers.
   //  NOTE: Addressed by `{3'b111, reg#}`.
   //  TODO:
   //-------------------------------------------------------------------------
   //  Register#:
   //    00  --  status register;
   //    01  --  logarithm of the bit-width of the visibilities counter;
   //
   wire [7:0]          status = {available, 2'b00, count_log[4:0]};
   reg [7:0]           count_log = 0;

   // TODO: Should be computed from `count_log`, and synchronised across
   //   domains.
   always @(posedge clk_i)
     if (rst_i) begin
        count_log <= #DELAY 0;
        blocksize <= #DELAY 0;
     end
     else if (cyc_i && stb_i && we_i && adr_i == 12'he01) begin
        count_log <= #DELAY byt_i;
        blocksize <= #DELAY (1 << byt_i[4:0]) - 1;
     end


   //-------------------------------------------------------------------------
   //  Cores that prefetch and store visibility data, to be transferred off-
   //  board via SPI.
   //-------------------------------------------------------------------------
   wire [MSB:0] p_val, p_dat;
   wire [CSB:0] p_adr, adr_w;
   wire         p_cyc, p_stb, p_we, p_bst, p_ack;

   //  Swap the LSB with the real/complex bank-select signal, so that real +
   //  complex pairs are read out together.
   assign adr_o = {adr_w[CSB:BBITS], adr_w[0], adr_w[BSB:1]};

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
       .a_adr_o(adr_w),
       .a_dat_i(dat_i),
       .a_dat_o(dat_o),

       .b_cyc_o(p_cyc),         // interface between prefetcher & SRAM
       .b_stb_o(p_stb),
       .b_we_o (p_we),
       .b_bst_o(p_bst),
       .b_ack_i(p_ack),
       .b_adr_o(p_adr),
       .b_dat_i(p_val),
       .b_dat_o(p_dat)
       );

   //  Buffers the visibility data until ready via the TART SPI interface.
   wb_sram_dual_port #( .SBITS(CBITS) ) SRAM0
     ( .rst_i(rst_i),

       .a_clk_i(clk_i),    // this port is filled by the prefetch unit
       .a_cyc_i(p_cyc),    // whenever the correlators perform a bank-
       .a_stb_i(p_stb),    // switch
       .a_we_i (p_we),
       .a_bst_i(p_bst),
       .a_ack_o(p_ack),
       .a_adr_i(p_adr),
       .a_dat_i(p_dat),
       .a_dat_o(p_val),

       .b_clk_i(clk_i),    // this port is driven by the TART SPI unit
       .b_cyc_i(cyc_i),
       .b_stb_i(stb_i),
       .b_we_i (we_i),
       .b_bst_i(bst_i),
       .b_ack_o(ack_o),
       .b_adr_i(adr_i),
       .b_dat_i(byt_i),
       .b_dat_o(byt_o)
       );


endmodule // tart_visibilities
