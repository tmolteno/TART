`timescale 1ns/100ps
/*
 *
 * Time-multiplexed block of correlator-blocks.
 * 
 * NOTE:
 *  + typically several of these would be attached to a common set of antenna
 *    and a system bus;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as multi-
 *    port SRAM's are used;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM resources;
 * 
 */

module correlator_block
  #( parameter ACCUM = 32,
     // Pairs of antennas to correlate, for each block.
     parameter PAIRS0 = 120'hb1a191817161b0a090807060,
     parameter PAIRS1 = 120'hb3a393837363b2a292827262,
     parameter PAIRS2 = 120'hb5a595857565b4a494847464,
     parameter PAIRS3 = 120'hb1a191817161b0a090807060, // TODO:
     parameter MSB   = ACCUM - 1,
     parameter DELAY = 3)
   (
    input          clk_x,       // correlator clock
    input          rst,

    // Wishbone-like bus interface for reading visibilities.
    input          clk_i,       // bus clock
    input          cyc_i,
    input          stb_i,
    input          we_i,        // writes are ignored
    input          bst_i,       // Bulk Sequential Transfer?
    output         ack_o,
    input [6:0]    adr_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    // Real and imaginary components from the antennas.
    input          sw,          // switch banks
    input          en,          // data is valid
    input [23:0]   re,
    input [23:0]   im,

    output reg     overflow_cos = 0,
    output reg     overflow_sin = 0
    );


   //-------------------------------------------------------------------------
   //  Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   wire [MSB:0]    dat_0, dat_1, dat_2, dat_3;

   // Address decoders.
   wire            sel0 = adr_i[6:5] == 2'b00;
   wire            sel1 = adr_i[6:5] == 2'b01;
   wire            sel2 = adr_i[6:5] == 2'b10;
   wire            sel3 = adr_i[6:5] == 2'b11;

   // 4:1 multiplexer for returning requested visibilities.
   assign ack_o = cyc_i & stb_i & (sel0 & ack_0 | sel1 & ack_1 | sel2 & ack_2 | sel3 & ack_3);
   assign dat_o = adr_i[6] ? (adr_i[5] ? dat_3 : dat_2) : (adr_i[5] ? dat_1 : dat_0) ;


   //-------------------------------------------------------------------------
   //  Correlator instances.
   //-------------------------------------------------------------------------
   correlator
     #(  .ACCUM(ACCUM),
         .PAIRS(PAIRS0),
         .DELAY(DELAY)
         ) CORRELATOR0
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stb_i && sel0),
         .we_i (we_i),
         .bst_i(bst_i),
         .ack_o(ack_0),
         .adr_i(adr_i[4:0]),
         .dat_i(dat_i),
         .dat_o(dat_0),

         .sw(sw),
         .en(en),
         .re(re),
         .im(im),

         .overflow_cos(oc_0),
         .overflow_sin(os_0)
         );

   correlator
     #(  .ACCUM(ACCUM),
         .PAIRS(PAIRS1),
         .DELAY(DELAY)
         ) CORRELATOR1
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stb_i && sel1),
         .we_i (we_i),
         .bst_i(bst_i),
         .ack_o(ack_1),
         .adr_i(adr_i[4:0]),
         .dat_i(dat_i),
         .dat_o(dat_1),

         .sw(sw),
         .en(en),
         .re(re),
         .im(im),

         .overflow_cos(oc_1),
         .overflow_sin(os_1)
         );

   correlator
     #(  .ACCUM(ACCUM),
         .PAIRS(PAIRS2),
         .DELAY(DELAY)
         ) CORRELATOR2
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stb_i && sel2),
         .we_i (we_i),
         .bst_i(bst_i),
         .ack_o(ack_2),
         .adr_i(adr_i[4:0]),
         .dat_i(dat_i),
         .dat_o(dat_2),

         .sw(sw),
         .en(en),
         .re(re),
         .im(im),

         .overflow_cos(oc_2),
         .overflow_sin(os_2)
         );

   correlator
     #(  .ACCUM(ACCUM),
         .PAIRS(PAIRS3),
         .DELAY(DELAY)
         ) CORRELATOR3
       ( .clk_x(clk_x),
         .rst(rst),

         .clk_i(clk_i),
         .cyc_i(cyc_i),
         .stb_i(stb_i && sel3),
         .we_i (we_i),
         .bst_i(bst_i),
         .ack_o(ack_3),
         .adr_i(adr_i[4:0]),
         .dat_i(dat_i),
         .dat_o(dat_3),

         .sw(sw),
         .en(en),
         .re(re),
         .im(im),

         .overflow_cos(oc_3),
         .overflow_sin(os_3)
         );

   
endmodule // correlator_block
