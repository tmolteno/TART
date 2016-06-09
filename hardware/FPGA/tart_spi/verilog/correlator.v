`timescale 1ns/1ps
/*
 *
 * Time-multiplexed correlator block.
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

module correlator
  #( parameter ACCUM = 32,
     parameter PAIR0 = 8'h60,   // Pairs of antennas to correlate
     parameter PAIR1 = 8'h70,
     parameter PAIR2 = 8'h80,
     parameter PAIR3 = 8'h90,
     parameter PAIR4 = 8'ha0,
     parameter PAIR5 = 8'hb0,
     parameter PAIR6 = 8'h61,
     parameter PAIR7 = 8'h71,
     parameter PAIR8 = 8'h81,
     parameter PAIR9 = 8'h91,
     parameter PAIRA = 8'ha1,
     parameter PAIRB = 8'hb1,
     parameter MSB   = ACCUM - 1,
     parameter DELAY = 3)
   (
    input          clk_x,       // correlator clock
    input          rst,

    // Wishbone-like bus interface for reading visibilities.
    input          clk_i,       // bus clock
    input          cyc_i,
    input          stb_i,
    input          we_i, // writes are ignored
    output         ack_o,
    input [4:0]    adr_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    // Real and imaginary components from the antennas.
    input          sw,          // switch banks
    input          en,          // data is valid
    input [11:0]   re,
    input [11:0]   im,

    output reg     overflow_cos = 0,
    output reg     overflow_sin = 0,
    );


   //-------------------------------------------------------------------------
   //  Distributed RAM's for the accumulators.
   //-------------------------------------------------------------------------
   // For Xilinx FPGA's, this should be two `RAM32M's.
   reg [MSB:0]     cosram[0:31];
   reg [MSB:0]     sinram[0:31];

   wire [MSB:0]    dcos = cosram[{bank, x_rd_adr}];
   wire [MSB:0]    dsin = sinram[{bank, x_rd_adr}];

   reg             bank = 0, clear = 0;
   reg [3:0]       x_rd_adr = 0, x_wt_adr = 0, x_wr_adr = 0;
   wire            wrap_x_rd_adr = x_rd_adr == 11;
   wire [3:0]      next_x_rd_adr = wrap_x_rd_adr ? 0 : x_rd_adr + 1 ;

   // Pipelined correlator requires cycles for:
   //   { read, MAC, write } .
   always @(posedge clk_x)
     if (rst) begin
        x_rd_adr <= 0;
        x_wt_adr <= 0;
        x_wr_adr <= 0;
     end
     else if (en) begin
        x_rd_adr <= next_x_rd_adr;
        x_wt_adr <= x_rd_adr;
        x_wr_adr <= x_wt_adr;
     end

   // Banks are switched at the next address-wrap event.
   always @(posedge clk_x)
     if (rst) begin : RAM_RESET_LOGIC
        swap  <= 0;
        bank  <= 0;
        clear <= 1;
     end
     else if (wrap_x_rd_adr && (sw || swap)) begin // swap banks
        swap  <= 0;
        bank  <= ~bank;
        clear <= 1;
     end
     else if (sw && !swap) begin // swap banks @next wrap
        swap  <= 1;
     end
     else if (wrap_x_rd_adr && clear) begin // finished restarting counters
        clear <= 0;
     end

   // Read and write RAM contents for the correlator.
   always @(posedge clk_x) begin : RAM_READ_WRITE
     if (!rst && en) begin
        dcos <= cosram[{bank, x_rd_adr}];
        dsin <= sinram[{bank, x_rd_adr}];
     end
     if (!rst && valid) begin
        cosram[x_wr_adr] <= qcos;
        sinram[x_wr_adr] <= qsin;
     end
   end


   //-------------------------------------------------------------------------
   //  Bus interface logic.
   //-------------------------------------------------------------------------

   /*
   always @(posedge clk)
     if (rst) begin
     end
     else if (cyc_i && stb_i)
    */


   //-------------------------------------------------------------------------
   //  Select pairs of antenna to correlate.
   //-------------------------------------------------------------------------
   wire [7:0] pairs_index = pairs[x_rd_adr];
   wire [3:0] a_index = pairs_index[3:0];
   wire [3:0] b_index = pairs_index[7:4];
   reg [7:0]  pairs[0:11];
   reg        go = 0, ar, br, bi;

   initial begin : PAIRS_ROM
      pairs[0] = PAIR0;
      pairs[1] = PAIR1;
      pairs[2] = PAIR2;
      pairs[3] = PAIR3;
      pairs[4] = PAIR4;
      pairs[5] = PAIR5;
      pairs[6] = PAIR6;
      pairs[7] = PAIR7;
      pairs[8] = PAIR8;
      pairs[9] = PAIR9;
      pairs[a] = PAIRA;
      pairs[b] = PAIRB;
   end // block: PAIRS_ROM

   // Add a cycle of latency to wait for the RAM read.
   always @(posedge clk_x) begin
      go <= en;
      ar <= re[a_index];
      br <= re[b_index];
      bi <= im[b_index];
   end


   //-------------------------------------------------------------------------
   //  Time-multiplexed correlator.
   //-------------------------------------------------------------------------
   correlate_cos_sin
     #(  .ACCUM(ACCUM), .DELAY(DELAY) ) CORR_COS_SIN0
       ( .clk(clk_x),
         .rst(rst),

         // Antenna enables and inputs:
         .en(go),
         .ar(ar),
         .br(br),
         .bi(bi),

         // Accumulator inputs and outputs:
         .dcos(dcos),
         .dsin(dsin),
         .valid(valid),
         .qcos(qcos),
         .qsin(qsin),

         // Overflow flags:
         .oc(oc),
         .os(os)
         );


endmodule // correlator
