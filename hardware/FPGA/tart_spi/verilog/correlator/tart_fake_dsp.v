`timescale 1ns/100ps
/*
 * Module      : verilog/tart_dsp.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * 
 * This file is part of TART.
 * 
 * TART is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * TART is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser Public License along with
 * TART.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * 
 * Description:
 * Fake top-level of the TART DSP units, to find the "all-zero" bug.
 * 
 * NOTE:
 *  + hardwired for a 24 antenna setup;
 *  + the correlator is set to use 12:1 time-multiplexing, so `clk_x` must
 *    be 12x the frequency of the sampling clock;
 *  + the number of bits in a block's counter corresponds to the maximum size
 *    of the accumulator, because the visibilities are monotone increasing;
 * 
 * TODO:
 *  + when `bst_i` deasserts, deassert `stb[i]` the next cycle? Currently, the
 *    "tails" of a transaction are one cycle too long;
 *  + compute the exponent of the count-size;
 *  + status registers, and correctly handle overflows;
 * 
 */

`include "tartcfg.v"

module tart_fake_dsp
   #(parameter NREAD = `READ_COUNT, // Number of visibilities to read back
     parameter DELAY = 3)
   (
    input          clk_x,
    input          rst_i,

    // 100 MHz, 8-bit, Wishbone-like interconnect for reading visibilities.
    input          aq_clk_i, // bus clock
    input          aq_cyc_i,
    input          aq_stb_i,
    input          aq_we_i,
    input          aq_bst_i,
    output         aq_ack_o,
    input [XSB:0]  aq_blk_i,
    input [BSB:0]  aq_dat_i,
    output [BSB:0] aq_dat_o,

    // Debugging signals.
    (* KEEP = "TRUE" *)
    output         stuck_o,
    (* KEEP = "TRUE" *)
    output         limp_o,

    // The real component of the signal from the antennas.
    input          aq_enable, // data acquisition is active
    output         switching, // NOTE: bus domain
    input [MSB:0]  blocksize, // block size - 1
    input [NSB:0]  antenna, // the raw antenna signal

    output         newblock,
    output [MSB:0] checksum, // TODO:
    output         streamed
    );

   //-------------------------------------------------------------------------
   //
   //  TART's system-wide, Wishbone-like interconnect and peripherals.
   //
   //-------------------------------------------------------------------------
   //  Visibilities/correlator settings.
   parameter AXNUM = `NUM_ANTENNA;// Number of antennae
   parameter NSB   = AXNUM-1;
   parameter ACCUM = `ACCUM_BITS; // Bit-width of the accumulators
   parameter COUNT = ACCUM;       // Maximum #bits of the block-size
   parameter MSB   = COUNT-1;     // Data transfer MSB
   parameter WIDTH = ACCUM+ACCUM; // Visibilites width, `{sin, cos}`
   parameter WSB   = WIDTH-1;
   parameter TRATE = `TMUX_RATE;  // Time-multiplexing rate
   parameter TBITS = 4;
   parameter TSB   = TBITS-1;

   //  Wishbone settings.
   parameter BBITS = `WBBUS_BITS; // Bit-width of the SoC Wishbone bus
   parameter BSB   = BBITS-1;     // Bus data MSB
   parameter ABITS = `WBADR_BITS; // Correlator bus address bit-width
   parameter ASB   = ABITS-1;     // Address MSB
   parameter XBITS = `BANK_BITS; // Bit-width of the block-counter
   parameter XSB   = XBITS-1;     // MSB of the block-counter


   assign stuck_o   = s_cyc;
   assign limp_o    = switch;

   assign switching = switch;
   assign streamed  = switch;
   assign newblock  = newblocks[7];
   assign checksum  = {ACCUM{1'bx}};

   reg [7:0]       newblocks = 0;
   always @(posedge aq_clk_i)
     newblocks <= #DELAY {newblocks[6:0], switch};


   //-------------------------------------------------------------------------
   //     Synchronise signals from the wishbone clock domain.
   //-------------------------------------------------------------------------
   reg [MSB:0]  block_x  = 0;
   reg          enable_x = 0;

   (* ASYNC_REG = "TRUE" *) reg [MSB:0]  block_s  = 0;
   (* ASYNC_REG = "TRUE" *) reg          enable_s = 0;

   always @(posedge clk_x) begin
      block_s  <= #DELAY blocksize;
      enable_s <= #DELAY aq_enable;
   end

   always @(posedge clk_x) begin
      block_x  <= #DELAY block_s;
      enable_x <= #DELAY enable_s;
   end

   //-------------------------------------------------------------------------
   //  Synchronise the bank-switching signal to the bus domain.
   //  NOTE: Keeps `sw_b` asserted until acknowledged.
   //-------------------------------------------------------------------------
   reg sw_x = 0, sw_d = 0;      // acquisition domain
   (* ASYNC_REG = "TRUE" *) reg sw_b = 1'b0; // bus domain
   reg switch = 1'b0;

   always @(posedge clk_x)
     if (rst_i || strobe) sw_x <= #DELAY 1'b0;
     else if (sw)         sw_x <= #DELAY 1'b1;
     else                 sw_x <= #DELAY sw_x;

   always @(posedge clk_x)
     if (rst_i) sw_d <= #DELAY 0;
     else       sw_d <= #DELAY sw_x && strobe;

   always @(posedge aq_clk_i or posedge sw_d)
     if (sw_d)                 sw_b <= #DELAY 1'b1;
     else if (rst_i || switch) sw_b <= #DELAY 1'b0;

   always @(posedge aq_clk_i)
     if (rst_i) switch <= #DELAY 1'b0;
     else       switch <= #DELAY sw_b && !switch;


   //-------------------------------------------------------------------------
   //  
   //  Hardware-correlator control logic.
   //  
   //-------------------------------------------------------------------------
   //  Fill a block with visibilities, and then switch banks.
   wire [COUNT:0]      next_count = wrap_count ? {(COUNT+1){1'b0}} : count+1;
   wire [MSB:0]        count_max  = block_x;
   wire                wrap_count = count == count_max;
   reg [MSB:0]         count = {COUNT{1'b0}};
   reg [3:0]           delays = 4'h0;
   wire                valid;

   //-------------------------------------------------------------------------
   //  Count the number of correlations, computed so far within this block,
   //  and switch blocks once `count_max` is reached.
   always @(posedge clk_x)
     if (rst_i) begin
        sw    <= #DELAY 1'b0;
        count <= #DELAY {COUNT{1'b0}};
     end
     else if (valid) begin
        // signal an upcoming bank-swap?
        sw    <= #DELAY delays[3] && wrap_count;
        count <= #DELAY strobe ? next_count[MSB:0] : count;
     end
     else begin
        sw    <= #DELAY 1'b0;
        count <= #DELAY count;
     end

   always @(posedge clk_x)
     delays <= #DELAY {delays[2:0], !rst_i && strobe};


   //-------------------------------------------------------------------------
   //  Banks are switched at the next address-wrap event.
   //-------------------------------------------------------------------------
   wire [XBITS:0] next_block = block + 1'b1;
   reg [XSB:0]    block = {XBITS{1'b0}};
   reg            sw = 1'b0, swap = 1'b0, clear = 1'b1;
   wire           w_swp = wrap_x_rd_adr && (sw || swap);
   wire           w_inc = wrap_x_wr_adr && clear;

   always @(posedge clk_x)
     if (rst_i)
       swap <= #DELAY 1'b0;
     else if (w_swp) // swap banks
       swap <= #DELAY 1'b0;
     else if (sw && !swap) // swap banks @next wrap
       swap <= #DELAY 1'b1;

   //  Increment the block-counter two cycles later, so that the correct data
   //  is stored within the SRAM's.
   always @(posedge clk_x)
     if (rst_i)      block <= #DELAY {XBITS{1'b0}};
     else if (w_inc) block <= #DELAY next_block;
   
   //  Clear a bank when correlators are enabled, or during the first set of
   //  writes after a bank-switch.
   always @(posedge clk_x)
     if (rst_i || !write)
        clear <= #DELAY 1'b1;
     else if (w_swp)
        clear <= #DELAY 1'b1;
     else if (wrap_x_rd_adr && clear) // finished restarting counters
        clear <= #DELAY 1'b0;

   //-------------------------------------------------------------------------
   //  Correlator memory pointers.
   //-------------------------------------------------------------------------
   wire [TSB:0] x_rd_adr, x_wr_adr;
   wire         wrap_x_rd_adr, wrap_x_wr_adr;

   rmw_address_unit
     #(  .ABITS(TBITS), .UPPER(TRATE-1)
         ) RMW0
       ( .clk_i    (clk_x),
         .rst_i    (rst_i),
         .ce_i     (valid),
         .rd_adr_o (x_rd_adr),
         .rd_wrap_o(wrap_x_rd_adr),
         .wr_adr_o (x_wr_adr),
         .wr_wrap_o(wrap_x_wr_adr)
         );

   //-------------------------------------------------------------------------
   //  Hilbert transform to recover imaginaries.
   //-------------------------------------------------------------------------
   wire         hilb_en = enable_x;
   wire         strobe;
   wire [NSB:0] re, im;

   fake_hilbert #( .WIDTH(AXNUM) ) HILB0
     (  .clk(clk_x),
        .rst(rst_i),
        .en(hilb_en),
        .d(antenna),
        .valid(valid),
        .strobe(strobe), // `antenna` data is valid
        .re(re),
        .im(im)
        );

   //-------------------------------------------------------------------------
   //  Add a single hardware correlator, for testing.
   //-------------------------------------------------------------------------
   wire         write;
   wire [WSB:0] vis_data;

`include "../include/tart_pairs.v"

   correlator_SDP
     #(  .ACCUM(ACCUM),
         .SUMHI(0),
         .TBITS(TBITS),
         .PAIRS(PAIRS00_00),
         .DELAY(DELAY)
         ) CORRELATOR0
       ( .clk_x(clk_x),
         .rst  (rst_i),

         .sw(clear),
         .en(valid),
         .re(re),
         .im(im),
         .rd(x_rd_adr),
         .wr(x_wr_adr),

         .vld(write),
         .vis(vis_data)
         );

   
   //-------------------------------------------------------------------------
   //  
   //  Wishbone to SRAM interface.
   //  
   //-------------------------------------------------------------------------
   wire sram_ce, sram_we;
   wire [7:0] sram_adr;
   wire [WSB:0] sram_to_wb, wb_to_sram;
   wire         s_cyc, s_ack;
   reg [7:0]    s_adr = 8'b0;
   wire [WSB:0] s_dat;

   always @(posedge aq_clk_i)
     if (rst_i)
       s_adr <= #DELAY 8'h00;
     else if (s_cyc && s_ack)
       s_adr <= #DELAY s_adr + 1;

   //-------------------------------------------------------------------------
   //  Break the 48-bit words into 8-bit chunks.
   wb_chunk #(.WIDTH(WIDTH), .CHUNK(BBITS)) WBCHUNK0
     ( .rst_i(rst_i),

       .clk_i(aq_clk_i),
       .cyc_i(aq_cyc_i),
       .stb_i(aq_stb_i),
       .we_i (aq_we_i),
       .ack_o(aq_ack_o),
       .dat_i(aq_dat_i),
       .dat_o(aq_dat_o),

       .fetch_o(s_cyc),
       .ready_i(s_ack),
       .value_i(s_dat)
       );

   //-------------------------------------------------------------------------
   //  Fetch the 48-bit `{sin, cos}` visibilities from the buffer.
   wb_sram_port #( .WIDTH(WIDTH), .ABITS(8) ) WBSRAM0
     ( .clk_i(aq_clk_i),
       .rst_i(rst_i),
       .cyc_i(s_cyc),
       .stb_i(s_cyc),
       .we_i (1'b0),
       .bst_i(1'b0),
       .ack_o(s_ack),
       .wat_o(),
       .err_o(),
       .adr_i(s_adr),
       .dat_i({WIDTH{1'b0}}),
       .dat_o(s_dat),

       .sram_ce_o (sram_ce),
       .sram_we_o (sram_we),
       .sram_adr_o(sram_adr),
       .sram_dat_i(sram_to_wb),
       .sram_dat_o(wb_to_sram)
       );

   //-------------------------------------------------------------------------
   //  Explicit block SRAM instantiation, to allow manual placement.
   //  This SRAM buffers blocks of visibilities, waiting to be read back via
   //  the SPI interface.
   //-------------------------------------------------------------------------
`ifdef __USE_GENERIC_SRAM
   reg [WSB:0]  visram [0:255];
   reg [WSB:0]  visdat = {WIDTH{1'b0}};
   wire [7:0]   visadr = {block, x_wr_adr};

   assign sram_to_wb = visdat;

   always @(posedge clk_x)
     if (write) visram[visadr] <= #DELAY vis_data;

   always @(posedge aq_clk_i)
     if (sram_ce) visdat <= #DELAY visram[sram_adr];

`else
   wire [71:0]  ramb8_wr_data, ramb8_rd_data;
   wire [7:0]   ramb8_wr_addr = {block, x_wr_adr};

   assign ramb8_wr_data = {{(72-WIDTH){1'b0}}, vis_data};
   assign sram_to_wb    = ramb8_rd_data[WSB:0];

   RAMB8X36_SDP #(.DELAY(DELAY)) VISRAM [1:0]
     ( .WCLK (clk_x),           // store the visibilities
       .WE   (write),
       .WADDR(ramb8_wr_addr),
       .DI   (ramb8_wr_data),
       .RCLK (aq_clk_i),        // read back the visibilities
//        .CE   (sram_ce),
       .CE   (1'b1),
       .RADDR(sram_adr),
       .DO   (ramb8_rd_data)
       );
`endif // !`ifdef __USE_GENERIC_SRAM


   //-------------------------------------------------------------------------
   //     Debugging stuff.
   //-------------------------------------------------------------------------
   always @aq_bst_i
     if (aq_bst_i)
       $display("%12t: WB burst-mode transfer attempted.", $time);


endmodule // tart_fake_dsp
