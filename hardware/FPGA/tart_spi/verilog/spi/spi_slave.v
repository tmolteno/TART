`timescale 1ns/100ps
/*
 * Module      : verilog/spi/spi_slave.v
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
 * This SPI slave module wraps `spi_layer` to give address support to the
 * (pipelined, Wishbone-like) bus interface.
 * 
 * NOTE:
 *  + XST synthesis achieves about 250 MHz on a Spartan VI;
 *  + currently, the SCK frequency can be no more than slightly higher than
 *    that of the (Wishbone, SPEC B4) bus clock, or else data isn't available
 *    early enough, resulting in FIFO underruns -- but this could be solved
 *    using an additional prefetch (OR, an additional padding byte), though
 *    this hasn't been implemented;
 * 
 * Changelog:
 *  + 18/06/2016  --  initial file;
 *  + 27/10/2016  --  now OBSOLETE, as it has been replaced by `spi_slave_wb`
 *                    which supports Wishbone SPEC B4;
 * 
 * TODO:
 *  + the design is still preliminary (as of 24/06/2016);
 *  + reduce the fanout of `rst_i`;
 * 
 */

// TODO: Not very modular, as the rest of this module is not TART-specific.
`include "tartcfg.v"

`define SPI_IDLE 4'h0
`define SPI_ADDR 4'h1
`define SPI_BUSY 4'h2
`define SPI_PUSH 4'h4
`define SPI_PULL 4'h8

module spi_slave
  #( parameter WIDTH = 8,       // TODO: currently must be `8`!
     parameter MSB   = WIDTH-1,
     parameter ASB   = WIDTH-2,
     parameter HEADER_BYTE  = 8'hA7, // Pattern to send as the first byte
     parameter DELAY = 3)
   ( // Wishbone-like (bus master) interface:
     input              clk_i,
     input              rst_i,
     output reg         cyc_o = 1'b0,
     output reg         stb_o = 1'b0,
     output             we_o,
     input              ack_i,
     input              wat_i,  // TODO: currently ignored
     input              rty_i,  // TODO: currently ignored
     input              err_i,  // TODO: currently ignored
     output reg [ASB:0] adr_o,
     input [MSB:0]      dat_i,
     output reg [MSB:0] dat_o,
     
     // Debug/diagnostic output, for when the recieve FIFO overflows.
     output             active_o,
     input [MSB:0]      status_i,
     output             overflow_o,
     output             underrun_o,

     input              SCK_pin, // Raw pin connection, not a global clock
     input              SSEL,
     input              MOSI,
     output             MISO
     );

   //  FSM state register, and some state-bit aliases:
   reg [3:0]            spi = `SPI_IDLE;
   wire                 a_spi = spi[0];
   wire                 b_spi = spi[1];
   wire                 i_spi = spi == `SPI_IDLE;

   //  Signals to/from the SPI layer:
   wire [7:0]           l_drx;
   wire                 a_we = l_drx[MSB];   // address-mode WE
   reg                  b_we = 0, l_ack = 0; // bus-mode WE
   wire                 l_cyc, l_get, l_wat;


`ifdef __icarus
   assign we_o     = cyc_o && b_we; // more convenient in GtkWave
`else
   assign we_o     = b_we;
`endif // __icarus
   assign active_o = l_cyc;


   //-------------------------------------------------------------------------
   //  FSM for the Wishbone-like bus (master) interface.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i || !l_cyc)
       spi <= #DELAY `SPI_IDLE;
     else
       case (spi)
         // new SPI transaction beginning?
         `SPI_IDLE: spi <= #DELAY l_get && r_rdy ? `SPI_ADDR : spi;

         // first byte from SPI is the write-mode and address?
         `SPI_ADDR:
           if (!l_wat) begin
              spi <= #DELAY a_we || !l_get ? `SPI_BUSY : `SPI_PULL;
           end

         `SPI_BUSY: begin
            if (l_get && !b_we && !r_rdy) // TODO: verify that this is OK!?
//             if (l_get && !b_we && !x_rdy)
              spi <= #DELAY `SPI_PULL; // pull data from the WB bus
            else if (!l_wat && b_we && !l_ack)
              spi <= #DELAY `SPI_PUSH; // push data onto the WB bus
            else                       // ignore reads when in write-mode,
              spi <= #DELAY spi;       // and writes when in read-mode
         end

         `SPI_PUSH: spi <= #DELAY ack_i ? `SPI_BUSY : spi;
         `SPI_PULL: spi <= #DELAY ack_i ? `SPI_BUSY : spi;
         default:   spi <= #DELAY 4'bx;
       endcase // case (spi)

   //-------------------------------------------------------------------------
   //  Generate the Wishbone-like flow-control signals.
   wire                 a_pull = l_get && !a_we && !r_rdy;
   wire                 b_xfer = b_pull || b_push;
   wire                 b_pull = l_get && !b_we && !r_rdy;
   wire                 b_push = !l_wat && b_we && !l_ack;

   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o} <= #DELAY 2'b00;
     else if (l_cyc)
       case (spi)
         `SPI_IDLE: {cyc_o, stb_o} <= #DELAY 2'b00;
`ifdef __WB_CLASSIC
         `SPI_ADDR: {cyc_o, stb_o} <= #DELAY {a_pull, a_pull};
         `SPI_BUSY: {cyc_o, stb_o} <= #DELAY {b_xfer, b_xfer};
         `SPI_PULL: {cyc_o, stb_o} <= #DELAY {!ack_i, !ack_i};
         `SPI_PUSH: {cyc_o, stb_o} <= #DELAY {!ack_i, !ack_i};
`else
         `SPI_ADDR: {cyc_o, stb_o} <= #DELAY {a_pull || cyc_o && !ack_i, a_pull};
         `SPI_BUSY: {cyc_o, stb_o} <= #DELAY {b_xfer || cyc_o && !ack_i, b_xfer};
         `SPI_PULL: {cyc_o, stb_o} <= #DELAY {cyc_o && !ack_i, 1'b0};
         `SPI_PUSH: {cyc_o, stb_o} <= #DELAY {!ack_i, 1'b0};
`endif
         default:   {cyc_o, stb_o} <= #DELAY 2'bx;
       endcase // case (spi)
     else
       {cyc_o, stb_o} <= #DELAY 2'b00;

   //-------------------------------------------------------------------------
   //  Gets the read#/write mode, and address, from the first byte of the SPI
   //  transaction.
   //  NOTE: `b_we` stores the last-requested bus read#/write mode.
   always @(posedge clk_i)
     if (!l_wat && !l_ack && a_spi)
       {b_we, adr_o} <= #DELAY {l_drx[MSB], l_drx[ASB:0]};

   //-------------------------------------------------------------------------
   //  Data received from `spi_layer` needs to be acknowledged.
   always @(posedge clk_i)
     if (!rst_i && !l_wat && !l_ack)
       case (spi)
         `SPI_ADDR: l_ack <= #DELAY 1;
         `SPI_BUSY: l_ack <= #DELAY 1;
         `SPI_PUSH: l_ack <= #DELAY ack_i;
         default:   l_ack <= #DELAY 0;
       endcase // case (spi)
     else
       l_ack <= #DELAY 0;

   //-------------------------------------------------------------------------
   //  Data requested by `spi_layer` needs to be flagged as ready.
   //  NOTE: Readies from the Wishbone-like SoC bus (which are just `ack_i`,
   //    when `cyc_o` is asserted, for bus read cycles), fall right through,
   //    without being registered first, saving a cycle of latency.
   reg                  r_rdy = 1'b0, r_wat = 1'b0;
   wire                 x_rdy = r_rdy || r_wat && ack_i;
   wire                 i_rdy = i_spi || a_we && a_spi || b_we && b_spi;

   //  Internal readies are registered.
   //  `r_wat` is asserted when waiting for data from the bus.
   always @(posedge clk_i) begin
     r_rdy <= #DELAY rst_i || r_rdy ? 1'b0 : l_get && i_rdy;
     r_wat <= #DELAY rst_i || ack_i ? 1'b0 : cyc_o && !b_we;
   end

   //-------------------------------------------------------------------------
   //  Data MUX to select between the status and bus data-sources.
   wire [MSB:0]         x_dtx = i_spi || a_spi ? status_i : dat_i;

   //-------------------------------------------------------------------------
   //  Hold the data from `spi_layer` constant until the end of a `spi_layer`
   //  bus write cycle.
   always @(posedge clk_i)
     dat_o <= #DELAY !l_wat && !l_ack ? l_drx : dat_o;


   //-------------------------------------------------------------------------
   //  SPI-layer, and the domain-crossing subcircuits, of the interface.
   //-------------------------------------------------------------------------
   spi_layer #( .WIDTH(WIDTH), .FSIZE(2), .HEADER_BYTE(HEADER_BYTE)
                ) SPI_LAYER0
     ( .clk_i(clk_i),
       .rst_i(rst_i),
       .cyc_o(l_cyc),
       .get_o(l_get),
       .rdy_i(x_rdy),
       .wat_o(l_wat),
       .ack_i(l_ack),
       .dat_i(x_dtx),
       .dat_o(l_drx),

       .overflow_o(overflow_o),
       .underrun_o(underrun_o),

       .SCK_pin(SCK_pin),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );

   
endmodule // spi_slave
