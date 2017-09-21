`timescale 1ns/100ps
/*
 * Module      : verilog/spi/spi_slave_wb.v
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
 * (pipelined, Wishbone SPEC B4) bus interface.
 * 
 * NOTE:
 *  + XST synthesis achieves about 250 MHz on a Spartan 6;
 *  + currently, the SCK frequency can be no more than slightly higher than
 *    that of the (Wishbone, SPEC B4) bus clock, or else data isn't available
 *    early enough, resulting in FIFO underruns -- but this could be solved
 *    using an additional prefetch (OR, an additional padding byte), though
 *    this hasn't been implemented;
 * 
 * Changelog:
 *  + 26/10/2016  --  initial file (built from the old SPI module);
 * 
 * TODO:
 *  + short and/or long addresses?
 * 
 */

`define SPI_IDLE 2'h0
`define SPI_ADDR 2'h1
`define SPI_BUSY 2'h2

module spi_slave_wb
  #( // Bus bit-widths:
     parameter WIDTH = 8,       // TODO: currently must be `8`!
     parameter MSB   = WIDTH-1,
     parameter ASB   = WIDTH-2,
//      parameter BYTES = WIDTH>>3, // TODO: byte selects
//      parameter SSB   = BYTES-1,

     // Wishbone modes/parameters:
     parameter ASYNC = 1,
     parameter PIPED = 1,
     parameter CHECK = 1,

     parameter HEADER_BYTE  = 8'hA7, // Pattern to send as the first byte
     parameter DELAY = 3)
   ( // Wishbone (SPEC B4) interface:
     input          clk_i,
     input          rst_i,
     output         cyc_o,
     output         stb_o,
     output         we_o,
     input          ack_i,
     input          wat_i,
     input          rty_i,
     input          err_i,
     output [ASB:0] adr_o,
//      input [SSB:0]  sel_i, // TODO: support larger word-sizes?
     input [MSB:0]  dat_i,
     output [MSB:0] dat_o,
   
     // Debug/diagnostic output, for when the recieve FIFO overflows.
     output         active_o,
     input [MSB:0]  status_i,
     output         overflow_o,
     output         underrun_o,

     input          SCK_pin, // Raw pin connection, not a global clock
     input          SSEL,
     input          MOSI,
     output         MISO
     );


   //-------------------------------------------------------------------------
   //  SPI FSM register:
   reg [1:0]        spi = `SPI_IDLE;
   reg              frame = 1'b0;

   //  SPI layer control signals:
   wire             l_cyc, l_get, l_wat;
   wire [7:0]       l_drx;
   reg              l_rdy = 1'b0; // Fetched data ready
   reg              l_ack = 1'b0; // Data received and acknowledged
   wire [MSB:0]     x_dtx; // Just send status, unless Wishbone read
   wire             x_rdy; // Internal or Wishbone ready?

   //  Signals for decoding SPI commands:
   wire             cmd_we  = l_drx[MSB];   // register read#/write
   wire [ASB:0]     cmd_adr = l_drx[ASB:0]; // register address

   //  Wishbone bus and control signals:
   reg              we = 1'b1;
   reg [ASB:0]      adr;
   reg [MSB:0]      dat;
   wire             busy, done, fail;
   wire             start, first, fetch, store;


   //-------------------------------------------------------------------------
   //  Map signals to the module outputs.
   assign we_o     = we;
   assign adr_o    = adr;
   assign dat_o    = dat;

   assign active_o = l_cyc;


   //-------------------------------------------------------------------------
   //
   //  FINITE STATE MACHINE (FSM) FOR THE SPI SLAVE.
   //
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (!l_cyc)
       spi <= #DELAY `SPI_IDLE;
     else
       case (spi)
         // To begin a new SPI transaction beginning, the SPI layer module
         // requests the status-byte. Once this has been transferred, wait for
         // the register mode & address.
         `SPI_IDLE:
           spi <= #DELAY l_get && l_rdy ? `SPI_ADDR : spi;

         // If the first byte from SPI has its MSB asserted, then the transfer
         // is a register-write, so change the state to waiting for data,
         // otherwise begin a register-read transaction. OR, if a read is
         // requested, by the SPI layer then??
         `SPI_ADDR:
           if (l_get) spi <= #DELAY `SPI_BUSY;

         // Arriving data is to be put onto the Wishbone bus, and requested
         // data from the bus.
         `SPI_BUSY:
           spi <= #DELAY spi;

         default:
           spi <= #DELAY 2'bx;
       endcase // case (spi)

   //  Frames SPI transactions.
   always @(posedge clk_i)
     if (!l_cyc)
       frame <= #DELAY 1'b0;
     else if (spi == `SPI_IDLE && l_get && l_rdy)
       frame <= #DELAY 1'b1;


   //-------------------------------------------------------------------------
   //
   //  WISHBONE (SPEC B4) INTERCONNECT.
   //
   //-------------------------------------------------------------------------
   //  Initiate a Wishbone transaction whenever:
   //   a) register address received, and the command is READ;
   //   b) SPI layer requests another register READ; or
   //   c) register data received, to be written.
   assign first = spi == `SPI_ADDR && !l_wat && !cmd_we;
   assign fetch = spi == `SPI_BUSY &&  l_get && !we;
   assign store = spi == `SPI_BUSY && !l_wat &&  we;

   assign start = !busy && (first || fetch || store);


   //-------------------------------------------------------------------------
   //  Gets the read#/write mode from the first byte of the SPI transaction.
   //  NOTE: `we` stores the last-requested bus read#/write mode, and is also
   //    used to control the "internal readies" ;i.e., by pulling `we` HI at
   //    the end of each transaction, then until a Wishbone READ command is
   //    requested, all readies can be generated internally.
   always @(posedge clk_i)
     if (!l_cyc)
       we <= #DELAY 1'b1;
     else if (!l_wat && spi == `SPI_ADDR)
       we <= #DELAY cmd_we;
     else
       we <= #DELAY we;

   //  Capture the Wishbone address from the first byte of the SPI
   //  transaction.
   always @(posedge clk_i)
     if (!l_wat && spi == `SPI_ADDR) adr <= #DELAY cmd_adr;

   //  Hold the data from `spi_layer` constant until the end of a `spi_layer`
   //  bus write cycle.
   always @(posedge clk_i)
     if (!l_wat && !busy) dat <= #DELAY l_drx;


   //-------------------------------------------------------------------------
   //
   //  SPI LAYER CONTROL.
   //
   //-------------------------------------------------------------------------
   assign x_dtx = we ? status_i : dat_i;
   assign x_rdy = l_rdy || !we && done;


   //-------------------------------------------------------------------------
   //  The `l_rdy` signal is for driving internal (to this module) data onto
   //  the SPI bus. The cases where the data is considered "internal" are
   //  when:
   //   1) requests are for the status byte; or
   //   2) "filler" is to be sent for register-write transactions.
   always @(posedge clk_i)
     if (l_get && we && !l_rdy)
       l_rdy <= #DELAY 1'b1;
     else
       l_rdy <= #DELAY 1'b0;

   //  The `l_ack` signal acknowledges any data received from the SPI layer.
   always @(posedge clk_i)
     if (frame && !l_wat && !l_ack)
       l_ack <= #DELAY 1'b1;
     else
       l_ack <= #DELAY 1'b0;


   //-------------------------------------------------------------------------
   //  Frame the external Wishbone bus cycles.
   //-------------------------------------------------------------------------
   wb_cycle
     #( .ASYNC(ASYNC),   // should be `1` unless very slow clock rates
        .PIPED(PIPED),   // should be `1` for Wishbone SPEC B4
        .CHECK(CHECK),   // `0` is probably OK for point-to-point, else `1`
        .DELAY(DELAY)
        ) RWCYC
       (
        .clk_i  (clk_i),
        .rst_i  (rst_i),
        .cyc_o  (cyc_o),
        .stb_o  (stb_o),
        .ack_i  (ack_i),
        .wat_i  (wat_i),
        .rty_i  (rty_i),
        .err_i  (err_i),
        .start_i(start),
        .busy_o (busy),
        .done_o (done),
        .fail_o (fail)
        );


   //-------------------------------------------------------------------------
   //  SPI-layer, and the domain-crossing subcircuits, of the interface.
   //-------------------------------------------------------------------------
   spi_layer
     #( .WIDTH(WIDTH),
        .FSIZE(2),
        .HEADER_BYTE(HEADER_BYTE)
        ) SPI_LAYER0
       (
        .clk_i(clk_i),
        .rst_i(rst_i),
        .cyc_o(l_cyc),          // asserted for SPI transfer duration
        .get_o(l_get),          // reads are readied
        .rdy_i(x_rdy),
        .wat_o(l_wat),          // writes are acknowledged
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

   
endmodule // spi_slave_wb
