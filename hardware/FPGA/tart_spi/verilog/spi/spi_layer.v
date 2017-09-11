`timescale 1ns/100ps
/*
 * Module      : verilog/spi/spi_layer.v
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
 * SPI clock-domain circuitry, plus the cross-domain synchronisers and
 * asynchronous FIFO's.
 * 
 * NOTE:
 *  + XST synthesis achieves aboout 250 MHz on a Spartan VI;
 * 
 * TODO:
 *  + rename the signals, as their names are too similar to Wishbone names;
 *  + can the output-delay be run-time configurable?
 *  + check the timings of the various TX & RX edge configurations;
 *  + replace the RX FIFO to a smaller design; e.g., a shift-register, and a
 *    synchroniser?
 * 
 */

// TODO: Not very modular, as the rest of this module is not TART-specific.
`include "tartcfg.v"

//----------------------------------------------------------------------------
//
//  Configuration options.
//
//----------------------------------------------------------------------------
//
// To simulate using Icarus Verilog:
// `define __icarus
// or, pass this on the command-line:
//   > iverilog -D__icarus ...
//
// If the SPI interface will be used with the SCK frequency below 32 MHz, then
// use legacy mode:
// `define __LEGACY_MODE

// Sets the transmit edge, depending upon the mode.
`ifdef __LEGACY_MODE
 `define TX_EDGE negedge
`elsif __icarus
 `define TX_EDGE negedge
`else
 `define TX_EDGE posedge
`endif

module spi_layer
  #( parameter WIDTH = 8,       // TODO: currently must be `8`!
     parameter MSB   = WIDTH-1,
     parameter ASB   = WIDTH-2,
     parameter FSIZE = 2,           // FIFO size (log2)
     parameter HEADER_BYTE = 8'hA7, // Pattern to send as the first byte
     parameter DELAY = 3)
   ( input        clk_i,
     input        rst_i,

     output       cyc_o, // SPI physical layer is active
     output       get_o, // Request TX data
     input        rdy_i, // TX data ready
     output       wat_o, // Asserted when no RX data
     input        ack_i, // RX data stored if ACK
     input [7:0]  dat_i,
     output [7:0] dat_o,

     // Debug/diagnostic output, for when the recieve FIFO overflows.
     output reg   overflow_o = 1'b0,
     output reg   underrun_o = 1'b0,

     input        SCK_pin, // Raw pin connection, not a global clock
     input        SSEL,
     input        MOSI,
     (* IOB = "FORCE" *)
     output reg   MISO = HEADER_BYTE[7]
     );

   //-------------------------------------------------------------------------
   //  
   //  SPI clocking.
   //  
   //-------------------------------------------------------------------------
   //-------------------------------------------------------------------------
   //  Transmission is supposed to be on negative edges, but at high speeds,
   //  the output latencies are large enough to require that transmission
   //  begins on the preceding positive edges.
   //-------------------------------------------------------------------------
   // TODO: Better configuration options.
`ifdef __LEGACY_MODE
   wire           SCK      = SCK_pin;
   wire           SCK_miso = SCK_pin;
   wire           SCK_tx   = ~SCK_pin;
`elsif __icarus
   wire           SCK, SCK_miso, SCK_tx;

   assign SCK      = SCK_pin;
   assign SCK_miso = SCK_pin;
   assign SCK_tx   = ~SCK_pin;
`else
   // The clock path:
   //    SCK -> IBUFG -> fabric -> BUFGMUX -> fabric -> OBUF -> MISO
   // is too long for high speed implementations, so a local I/O clock is
   // used to drive MISO.
   wire           SCK_buf, SCK_bufg, SCK_miso, SCK_tx, SCK;

   assign SCK    = SCK_bufg;    // Main SPI-domain clock
   assign SCK_tx = SCK_bufg;    // TX FIFO clock

   IBUFG IBUFG_SCK0 ( .I(SCK_pin), .O(SCK_buf ) );
   BUFG  BUFG_SCK0  ( .I(SCK_buf), .O(SCK_bufg) );

   // Use local I/O clocking resources to drive the output D-type flip-flop.
   // TODO: Does this work with an intermittent input clock signal?
   // TODO: Passed test @62.5 MHz, with a Raspberry Pi II (Pat @01/06/2016).
   BUFIO2
     #(  .DIVIDE(1),
         .DIVIDE_BYPASS("TRUE"),
         .I_INVERT("FALSE"),
         .USE_DOUBLER("FALSE")
         ) BUFIO2_MISO0
       ( .DIVCLK(),       // unused
         .IOCLK(SCK_miso),
         .SERDESSTROBE(), // unused
         .I(SCK_buf)
         );
`endif

   //  FIFO status signals:
   wire               tx_empty;
   wire               rx_empty, rx_full;


   //-------------------------------------------------------------------------
   //
   //  Cross-domain synchronisation.
   //
   //-------------------------------------------------------------------------
   reg            tx_rst = 1'b1, tx_flg = 1'b0;
   reg            spi_req = 1'b0, dat_req = 1'b0;
   reg            dat_req_sync = 1'b1, dat_req_done = 1'b0;

   //-------------------------------------------------------------------------
   //  Synchronise the SSEL signal across clock domains.
   //-------------------------------------------------------------------------
   // SSEL is used to generate the bus transaction's framing signal, `cyc_o`,
   // but it must be synchronised across clock domains.
   reg            ssel_pos, ssel_neg;
   reg            spi_select = 1'b0, old_select = 1'b0;

   assign cyc_o = spi_select;
   assign get_o = dat_req;
   assign wat_o = rx_empty;


   //  SPI transaction beginning.
   always @(posedge clk_i or negedge SSEL)
     if (!SSEL) ssel_neg <= #DELAY 1'b1;
     else       ssel_neg <= #DELAY 1'b0;

   //  End of SPI transaction.
   always @(posedge clk_i or posedge SSEL)
     if (SSEL)  ssel_pos <= #DELAY 1'b1;
     else       ssel_pos <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (rst_i)         spi_select <= #DELAY 1'b0;
     else if (ssel_neg) spi_select <= #DELAY 1'b1;
     else if (ssel_pos) spi_select <= #DELAY 1'b0;
     else               spi_select <= #DELAY spi_select;


   //-------------------------------------------------------------------------
   //  SPI transmission data prefetch.
   //-------------------------------------------------------------------------
   //  After a bit has been sent/received, issue a prefetch for the next byte.
   always @(posedge SCK or posedge SSEL)
     if (SSEL)    spi_req <= #DELAY 1'b0;
     else         spi_req <= #DELAY rx_count == 3'h0;

   always @(posedge clk_i or posedge spi_req)
     if (spi_req)           dat_req_sync <= #DELAY 1'b1;
     else if (dat_req_sync) dat_req_sync <= #DELAY !dat_req_done;
     else                   dat_req_sync <= #DELAY dat_req_sync;

   //-------------------------------------------------------------------------
   //  Request more data when a new SPI byte transfer begins, but prevent
   //  multiple requests for slow SPI clocks, using a one-shot.
   always @(posedge clk_i)
     if (rst_i) begin
       dat_req_done <= #DELAY 1'b0;
       dat_req      <= #DELAY 1'b0;
     end
     else begin
        dat_req_done <= #DELAY dat_req_done ? dat_req_sync : dat_req && rdy_i;
        dat_req      <= #DELAY dat_req_sync && !dat_req_done && !rdy_i;
     end


   //-------------------------------------------------------------------------
   //  The TX FIFO reset logic.
   //-------------------------------------------------------------------------
   // Any prefetched data needs to be cleared at the end of a SPI transaction.
   // TODO: Clean this up, as it is more complicated than it needs to be.
   always @(posedge clk_i)
     if (rst_i) begin
        tx_rst <= #DELAY 1'b1;
        tx_flg <= #DELAY 1'b0;
     end
     else if (!spi_select && !tx_flg) begin // Issue a single reset
        tx_rst <= #DELAY 1'b1;
        tx_flg <= #DELAY 1'b1;
     end
     else if (spi_select) begin
        tx_rst <= #DELAY 1'b0;
        tx_flg <= #DELAY 1'b0;
     end
     else begin
        tx_rst <= #DELAY 1'b0;
        tx_flg <= #DELAY tx_flg;
     end


   //-------------------------------------------------------------------------
   //
   //  SPI interface.
   //  
   //-------------------------------------------------------------------------
   wire [7:0] rx_data  = {rx_reg[6:0], MOSI};
   wire [3:0] rx_inc = rx_count + 1;
   reg [2:0]  rx_count = 3'h0;
   reg [6:0]  rx_reg;
   wire       rx_push  = rx_count == 7;

   //-------------------------------------------------------------------------
   //  Data-capture is on positive edges.
   //-------------------------------------------------------------------------
   // Deserialise the incoming SPI data, receiving MSB -> LSB.
   always @(posedge SCK)
     rx_reg <= #DELAY {rx_reg[5:0], MOSI};

   always @(posedge SCK or posedge SSEL)
     if (SSEL) rx_count <= #DELAY 3'h0;
     else      rx_count <= #DELAY rx_inc[2:0];

   // RX FIFO overflow detection.
   always @(posedge SCK or posedge rst_i)
     if (rst_i)
       overflow_o  <= #DELAY 1'b0;
     else if (rx_push && rx_full)
       overflow_o  <= #DELAY 1'b1;

   //-------------------------------------------------------------------------
   //  Transmission logic.
   //-------------------------------------------------------------------------
   wire [7:0] tx_data;
   reg [6:0]  tx_reg = HEADER_BYTE[6:0];
   reg [2:0]  tx_count = 3'h0;
   reg        tx_pull = 1'b0;
   wire       tx_next  = tx_count == 7;
   wire [3:0] tx_inc = tx_count + 1;

   // Output the header/status byte on SPI start, else transmit FIFO data.
   // Serialise the SPI data, sending MSB -> LSB.
   always @(`TX_EDGE SCK or posedge SSEL)
     if (SSEL)         tx_reg <= #DELAY HEADER_BYTE[6:0];
     else if (tx_next) tx_reg <= #DELAY tx_data[6:0];
     else              tx_reg <= #DELAY {tx_reg[5:0], 1'bx};

   // Use a local clock for faster source-synchronous transmission.
   always @(`TX_EDGE SCK_miso or posedge SSEL)
     if (SSEL)         MISO <= #DELAY HEADER_BYTE[7];
     else if (tx_next) MISO <= #DELAY tx_data[7];
     else              MISO <= #DELAY tx_reg[6];

   always @(`TX_EDGE SCK or posedge SSEL)
     if (SSEL) tx_count <= #DELAY 3'h0;
     else      tx_count <= #DELAY tx_inc[2:0];

   // Add a cycle of delay, avoiding an additional read just before a SPI
   // transaction completes.
   always @(`TX_EDGE SCK or posedge SSEL)
     if (SSEL) tx_pull <= #DELAY 1'b0;
     else      tx_pull <= #DELAY tx_next;

   // TX FIFO underrun detection.
   always @(`TX_EDGE SCK or posedge rst_i)
     if (rst_i)
       underrun_o <= #DELAY 1'b0;
     else if (tx_pull && tx_empty)
       underrun_o <= #DELAY 1'b1;


   //-------------------------------------------------------------------------
   //
   //  Asynchronous FIFO's for transmitting and receiving.
   //
   //-------------------------------------------------------------------------
   afifo_gray #( .WIDTH(8), .ABITS(FSIZE) ) TX_FIFO0
     ( .rd_clk_i (SCK_tx),
       .rd_en_i  (tx_pull),
       .rd_data_o(tx_data),
       
       .wr_clk_i (clk_i),
       .wr_en_i  (rdy_i),
       .wr_data_i(dat_i),

       .rst_i    (tx_rst),
       .rempty_o (tx_empty),
       .wfull_o  ()
       );

   // TODO: There is never more than one and a bit bytes stored within this
   //   FIFO, so should there be something more efficient?
   afifo_gray #( .WIDTH(8), .ABITS(FSIZE) ) RX_FIFO0
     ( .rd_clk_i (clk_i),
       .rd_en_i  (ack_i),
       .rd_data_o(dat_o),
       
       .wr_clk_i (SCK),
       .wr_en_i  (rx_push),
       .wr_data_i(rx_data),

       .rst_i    (rst_i),
       .rempty_o (rx_empty),
       .wfull_o  (rx_full)
       );

   
endmodule // spi_layer
