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
 * This is just a simple, SPI master module and with a (pipelined, Wishbone-
 * like) bus interface.
 * 
 * NOTE:
 *  + XST synthesis achieves about 250 MHz on a Spartan VI;
 *  + currently, the SCK frequency can be slightly higher than that of the bus
 *    bus clock, or else data isn't available early enough, resulting in FIFO
 *    underruns -- but this could be solved by using an additional prefetch,
 *    though this hasn't been implemented;
 * 
 * TODO:
 *  + the design is still preliminary (as of 24/06/2016);
 *  + reduce the fanout of `rst_i`;
 *  + 27/10/2016  --  now OBSOLETE, as it needs to be rewritten to support
 *                    Wishbone SPEC B4, and be compatible with 'spi_slave_wb';
 * 
 * FIXME:
 *  + the master sends/receives one more byte than intended;
 * 
 */

// Bus interface states.
`define BUS_IDLE 0
`define BUS_BUSY 1

// SPI interface states.
`define SPI_IDLE 0
`define SPI_WAIT 1
`define SPI_SEND 2

module spi_master
  #( parameter WIDTH = 8,       // TODO: currently must be `8`!
     parameter MSB   = WIDTH-1,
     parameter ASB   = WIDTH-2,
     parameter HEADER_BYTE  = 8'hA7,
     parameter DELAY = 3) // Pattern to send as the first byte
   ( // Wishbone-like (bus slave) interface:
     input            clk_i,
     input            rst_i,
     input            cyc_i,
     input            stb_i,
     input            we_i,
     input            bst_i,
     output reg       ack_o = 0,
     output reg       rdy_o = 0,
     output reg       wat_o = 0,
     input [ASB:0]    adr_i,
     input [MSB:0]    dat_i,
     output reg [7:0] dat_o,

     input            SCK,
     output reg       SCK_enable = 0, // suppress SCK when data isn't ready
     output reg       SSEL = 1,
     output reg       MOSI = 0,
     input            MISO
     );

   reg                sync0, sync1; // synchronise `cyc_i` across domains.
   wire               tx_empty, tx_full;
   wire               rx_empty, rx_full;


   // Synchronise bus cycles across clock domains.
   always @(posedge clk_i)
     if (rst_i)
       sync0 <= #DELAY 1;
     else
       sync0 <= #DELAY !cyc_i;

   // TODO: Does this cleanly terminate SPI transfers?
   always @(posedge SCK)
     if (rst_i)
       begin
          sync1 <= #DELAY 1;
          SSEL  <= #DELAY 1;
       end
     else
       begin
          sync1 <= #DELAY sync0;
          // FIXME: Use bus/SPI state?
//           SSEL  <= sync1 || spi_done;
          SSEL  <= #DELAY sync1 && !SCK_enable;
       end

   reg spi_done = 0;
   always @(posedge SCK or posedge rst_i)
     if (rst_i)
       spi_done <= #DELAY 0;
     else if (bit_count == 7 && tx_empty)
       spi_done <= #DELAY 1;
     else if (!cyc_i)
       spi_done <= #DELAY 0;

   
   //-------------------------------------------------------------------------
   //
   //  Wishbone-like interface.
   //  
   //-------------------------------------------------------------------------
   wire [7:0] dat_w;
   wire       pop_byte  = !rx_empty && bus_state != `BUS_IDLE;
   reg        bus_state = `BUS_IDLE;
   wire       push_byte = cyc_i && stb_i && !tx_full;

   always @(posedge clk_i)
     if (rst_i)
       bus_state <= #DELAY `BUS_IDLE;
     else
       case (bus_state)
         `BUS_IDLE:
           bus_state <= #DELAY cyc_i && stb_i ? `BUS_BUSY : `BUS_IDLE;
         
         `BUS_BUSY:
//            bus_state <= !rx_empty && !stb_i ? `BUS_IDLE : `BUS_BUSY;
           bus_state <= #DELAY !cyc_i ? `BUS_IDLE : `BUS_BUSY;
       endcase // case (bus_state)

   always @(posedge clk_i)
     if (rst_i || ack_o) ack_o <= #DELAY 0;
     else                ack_o <= #DELAY push_byte;

   // Insert wait-states if the master attempts to use burst-transfers.
   wire       wat_w = tx_full || bst_i && !wat_o;

   always @(posedge clk_i)
     if (rst_i)      wat_o <= #DELAY 0;
     else if (wat_o) wat_o <= #DELAY tx_full;
     else            wat_o <= #DELAY bst_i || tx_full;

   always @(posedge clk_i)
     if (pop_byte && !rdy_o)
       begin
          dat_o <= #DELAY dat_w;
          rdy_o <= #DELAY 1;
       end
     else
       begin
          dat_o <= #DELAY dat_o;
          rdy_o <= #DELAY 0;
       end


   //-------------------------------------------------------------------------
   //
   //  SPI interface.
   //  
   //-------------------------------------------------------------------------
   wire [7:0] tx_data;
   reg [1:0]  spi_state = `SPI_IDLE;
   reg [7:0]  shift_reg;
   reg [2:0]  bit_count = 0;
   reg        byte_ready = 0;
   wire       data_sent = bit_count == 7 && tx_empty;
   wire       next_byte = spi_state == `SPI_WAIT && !tx_empty || bit_count == 7 && !tx_empty;

//    assign MOSI = shift_reg[7];

   always @(posedge SCK)
     if (rst_i || SSEL)
       spi_state <= #DELAY `SPI_IDLE;
     else
       case (spi_state)
         `SPI_IDLE:
           spi_state <= #DELAY !SSEL ? `SPI_WAIT : `SPI_IDLE;

         `SPI_WAIT:
           spi_state <= #DELAY !tx_empty ? `SPI_SEND : `SPI_WAIT;

         `SPI_SEND:
           spi_state <= #DELAY data_sent ? `SPI_WAIT : `SPI_SEND;
       endcase // case (spi_state)

   // Enable the SPI clock only when valid data is ready to be sent.
   // TODO: Make this glitch-free!
//    always @(posedge SCK or posedge SSEL)
//      if (SSEL || rst_i)
   always @(negedge SCK)
     if (rst_i)
       SCK_enable <= #DELAY 0;
     else
       SCK_enable <= #DELAY spi_state == `SPI_SEND;
   /*
     else if (next_byte)
       SCK_enable <= #DELAY 1;
     else if (data_sent)
       SCK_enable <= #DELAY 0;
    */
   
   // Serialise the SPI data, sending MSB -> LSB.
   always @(posedge SCK)
     if (spi_state == `SPI_SEND && !data_sent)
       bit_count <= #DELAY bit_count + 1;
     else
       bit_count <= #DELAY 0;

   always @(posedge SCK)
     if (next_byte)
       shift_reg <= #DELAY tx_data;
     else if (spi_state == `SPI_SEND)
       shift_reg <= #DELAY {shift_reg[6:0], MISO};

   always @(negedge SCK)
     MOSI <= #DELAY shift_reg[7];

   reg [7:0]  in_reg;
   always @(posedge SCK)
     in_reg <= #DELAY {in_reg[6:0], MISO};

   // Deserialise the incoming SPI data, receiving MSB -> LSB.
   always @(posedge SCK)
     begin
        byte_ready <= #DELAY bit_count == 7;
     end

   // The RX FIFO reset logic, because it needs any prefetched data to be
   // cleared at the end of a SPI transaction.
   reg  rx_rst_n = 1'b0;
   reg  rx_flg = 1'b0;

   always @(posedge clk_i or posedge rst_i)
     if (rst_i) begin
        rx_rst_n <= #DELAY 1'b0;
        rx_flg <= #DELAY 1'b0;
     end
     else if (!cyc_i && !rx_flg) begin // Issue a single reset
        rx_rst_n <= #DELAY 1'b0;
        rx_flg <= #DELAY 1'b1;
     end
     else if (cyc_i) begin
        rx_rst_n <= #DELAY 1'b1;
        rx_flg <= #DELAY 1'b0;
     end
     else begin
        rx_rst_n <= #DELAY 1'b1;
        rx_flg <= #DELAY rx_flg;
     end


   //-------------------------------------------------------------------------
   //
   //  Asynchronous FIFO's for transmitting and receiving.
   //
   //-------------------------------------------------------------------------
   // TODO: Clear the FIFO when `cyc_o` deasserts?
   wire [MSB:0] dat_tx = bus_state == `BUS_IDLE ? {we_i, adr_i} : (dat_next ? dat : dat_i);
   wire         tx_byte = push_byte && !ack_o || dat_next;
   reg [MSB:0]  dat;
   reg          dat_next = 0;

   always @(posedge clk_i)
     if (cyc_i && stb_i) dat <= #DELAY dat_i;

   always @(posedge clk_i)
     if (rst_i) dat_next <= #DELAY 0;
     else if (cyc_i && stb_i && bus_state == `BUS_IDLE) dat_next <= #DELAY 1;
     else dat_next <= #DELAY 0;


   afifo16 #( .WIDTH(8) ) TX_FIFO1
     ( .reset_ni(!rst_i),
       
       .rd_clk_i(SCK),
       .rd_en_i(next_byte),
       .rd_data_o(tx_data),
       
       .wr_clk_i(clk_i),
       .wr_en_i(tx_byte),
//        .wr_data_i(dat_i),
       .wr_data_i(dat_tx),

       .rempty_o(tx_empty),
       .wfull_o(tx_full)
       );

   afifo16 #( .WIDTH(8) ) RX_FIFO1
     ( .reset_ni(rx_rst_n),
       
       .rd_clk_i(clk_i),
       .rd_en_i(pop_byte),
       .rd_data_o(dat_w),
       
       .wr_clk_i(SCK),
       .wr_en_i(byte_ready),
//        .wr_data_i(shift_reg),
       .wr_data_i(in_reg),

       .rempty_o(rx_empty),
       .wfull_o(rx_full)
       );

   
endmodule // spi_master
