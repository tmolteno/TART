`timescale 1ns/1ps

/*
 * SPI slave module using asynchronous FIFO's.
 * 
 * TODO:
 *  + the design is still preliminary (as of 24/05/2016);
 * 
 * NOTE:
 *  + XST synthesis achieves aboout 250 MHz on a Spartan VI;
 * 
 */

// Bus interface states.
`define BUS_IDLE 0
`define BUS_READ 1
`define BUS_WAIT 2
`define BUS_SEND 4

// SPI interface states.
`define SPI_IDLE 0
`define SPI_BUSY 1

module spi_target
  (
   input            clk_i,
   input            rst_i,
   output reg       cyc_o = 0,
   output reg       stb_o = 0,
   output reg       we_o = 0,
   input            ack_i,
   input [7:0]      dat_i,
   output reg [7:0] dat_o,

   // Debug/diagnostic output, and for when the recieve FIFO overflows.
   output reg       overflow_o = 0,
   output reg       underrun_o = 0,

   input            SCK,
   input            SSEL,
   input            MOSI,
   output           MISO
   );

   parameter HEADER_BYTE  = 8'hA7; // Pattern to send as the first byte

   //-------------------------------------------------------------------------
   //
   //  Cross-domain synchronisation.
   //
   //-------------------------------------------------------------------------
   reg              sync0 = 0, sync1 = 0; // synchronise `cyc_i` across domains.
   reg              tx_rst_n = 1'b0, tx_flg = 1'b0;
   reg              spi_req = 0, dat_req_sync = 0, dat_req = 0;

   //-------------------------------------------------------------------------
   //  Synchronise the SSEL signal across clock domains.
   //-------------------------------------------------------------------------
   // SSEL is used to generate the framing signal, `cyc_o`, for bus transation
   // cycles.
   always @(posedge SCK or posedge SSEL)
     if (SSEL)  sync0 <= 0;
     else       sync0 <= !SSEL;

   always @(posedge clk_i)
     if (rst_i) sync1 <= 0;
     else       sync1 <= sync0;

   //-------------------------------------------------------------------------
   //  SPI transmission data prefetch.
   //-------------------------------------------------------------------------
   // After a bit has been sent/received, issue a prefetch for the next byte.
   always @(posedge SCK or posedge SSEL)
     if (SSEL)    spi_req <= 0;
     else         spi_req <= rx_count == 1;

   always @(posedge clk_i or posedge spi_req)
     if (spi_req) dat_req_sync <= 1;
     else         dat_req_sync <= 0;

   always @(posedge clk_i)
     if (rst_i)   dat_req <= 0;
     else         dat_req <= dat_req_sync;

   //-------------------------------------------------------------------------
   //  The TX FIFO reset logic.
   //-------------------------------------------------------------------------
   // Any prefetched data needs to be cleared at the end of a SPI transaction.
   // TODO: Clean this up, as it is more complicated than it needs to be.
   always @(posedge clk_i)
     if (rst_i)
       begin
          tx_rst_n <= 1'b0;
          tx_flg <= 1'b0;
       end
     else if (!sync1 && !tx_flg) // Issue a single reset
       begin
          tx_rst_n <= 1'b0;
          tx_flg <= 1'b1;
       end
     else if (sync1)
       begin
          tx_rst_n <= 1'b1;
          tx_flg <= 1'b0;
       end
     else
       begin
          tx_rst_n <= 1'b1;
          tx_flg <= tx_flg;
       end

   
   //-------------------------------------------------------------------------
   //
   //  Wishbone-like interface.
   //  
   //-------------------------------------------------------------------------
   wire [7:0] dat_w;
   wire       pop_byte  = !rx_empty && bus_state == `BUS_WAIT;
   wire       push_byte = cyc_o && stb_o && !we_o && ack_i;
   reg [2:0]  bus_state = `BUS_IDLE;

   always @(posedge clk_i)
     if (rst_i)
       bus_state <= `BUS_IDLE;
     else
       case (bus_state)
         `BUS_IDLE:
           bus_state <= sync1 && dat_req ? `BUS_READ : bus_state;
         
         `BUS_READ:
           bus_state <= !sync1 ? `BUS_IDLE : ack_i ? `BUS_WAIT : bus_state;

         `BUS_WAIT:
           bus_state <= !rx_empty ? `BUS_SEND : bus_state;

         `BUS_SEND:
           bus_state <= ack_i ? `BUS_IDLE : bus_state;
       endcase // case (bus_state)

   // TODO: Better prefetching, and don't use `rx_empty`?
   always @(posedge clk_i)
     if (rst_i) begin
        cyc_o <= 0;
        stb_o <= 0;
        we_o  <= 0;
     end
     else
       case (bus_state)
         `BUS_IDLE:             // Prefetch a byte on SPI start
           if (dat_req && sync1) begin
              cyc_o <= 1;
              stb_o <= 1;
              we_o  <= 0;
           end
           else if (!sync1) begin
              cyc_o <= 0;
              stb_o <= 0;
              we_o  <= 0;
           end
         
         `BUS_READ:             // Wait for prefetched byte
           stb_o <= ~ack_i && sync1;

         `BUS_WAIT: begin       // Send any received data
            stb_o <= ~rx_empty;
            we_o  <= ~rx_empty;
         end

         `BUS_SEND: begin       // Wait for sent data to be acknowledged
            stb_o <= ~ack_i;
            we_o  <= ~ack_i;
         end
       endcase // case (bus_state)

   // Dequeue the data to be sent over the system bus.
   always @(posedge clk_i)
     if (pop_byte) dat_o <= dat_w;
     else          dat_o <= dat_o;


   //-------------------------------------------------------------------------
   //
   //  SPI interface.
   //  
   //-------------------------------------------------------------------------
   wire [7:0] rx_data  = {rx_reg[6:0], MOSI};
   wire [7:0] tx_data;
   reg [7:0]  rx_reg, tx_reg;
   reg [2:0]  rx_count = 0, tx_count = 0;
   reg        tx_state = `SPI_IDLE;
   reg        tx_fifo_read = 0;
   wire       tx_next  = tx_count == 7;
   wire       rx_done  = rx_count == 7;

   // Output the header/status byte on SPI start, else transmit FIFO data.
   assign MISO  = tx_state == `SPI_IDLE ? HEADER_BYTE[7] : tx_reg[7];
   assign SCK_n = ~SCK;

   //-------------------------------------------------------------------------
   //  Data-capture is on positive edges.
   //-------------------------------------------------------------------------
   // Deserialise the incoming SPI data, receiving MSB -> LSB.
   always @(posedge SCK)
     rx_reg <= rx_data;

   always @(posedge SCK or posedge SSEL)
     if (SSEL) rx_count <= 0;
     else      rx_count <= rx_count + 1;

   // RX FIFO overflow detection.
   always @(posedge SCK or posedge rst_i)
     if (rst_i)
       overflow_o  <= 0;
     else if (rx_done && rx_full)
       overflow_o  <= 1;

   //-------------------------------------------------------------------------
   //  Transmission is on negative edges.
   //-------------------------------------------------------------------------
   always @(posedge SCK_n or posedge SSEL)
     if (SSEL)
       tx_state <= `SPI_IDLE;
     else
       case (tx_state)
         `SPI_IDLE: tx_state <= !SSEL ? `SPI_BUSY : `SPI_IDLE;
         default:   tx_state <= tx_state;
       endcase // case (tx_state)

   // Serialise the SPI data, sending MSB -> LSB.
   always @(posedge SCK_n)
     if (tx_state == `SPI_IDLE)
       tx_reg <= {HEADER_BYTE[6:0], HEADER_BYTE[7]};
     else if (tx_next)
       tx_reg <= tx_data;
     else
       tx_reg <= {tx_reg[6:0], tx_reg[7]};

   always @(posedge SCK_n or posedge SSEL)
     if (SSEL) tx_count <= 0;
     else      tx_count <= tx_count + 1;

   // Add a cycle of delay, avoiding an additional read just before a SPI
   // transaction completes.
   always @(posedge SCK_n or posedge SSEL)
     if (SSEL)
       tx_fifo_read <= 0;
     else
       tx_fifo_read <= tx_next && !tx_empty;

   // TX FIFO underrun detection.
   always @(posedge SCK_n or posedge rst_i)
     if (rst_i)
       underrun_o <= 0;
     else if (tx_next && tx_empty)
       underrun_o <= 1;


   //-------------------------------------------------------------------------
   //
   //  Asynchronous FIFO's for transmitting and receiving.
   //
   //-------------------------------------------------------------------------
   afifo16 #( .WIDTH(8) ) TX_FIFO0
     ( .reset_ni (tx_rst_n),
       
       .rd_clk_i (SCK_n),
       .rd_en_i  (tx_fifo_read),
       .rd_data_o(tx_data),
       
       .wr_clk_i (clk_i),
       .wr_en_i  (push_byte),
       .wr_data_i(dat_i),

       .rempty_o (tx_empty),
       .wfull_o  (tx_full)
       );

   // TODO: Not useful? Just use a synchroniser?
   afifo16 #( .WIDTH(8) ) RX_FIFO0
     ( .reset_ni (!rst_i),
       
       .rd_clk_i (clk_i),
       .rd_en_i  (pop_byte),
       .rd_data_o(dat_w),
       
       .wr_clk_i (SCK),
       .wr_en_i  (rx_done),
       .wr_data_i(rx_data),

       .rempty_o (rx_empty),
       .wfull_o  (rx_full)
       );

   
endmodule // spi_target
