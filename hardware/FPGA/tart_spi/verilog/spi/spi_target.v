`timescale 1ns/1ps

/*
 * SPI slave module using asynchronous FIFO's.
 * 
 * NOTE:
 *  + XST synthesis achieves aboout 250 MHz on a Spartan VI;
 * 
 * TODO:
 *  + the design is still preliminary (as of 24/05/2016);
 *  + constrain the input and output OFFSET's;
 *  + can the output-delay be run-time configurable?
 *  + change the RX-FIFO to just one bit wide, and deserialise in the bus
 *    domain? Can this cause problems for slow domains?
 * 
 */

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
// If the SPI interface will be used with SCK below 32 MHz, then use legacy
// mode:
// `define __LEGACY_MODE

`ifdef __LEGACY_MODE
 `define TX_EDGE negedge
`elsif __icarus
 `define TX_EDGE negedge
`else
 `define TX_EDGE posedge
`endif

// Support CLASSIC Wishbone-like bus transactions?
`define __WB_CLASSIC
// `undef __WB_CLASSIC

// Bus interface states.
`define BUS_IDLE 0
`define BUS_READ 1
`define BUS_WAIT 2
`define BUS_SEND 4

module spi_target
  #( parameter WIDTH = 8,       // TODO: currently must be `8`!
     parameter MSB   = WIDTH-1,
     parameter ASB   = WIDTH-2,
     parameter HEADER_BYTE  = 8'hA7, // Pattern to send as the first byte
     parameter DELAY = 3)
   ( // Wishbone-like (bus master) interface:
     input            clk_i,
     input            rst_i,
     output           cyc_o,
     output reg       stb_o = 0,
     output           bst_o,
     output reg       we_o = 0,
     input            ack_i,
     input            rdy_i,
     input [7:0]      dat_i,
     output reg [7:0] dat_o,

     // Debug/diagnostic output, for when the recieve FIFO overflows.
     output reg       overflow_o = 0,
     output reg       underrun_o = 0,

     input            SCK_pin, // Raw pin connection, not a global clock
     input            SSEL,
     input            MOSI,
     (* IOB = "FORCE" *)
     output reg       MISO = HEADER_BYTE[7]
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
   wire             SCK      = SCK_pin;
   wire             SCK_miso = SCK_pin;
   wire             SCK_tx   = ~SCK_pin;
`elsif __icarus
   wire             SCK, SCK_miso, SCK_tx;

   assign SCK      = SCK_pin;
   assign SCK_miso = SCK_pin;
   assign SCK_tx   = SCK_pin;
`else
   // The clock path:
   //    SCK -> IBUFG -> fabric -> BUFGMUX -> fabric -> OBUF -> MISO
   // is too long for high speed implementations, so a local I/O clock is
   // used to drive MISO.
   wire             SCK_buf, SCK_bufg, SCK_miso, SCK_tx, SCK;

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
       ( .DIVCLK(DIVCLK),       // unused
         .IOCLK(SCK_miso),
         .SERDESSTROBE(SERDESSTROBE), // unused
         .I(SCK_buf)
         );
`endif

   //-------------------------------------------------------------------------
   //
   //  Cross-domain synchronisation.
   //
   //-------------------------------------------------------------------------
   reg              tx_rst_n = 1'b0, tx_flg = 1'b0;
   reg              spi_req = 0, dat_req_sync = 1, dat_req = 0;

   //-------------------------------------------------------------------------
   //  Synchronise the SSEL signal across clock domains.
   //-------------------------------------------------------------------------
   // SSEL is used to generate the bus transaction's framing signal, `cyc_o`,
   // but it must be synchronised across clock domains.
`ifdef __USE_OLD_SSEL
   reg              sync0 = 0, sync1 = 0;
   wire             spi_select = sync1;

   always @(posedge SCK or posedge SSEL)
     if (SSEL)  sync0 <= #DELAY 0;
     else       sync0 <= #DELAY !SSEL;

   always @(posedge clk_i)
     if (rst_i) sync1 <= #DELAY 0;
     else       sync1 <= #DELAY sync0;


   //-------------------------------------------------------------------------
   //  SPI transmission data prefetch.
   //-------------------------------------------------------------------------
   // After a bit has been sent/received, issue a prefetch for the next byte.
   always @(posedge SCK or posedge SSEL)
     if (SSEL)    spi_req <= #DELAY 0;
     else         spi_req <= #DELAY rx_count == 0;

   always @(posedge clk_i or posedge spi_req)
     if (spi_req)           dat_req_sync <= #DELAY 1;
     else if (dat_req_sync) dat_req_sync <= #DELAY 0;
     else                   dat_req_sync <= #DELAY dat_req_sync;

   always @(posedge clk_i)
     if (rst_i)   dat_req <= #DELAY 0;
//      else         dat_req <= #DELAY dat_req_sync;
     else         dat_req <= #DELAY dat_req ? bus_state != `BUS_READ : dat_req_sync;

   /*
   reg              spi_selreg = 0;
   always @(posedge clk_i)
     if (rst_i) spi_selreg <= #DELAY 0;
     else spi_selreg <= #DELAY spi_selreg;
    */

`else
   reg              ssel_pos, ssel_neg;
   reg              spi_select = 0, old_select = 0;

   assign cyc_o = spi_select;

   //  SPI transaction beginning.
   always @(posedge clk_i or negedge SSEL)
     if (!SSEL) ssel_neg <= #DELAY 1;
     else       ssel_neg <= #DELAY 0;

   //  End of SPI transaction.
   always @(posedge clk_i or posedge SSEL)
     if (SSEL)  ssel_pos <= #DELAY 1;
     else       ssel_pos <= #DELAY 0;

   always @(posedge clk_i)
     if (rst_i)         spi_select <= #DELAY 0;
     else if (ssel_neg) spi_select <= #DELAY 1;
     else if (ssel_pos) spi_select <= #DELAY 0;
     else               spi_select <= #DELAY spi_select;

   always @(posedge clk_i)
     old_select <= #DELAY spi_select;


   //-------------------------------------------------------------------------
   //  SPI transmission data prefetch.
   //-------------------------------------------------------------------------
   // After a bit has been sent/received, issue a prefetch for the next byte.
   always @(posedge SCK or posedge SSEL)
     if (SSEL)    spi_req <= #DELAY 0;
     else         spi_req <= #DELAY rx_count == 0;

   always @(posedge clk_i or posedge spi_req)
     if (spi_req)           dat_req_sync <= #DELAY 1;
//      else if (dat_req_sync) dat_req_sync <= #DELAY 0;
     else if (dat_req_sync) dat_req_sync <= #DELAY !dat_req;
     else                   dat_req_sync <= #DELAY dat_req_sync;

   always @(posedge clk_i)
     if (rst_i)
       dat_req <= #DELAY 0;
     else if (dat_req && stb_o && !we_o)
       dat_req <= #DELAY 0;
     else if (!dat_req)
       dat_req <= #DELAY dat_req_sync; // || spi_select && !old_select && bus_state == `BUS_IDLE;
     else
       dat_req <= #DELAY dat_req;

//      else         dat_req <= #DELAY dat_req_sync && !dat_req;
//      else         dat_req <= #DELAY dat_req ? bus_state != `BUS_READ : dat_req_sync;

   /*
   reg              spi_selreg = 0;
   always @(posedge clk_i)
     if (rst_i) spi_selreg <= #DELAY 0;
     else spi_selreg <= #DELAY spi_selreg;
    */
`endif


   //-------------------------------------------------------------------------
   //  The TX FIFO reset logic.
   //-------------------------------------------------------------------------
   // Any prefetched data needs to be cleared at the end of a SPI transaction.
   // TODO: Clean this up, as it is more complicated than it needs to be.
   always @(posedge clk_i)
     if (rst_i) begin
        tx_rst_n <= #DELAY 1'b0;
        tx_flg   <= #DELAY 1'b0;
     end
     else if (!spi_select && !tx_flg) begin // Issue a single reset
        tx_rst_n <= #DELAY 1'b0;
        tx_flg   <= #DELAY 1'b1;
     end
     else if (spi_select) begin
        tx_rst_n <= #DELAY 1'b1;
        tx_flg   <= #DELAY 1'b0;
     end
     else begin
        tx_rst_n <= #DELAY 1'b1;
        tx_flg   <= #DELAY tx_flg;
     end

   
   //-------------------------------------------------------------------------
   //
   //  Wishbone-like interface.
   //  
   //-------------------------------------------------------------------------
   wire [7:0] dat_w;
//    wire       rx_pull  = !rx_empty && bus_state == `BUS_WAIT;
   wire       rx_pull = cyc_o && !rx_empty && bus_state != `BUS_SEND;
   wire       tx_push = cyc_o && !we_o && ack_i;
   reg [2:0]  bus_state = `BUS_IDLE;

   assign bst_o = 1'b0;

   always @(posedge clk_i)
     if (rst_i)
       bus_state <= #DELAY `BUS_IDLE;
     else
       case (bus_state)
         `BUS_IDLE:
           if (spi_select && dat_req)
             bus_state <= #DELAY `BUS_READ;
           else if (spi_select && !rx_empty)
             bus_state <= #DELAY `BUS_SEND;
           else
             bus_state <= #DELAY bus_state;
         
         `BUS_READ:
           bus_state <= #DELAY !spi_select ? `BUS_IDLE : ack_i ? `BUS_WAIT : bus_state;

         `BUS_WAIT:
           bus_state <= #DELAY !rx_empty ? `BUS_SEND : bus_state;

         `BUS_SEND:
           bus_state <= #DELAY ack_i ? (dat_req ? `BUS_READ : `BUS_IDLE) : bus_state;
       endcase // case (bus_state)

   // TODO: Better prefetching, and don't use `rx_empty`?
   always @(posedge clk_i)
     if (rst_i) begin
        stb_o <= #DELAY 0;
        we_o  <= #DELAY 0;
     end
     else
       case (bus_state)
         `BUS_IDLE:             // Prefetch a byte on SPI start
           if (dat_req && spi_select) begin
              $display("%10t: TARGET -- Requesting status byte", $time);
              stb_o <= #DELAY 1;
              we_o  <= #DELAY 0;
           end
           else if (!spi_select) begin
              stb_o <= #DELAY 0;
              we_o  <= #DELAY 0;
           end
           else if (!rx_empty) begin
              stb_o <= #DELAY 1;
              we_o  <= #DELAY 1;
           end
         
         `BUS_READ:             // Wait for prefetched byte
`ifdef __WB_CLASSIC
           stb_o <= #DELAY ~ack_i;
`else
           stb_o <= #DELAY 0; // !stb_o && !ack_i && spi_select;
`endif //  __WB_CLASSIC

         `BUS_WAIT: begin       // Send any received data
            stb_o <= #DELAY ~rx_empty;
            we_o  <= #DELAY ~rx_empty;
         end

         `BUS_SEND: begin       // Wait for sent data to be acknowledged
`ifdef __WB_CLASSIC
            stb_o <= #DELAY ~ack_i;
`else
            stb_o <= #DELAY ack_i && dat_req;
`endif //  __WB_CLASSIC
            we_o  <= #DELAY ~ack_i;
         end
       endcase // case (bus_state)

   // Dequeue the data to be sent over the system bus.
   always @(posedge clk_i)
     if (rx_pull) dat_o <= #DELAY dat_w;
     else         dat_o <= #DELAY dat_o;


   //-------------------------------------------------------------------------
   //
   //  SPI interface.
   //  
   //-------------------------------------------------------------------------
   wire [7:0] rx_data  = {rx_reg[6:0], MOSI};
   wire [7:0] tx_data;
   reg [6:0]  tx_reg = HEADER_BYTE[6:0];
   reg [6:0]  rx_reg;
   reg [2:0]  rx_count = 0, tx_count = 0;
   reg        tx_pull = 0;
   wire       tx_next  = tx_count == 7;
   wire       rx_push  = rx_count == 7;

   //-------------------------------------------------------------------------
   //  Data-capture is on positive edges.
   //-------------------------------------------------------------------------
   // Deserialise the incoming SPI data, receiving MSB -> LSB.
   always @(posedge SCK)
     rx_reg <= #DELAY {rx_reg[5:0], MOSI};

   always @(posedge SCK or posedge SSEL)
     if (SSEL) rx_count <= #DELAY 0;
     else      rx_count <= #DELAY rx_count + 1;

   // RX FIFO overflow detection.
   always @(posedge SCK or posedge rst_i)
     if (rst_i)
       overflow_o  <= #DELAY 0;
     else if (rx_push && rx_full)
       overflow_o  <= #DELAY 1;

   //-------------------------------------------------------------------------
   //  Transmission logic.
   //-------------------------------------------------------------------------
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
     if (SSEL) tx_count <= #DELAY 0;
     else      tx_count <= #DELAY tx_count + 1;

   // Add a cycle of delay, avoiding an additional read just before a SPI
   // transaction completes.
   always @(`TX_EDGE SCK or posedge SSEL)
     if (SSEL) tx_pull <= #DELAY 0;
     else      tx_pull <= #DELAY tx_next && !tx_empty;

   // TX FIFO underrun detection.
   always @(`TX_EDGE SCK)
     if (rst_i)
       underrun_o <= #DELAY 0;
     else if (tx_next && tx_empty)
       underrun_o <= #DELAY 1;


   //-------------------------------------------------------------------------
   //
   //  Asynchronous FIFO's for transmitting and receiving.
   //
   //-------------------------------------------------------------------------
   afifo16 #( .WIDTH(8) ) TX_FIFO0
     ( .reset_ni (tx_rst_n),
       
       .rd_clk_i (SCK_tx),
       .rd_en_i  (tx_pull),
       .rd_data_o(tx_data),
       
       .wr_clk_i (clk_i),
//        .wr_en_i  (rdy_i),
       .wr_en_i  (tx_push),
       .wr_data_i(dat_i),

       .rempty_o (tx_empty),
       .wfull_o  (tx_full)
       );

   // TODO: Not useful? Just use a synchroniser?
   afifo16 #( .WIDTH(8) ) RX_FIFO0
     ( .reset_ni (!rst_i),
       
       .rd_clk_i (clk_i),
       .rd_en_i  (rx_pull),
       .rd_data_o(dat_w),
       
       .wr_clk_i (SCK),
       .wr_en_i  (rx_push),
       .wr_data_i(rx_data),

       .rempty_o (rx_empty),
       .wfull_o  (rx_full)
       );

   
endmodule // spi_target
