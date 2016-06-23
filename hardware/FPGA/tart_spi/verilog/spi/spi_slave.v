`timescale 1ns/1ps
/*
 *
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
 * This SPI slave module wraps `spi_target` to give address support to the
 * (pipelined Wishbone-like) bus interface.
 * 
 * NOTE:
 *  + XST synthesis achieves aboout 250 MHz on a Spartan VI;
 *  + currently, the SCK frequency has to be slightly lower than that of the
 *    bus clock's, or else data isn't available early enough, resulting in a
 *    FIFO underrun;
 * 
 * Changelog:
 *  + 18/06/2016  --  initial file;
 * 
 * TODO:
 *  + the design is still preliminary (as of 24/05/2016);
 *  + constrain the input and output OFFSET's;
 *  + can the output-delay be run-time configurable?
 *  + parameterise the delay for read-requests?
 * 
 */

`define SPI_IDLE 0
`define SPI_ADDR 1
`define SPI_BUSY 2
`define SPI_PUSH 4
`define SPI_PULL 8

// Support CLASSIC Wishbone-like bus transactions?
`define __WB_CLASSIC
// `undef __WB_CLASSIC

// `define __USE_FAST_ACKS
`undef  __USE_FAST_ACKS

// `define __USE_SPI_LAYER

module spi_slave
  #( parameter WIDTH = 8,       // TODO: currently must be `8`!
     parameter MSB   = WIDTH-1,
     parameter ASB   = WIDTH-2,
     parameter HEADER_BYTE  = 8'hA7, // Pattern to send as the first byte
     parameter DELAY = 3)
   ( // Wishbone-like (bus master) interface:
     input              clk_i,
     input              rst_i,
     output reg         cyc_o = 0,
     output reg         stb_o = 0,
     output reg         bst_o = 0,
     output             we_o,
     input              ack_i,
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

   reg                  we = 0;

`ifdef __icarus
   assign we_o     = cyc_o && we; // more convenient in GtkWave
`else
   assign we_o = we;
`endif // __icarus


   //-------------------------------------------------------------------------
   //  Target interface logic.
   wire                 t_cyc, t_stb, t_bst, t_we;
   wire [MSB:0]         t_drx;
`ifdef __USE_FAST_ACKS
//    wire                 t_ack = t_stb || b_pend;
   wire                 t_ack = t_stb;
   wire [MSB:0]         t_dtx;
`else
   reg                  t_ack = 0;
   reg [MSB:0]          t_dtx;
`endif
                  
   assign active_o = t_cyc;


   //-------------------------------------------------------------------------
   //  SPI to Wishbone state-machine.
   //-------------------------------------------------------------------------
   reg [3:0]            spi = `SPI_IDLE;
   wire                 b_we = t_drx[MSB];
   wire [3:0]           mode = b_we ? `SPI_BUSY : `SPI_PULL;

`ifdef __USE_FAST_ACKS
   wire                 b_push = t_stb && t_we;
`else
   wire                 b_push = t_stb && t_we && !t_ack;
`endif
   reg                  b_pend = 0;
   wire                 addr = t_cyc && t_stb && t_we;
`ifndef __USE_SPI_LAYER
   wire                 b_xfer = spi != `SPI_IDLE && t_stb && t_we;
   wire                 b_pull = t_cyc && t_stb && t_we && !b_we;


   //-------------------------------------------------------------------------
   //  FSM for the Wishbone-like bus (master) interface.
   always @(posedge clk_i)
     if (rst_i || !t_cyc) spi <= #DELAY `SPI_IDLE;
     else
       case (spi)
         // new SPI transaction beginning:
         `SPI_IDLE: spi <= #DELAY t_cyc ? `SPI_ADDR : spi;

         // first byte from SPI is the write-mode and address:
         `SPI_ADDR: spi <= #DELAY t_we && t_ack ? mode : spi;

         `SPI_BUSY:
           if (t_cyc && t_stb) begin // `spi_target` wants to do something
              if (we && t_we)
                spi <= #DELAY `SPI_PUSH; // push data onto the WB bus
              else if (!we && t_we)      // prefetch
                spi <= #DELAY `SPI_PULL; // pull data from the WB bus
              else                       // ignore reads when in write-mode,
                spi <= #DELAY spi;       // and writes when in read-mode
           end

         `SPI_PUSH: spi <= #DELAY ack_i ? `SPI_BUSY : spi;
         `SPI_PULL: spi <= #DELAY !t_we && t_ack ? `SPI_BUSY : spi;
       endcase // case (spi)

   //  Generate the Wishbone-like flow-control signals.
   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o} <= #DELAY 2'b00;
     else if (t_cyc)
       case (spi)
`ifdef __WB_CLASSIC
         `SPI_BUSY: {cyc_o, stb_o} <= #DELAY {b_xfer, b_xfer};
         `SPI_ADDR: {cyc_o, stb_o} <= #DELAY {b_pull, b_pull};
         `SPI_PULL: {cyc_o, stb_o} <= #DELAY {cyc_o && !ack_i, cyc_o && !ack_i};
         `SPI_PUSH: {cyc_o, stb_o} <= #DELAY {!ack_i, !ack_i};
`else
         `SPI_BUSY: {cyc_o, stb_o} <= #DELAY {b_xfer || cyc_o && !ack_i, b_xfer};
         `SPI_ADDR: {cyc_o, stb_o} <= #DELAY {b_pull || cyc_o && !ack_i, b_pull};
         `SPI_PULL: {cyc_o, stb_o} <= #DELAY {cyc_o && !ack_i, 1'b0};
         `SPI_PUSH: {cyc_o, stb_o} <= #DELAY {!ack_i, 1'b0};
`endif
       endcase // case (spi)
     else
       {cyc_o, stb_o} <= #DELAY 2'b00;

`ifndef __USE_FAST_ACKS
   always @(posedge clk_i)
     if (rst_i)
       t_ack <= #DELAY 1'b0;
     else if (t_cyc && !t_ack)
       case (spi)
         `SPI_PULL: t_ack <= #DELAY (b_pend || ack_i) && t_stb && !t_we;
         default:   t_ack <= #DELAY t_stb;
       endcase // case (spi)
     else
       t_ack <= #DELAY 1'b0;
`endif //  `ifndef __USE_FAST_ACKS

   //  Data for the next SPI transfer is prefetched, so mark this as "pending"
   //  until the SPI target requests it.
// `define __USE_FAST_WRITES
`ifdef __USE_FAST_WRITES
   always @(posedge clk_i)
     if (rst_i || !t_cyc)
       b_pend <= #DELAY 0;
     else if (spi == `SPI_ADDR)
       b_pend <= #DELAY b_pend ? 1'b0 : t_stb && !t_we;
     else if (spi == `SPI_PULL)
       b_pend <= #DELAY b_pend ? 1'b0 : !we && ack_i;
     else
       b_pend <= #DELAY 0;

`else // !`ifdef __USE_FAST_WRITES
   always @(posedge clk_i)
     if (rst_i || !t_cyc || t_stb && !t_we)
       b_pend <= #DELAY 0;
     else if (spi == `SPI_PULL)
       b_pend <= #DELAY b_pend ? b_pend : !we && ack_i;
     else
       b_pend <= #DELAY 0;
`endif //  !`ifdef __USE_FAST_WRITES

   //  Latch incoming data, until needed by `spi_target`.
`ifdef __USE_FAST_ACKS
   assign t_dtx = spi == `SPI_PULL ? dat_i : status_i;
`else
   always @(posedge clk_i)
     if (rst_i || !t_cyc)
       t_dtx <= #DELAY status_i;
     else if (t_cyc && !we && spi == `SPI_PULL && ack_i)
       t_dtx <= #DELAY dat_i;
`endif

   // `we` stores the last-requested bus read#/write mode.
   always @(posedge clk_i)
     if (rst_i) we <= #DELAY 1'b0;
`ifdef __USE_FAST_ACKS
     else if (t_cyc && t_stb && t_we)
`else
     else if (t_cyc && t_stb && t_we && !t_ack)
`endif
       case (spi)
         `SPI_ADDR: {we, adr_o} <= #DELAY {t_drx[MSB], t_drx[ASB:0]};
         default:   {we, adr_o} <= #DELAY {we, adr_o};
       endcase // case (spi)

   always @(posedge clk_i)
`ifdef __USE_FAST_ACKS
     if (t_cyc && t_stb && t_we)
`else
     if (t_cyc && t_stb && t_we && !t_ack)
`endif
       dat_o <= #DELAY t_drx;


   //-------------------------------------------------------------------------
   //  SPI target core instance.
   //-------------------------------------------------------------------------
   spi_target #( .HEADER_BYTE(HEADER_BYTE) ) SPI_TARGET0
     ( .clk_i(clk_i),
       .rst_i(rst_i),
       .cyc_o(t_cyc),
       .stb_o(t_stb),
       .bst_o(t_bst),
       .we_o (t_we),
       .ack_i(t_ack),
       .rdy_i(b_pend),
       .dat_i(t_dtx),
       .dat_o(t_drx),

       .overflow_o(overflow_o),
       .underrun_o(underrun_o),
       
       .SCK_pin(SCK_pin),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );
`endif

   //-------------------------------------------------------------------------
   //  Lower-level SPI-layer wrapper.
   //  TODO:
   wire [7:0] l_drx;
   wire       l_cyc, l_get, l_wat, l_overflow, l_underrun, l_MISO;
   reg        l_ack = 0;

`ifdef __USE_SPI_LAYER
   //-------------------------------------------------------------------------
   //  FSM for the Wishbone-like bus (master) interface.
   always @(posedge clk_i)
     if (rst_i || !l_cyc) spi <= #DELAY `SPI_IDLE;
     else
       case (spi)
         // new SPI transaction beginning:
         `SPI_IDLE: spi <= #DELAY l_cyc ? `SPI_ADDR : spi;

         // first byte from SPI is the write-mode and address:
         `SPI_ADDR: spi <= #DELAY !l_wat && l_ack ? mode : spi;

         `SPI_BUSY:
           if (l_cyc) begin
              if (l_get && !we)
                spi <= #DELAY `SPI_PULL; // pull data from the WB bus
              else if (!l_wat && we)
                spi <= #DELAY `SPI_PUSH; // push data onto the WB bus
              else                       // ignore reads when in write-mode,
                spi <= #DELAY spi;       // and writes when in read-mode
           end

         `SPI_PUSH: spi <= #DELAY ack_i ? `SPI_BUSY : spi;
         `SPI_PULL: spi <= #DELAY ack_i ? `SPI_BUSY : spi;
       endcase // case (spi)

   wire b_xfer = l_get || !l_wat;
   wire b_pull = !l_wat && !l_drx[MSB];

   //  Generate the Wishbone-like flow-control signals.
   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o} <= #DELAY 2'b00;
     else if (l_cyc)
       case (spi)
`ifdef __WB_CLASSIC
         `SPI_BUSY: {cyc_o, stb_o} <= #DELAY {b_xfer, b_xfer};
         `SPI_ADDR: {cyc_o, stb_o} <= #DELAY {b_pull, b_pull};
         `SPI_PULL: {cyc_o, stb_o} <= #DELAY {cyc_o && !ack_i, cyc_o && !ack_i};
         `SPI_PUSH: {cyc_o, stb_o} <= #DELAY {!ack_i, !ack_i};
`else
         `SPI_BUSY: {cyc_o, stb_o} <= #DELAY {b_xfer || cyc_o && !ack_i, b_xfer};
         `SPI_ADDR: {cyc_o, stb_o} <= #DELAY {b_pull || cyc_o && !ack_i, b_pull};
         `SPI_PULL: {cyc_o, stb_o} <= #DELAY {cyc_o && !ack_i, 1'b0};
         `SPI_PUSH: {cyc_o, stb_o} <= #DELAY {!ack_i, 1'b0};
`endif
       endcase // case (spi)
     else
       {cyc_o, stb_o} <= #DELAY 2'b00;

   // `we` stores the last-requested bus read#/write mode.
   always @(posedge clk_i)
     if (rst_i) we <= #DELAY 1'b0;
     else if (l_cyc && !l_wat && !l_ack)
       case (spi)
         `SPI_ADDR: {we, adr_o} <= #DELAY {l_drx[MSB], l_drx[ASB:0]};
         default:   {we, adr_o} <= #DELAY {we, adr_o};
       endcase // case (spi)

   always @(posedge clk_i)
     if (rst_i || !l_cyc)
       l_ack <= #DELAY 0;
     else if (!l_wat && !l_ack) begin
        if (spi == `SPI_PUSH)
          l_ack <= #DELAY ack_i;
        else if (spi == `SPI_BUSY && !we)
          l_ack <= #DELAY 1;
        else
          l_ack <= #DELAY 0;
     end
     else
       l_ack <= #DELAY 0;

   wire [MSB:0] l_dtx = dat_i;
   wire         l_rdy = cyc_o && !we_o && ack_i;
`endif //  `ifdef __USE_SPI_LAYER


`ifndef __USE_SPI_LAYER
   reg        l_rdy = 0;
   reg [7:0]  l_dtx;

   //-------------------------------------------------------------------------
   //  `spi_layer` control signals.
   always @(posedge clk_i)
      if (l_cyc && l_get && !l_rdy) begin
         l_rdy <= #DELAY 1;
         l_dtx <= #DELAY $random;
      end
      else
        l_rdy <= #DELAY 0;

   always @(posedge clk_i)
     if (!l_wat && l_ack)
       $display("%10t: LAYER data = %02x", $time, l_drx);

   always @(posedge clk_i)
     if (rst_i || !l_cyc)
       l_ack <= #DELAY 0;
     else if (!l_wat && !l_ack)
       l_ack <= #DELAY 1;
     else
       l_ack <= #DELAY 0;
`endif


   spi_layer #( .HEADER_BYTE(HEADER_BYTE) ) SPI_LAYER0
     ( .clk_i(clk_i),
       .rst_i(rst_i),
       .cyc_o(l_cyc),
       .get_o(l_get),
       .rdy_i(l_rdy),
       .wat_o(l_wat),
       .ack_i(l_ack),
       .dat_i(l_dtx),
       .dat_o(l_drx),

       .overflow_o(l_overflow),
       .underrun_o(l_underrun),

       .SCK_pin(SCK_pin),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(l_MISO)
       );

   
endmodule // spi_slave
