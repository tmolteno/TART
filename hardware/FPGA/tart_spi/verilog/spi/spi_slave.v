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


   //-------------------------------------------------------------------------
   //  SPI to Wishbone state-machine.
   //-------------------------------------------------------------------------
   wire [7:0]           l_drx;
   reg [3:0]            spi = `SPI_IDLE;
   wire                 b_we = l_drx[MSB];
   wire [3:0]           mode = b_we ? `SPI_BUSY : `SPI_PULL;
   wire                 l_cyc, l_get, l_wat;
   wire                 l_put = ~l_wat;
   reg                  we = 0, l_ack = 0;


`ifdef __icarus
   assign we_o     = cyc_o && we; // more convenient in GtkWave
`else
   assign we_o = we;
`endif // __icarus
   assign active_o = l_cyc;


   //-------------------------------------------------------------------------
   //  FSM for the Wishbone-like bus (master) interface.
   always @(posedge clk_i)
     if (rst_i || !l_cyc)
       spi <= #DELAY `SPI_IDLE;
     else
       case (spi)
         // new SPI transaction beginning?
         `SPI_IDLE: spi <= #DELAY l_get && r_rdy ? `SPI_ADDR : spi;

         // first byte from SPI is the write-mode and address?
         `SPI_ADDR: spi <= #DELAY l_put ? (b_we || !l_get ? `SPI_BUSY : `SPI_PULL) : spi;

         `SPI_BUSY: begin
            if (l_get && !we && !x_rdy)
              spi <= #DELAY `SPI_PULL; // pull data from the WB bus
            else if (l_put && we && !l_ack)
              spi <= #DELAY `SPI_PUSH; // push data onto the WB bus
            else                       // ignore reads when in write-mode,
              spi <= #DELAY spi;       // and writes when in read-mode
         end

         `SPI_PUSH: spi <= #DELAY ack_i ? `SPI_BUSY : spi;
         `SPI_PULL: spi <= #DELAY ack_i ? `SPI_BUSY : spi;
       endcase // case (spi)

   wire                 a_pull = l_get && !b_we && !r_rdy;

   wire                 b_xfer = b_pull || b_push;
   wire                 b_pull = l_get && !we && !r_rdy;
   wire                 b_push = l_put &&  we && !l_ack;

   //  Generate the Wishbone-like flow-control signals.
   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o} <= #DELAY 2'b00;
     else if (l_cyc)
       case (spi)
         `SPI_IDLE: {cyc_o, stb_o} <= #DELAY 2'b00;
`ifdef __WB_CLASSIC
         `SPI_ADDR: {cyc_o, stb_o} <= #DELAY {a_pull, a_pull};
         `SPI_BUSY: {cyc_o, stb_o} <= #DELAY {b_xfer, b_xfer};
         `SPI_PULL: {cyc_o, stb_o} <= #DELAY {cyc_o && !ack_i, cyc_o && !ack_i};
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

   // `we` stores the last-requested bus read#/write mode.
   always @(posedge clk_i)
     if (rst_i) we <= #DELAY 1'b0;
     else if (l_put && !l_ack)
       case (spi)
         `SPI_ADDR: {we, adr_o} <= #DELAY {l_drx[MSB], l_drx[ASB:0]};
         default:   {we, adr_o} <= #DELAY {we, adr_o};
       endcase // case (spi)

   always @(posedge clk_i)
     if (rst_i || !l_cyc)
       l_ack <= #DELAY 0;
     else if (l_put && !l_ack)
       case (spi)
         `SPI_ADDR: l_ack <= #DELAY 1;
         `SPI_BUSY: l_ack <= #DELAY 1; // !we;
         `SPI_PUSH: l_ack <= #DELAY ack_i;
       endcase // case (spi)
     else
       l_ack <= #DELAY 0;


   //-------------------------------------------------------------------------
   //  Set up the readies so that WB ACK's fall right through, without
   //  being registered first.
   wire [MSB:0] w_dtx = spi == `SPI_IDLE || spi == `SPI_ADDR ? status_i : dat_i;
   reg [MSB:0]  l_dtx;
   reg          r_rdy = 0, r_wat = 0;
   wire         w_rdy = rdy_status || rdy_ignore;
   wire         x_rdy = r_rdy || r_wat && ack_i;
   wire         rdy_status = spi == `SPI_IDLE && l_get;
   wire         rdy_ignore = l_get && (b_we && spi == `SPI_ADDR || we && spi == `SPI_BUSY);

   always @(posedge clk_i)
     if (rst_i || r_rdy)
       r_rdy <= #DELAY 0;
     else
       {r_rdy, l_dtx} <= #DELAY {w_rdy, r_wat && ack_i ? w_dtx : l_dtx};

   always @(posedge clk_i)
     if (rst_i || r_rdy) r_wat <= #DELAY 0;
     else                r_wat <= #DELAY cyc_o && !we && !ack_i;

   //  TODO: Unnecessary?
   always @(posedge clk_i)
     if (l_put && we && !l_ack && spi == `SPI_BUSY)
       dat_o <= #DELAY l_drx;


   //-------------------------------------------------------------------------
   //  SPI-layer, and the domain-crossing subcircuits, of the interface.
   //-------------------------------------------------------------------------
   spi_layer #( .WIDTH(WIDTH), .HEADER_BYTE(HEADER_BYTE) ) SPI_LAYER0
     ( .clk_i(clk_i),
       .rst_i(rst_i),
       .cyc_o(l_cyc),
       .get_o(l_get),
//        .rdy_i(l_rdy),
       .rdy_i(x_rdy),
       .wat_o(l_wat),
       .ack_i(l_ack),
//        .dat_i(l_dtx),
       .dat_i(w_dtx),
       .dat_o(l_drx),

       .overflow_o(overflow_o),
       .underrun_o(underrun_o),

       .SCK_pin(SCK_pin),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );

   
endmodule // spi_slave
