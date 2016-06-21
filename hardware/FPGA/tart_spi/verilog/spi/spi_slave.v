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
`define SPI_WAIT 2
`define SPI_PUSH 4
`define SPI_PULL 8

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

   //-------------------------------------------------------------------------
   //  Target interface logic.
   wire                 t_cyc, t_stb, t_bst, t_we;
   wire [MSB:0]         t_drx;
   reg                  t_ack = 0;
   reg [MSB:0]          t_dtx;
   reg                  we = 0;
                  
   assign active_o = t_cyc;
`ifdef __icarus
   assign we_o     = cyc_o && we; // more convenient in GtkWave
`else
   assign we_o = we;
`endif // __icarus


   //-------------------------------------------------------------------------
   //  SPI to Wishbone state-machine.
   //-------------------------------------------------------------------------
   reg [3:0]            spi = `SPI_IDLE;
   reg                  t_new = 1;
   wire                 addr = t_cyc & t_stb & t_we;
   wire [3:0]           mode = b_we ? `SPI_WAIT : `SPI_PULL;
   wire                 b_we = t_drx[MSB];
   wire                 b_xfer = !t_new && t_cyc && t_stb && t_we;
   wire                 b_wait = addr && !b_we;
   wire                 b_done = t_stb && t_we && !t_ack;
   reg                  b_pend = 0;

   always @(posedge clk_i)
     if (rst_i || !t_cyc) t_new <= #DELAY 1;
     else                 t_new <= #DELAY 0;

   always @(posedge clk_i)
     if (rst_i || !t_cyc) spi <= #DELAY `SPI_IDLE;
     else
       case (spi)
         `SPI_IDLE:
           if (t_cyc && t_stb) begin
              if (t_new)
                spi <= #DELAY `SPI_ADDR; // new transaction beginning
              else if (we && t_we)
                spi <= #DELAY `SPI_PUSH; // push data onto the WB bus
              else if (!we && t_we)      // prefetch
                spi <= #DELAY `SPI_PULL; // pull data from the WB bus
              else                       // ignore reads when in write-mode,
                spi <= #DELAY spi;       // and writes when in read-mode
           end

         `SPI_ADDR: spi <= #DELAY addr ? mode : spi;
         `SPI_WAIT: spi <= #DELAY b_done ? `SPI_PUSH : spi;
         `SPI_PULL: spi <= #DELAY !t_we && t_ack ? `SPI_IDLE : spi;
         `SPI_PUSH: spi <= #DELAY ack_i ? `SPI_IDLE : spi;
       endcase // case (spi)

   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o} <= #DELAY 2'b00;
     else if (t_cyc)
       case (spi)
         `SPI_IDLE: {cyc_o, stb_o} <= #DELAY {b_xfer, b_xfer};
         `SPI_ADDR: {cyc_o, stb_o} <= #DELAY {b_wait, b_wait};
         `SPI_WAIT: {cyc_o, stb_o} <= #DELAY {b_done, b_done};
         `SPI_PULL: {cyc_o, stb_o} <= #DELAY {cyc_o && !ack_i, 1'b0};
         `SPI_PUSH: {cyc_o, stb_o} <= #DELAY {!ack_i, 1'b0};
       endcase // case (spi)
     else
       {cyc_o, stb_o} <= #DELAY 2'b00;

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

   //  Data for the next SPI transfer is prefetched, so mark this as "pending"
   //  until the SPI target requests it.
   always @(posedge clk_i)
     if (rst_i || !t_cyc || t_stb && !t_we)
       b_pend <= #DELAY 0;
     else if (spi == `SPI_PULL)
       b_pend <= #DELAY b_pend ? b_pend : !we && ack_i;
     else
       b_pend <= #DELAY 0;

   always @(posedge clk_i)
     if (rst_i || !t_cyc)
       t_dtx <= #DELAY status_i;
     else if (t_cyc && !we && spi == `SPI_PULL && ack_i)
       t_dtx <= #DELAY dat_i;

   // `we` stores the last-requested bus read#/write mode.
   always @(posedge clk_i)
     if (rst_i) we <= #DELAY 1'b0;
     else if (t_cyc && t_stb && t_we && !t_ack)
       case (spi)
         `SPI_ADDR: {we, adr_o} <= #DELAY {t_drx[MSB], t_drx[ASB:0]};
         default:   {we, adr_o} <= #DELAY {we, adr_o};
       endcase // case (spi)

   always @(posedge clk_i)
     if (t_cyc && t_stb && t_we && !t_ack)
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
       .dat_i(t_dtx),
       .dat_o(t_drx),

       .overflow_o(overflow_o),
       .underrun_o(underrun_o),
       
       .SCK_pin(SCK_pin),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );

   
endmodule // spi_slave
