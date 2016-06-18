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
 * 
 * Changelog:
 *  + 18/06/2016  --  initial file;
 * 
 * TODO:
 *  + the design is still preliminary (as of 24/05/2016);
 *  + constrain the input and output OFFSET's;
 *  + can the output-delay be run-time configurable?
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
     parameter HEADER_BYTE  = 8'hA7,
     parameter DELAY = 3) // Pattern to send as the first byte
   ( // Wishbone-like (bus master) interface:
     input              clk_i,
     input              rst_i,
     output reg         cyc_o = 0,
     output reg         stb_o = 0,
     output reg         bst_o = 0,
     output reg         we_o = 0,
     input              ack_i,
     output reg [ASB:0] adr_o,
     input [MSB:0]      dat_i,
     output reg [MSB:0] dat_o,
     
     // Debug/diagnostic output, for when the recieve FIFO overflows.
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
   wire [MSB:0]         t_dato;
   reg                  t_ack = 0;
   reg [MSB:0]          t_dati;


   //-------------------------------------------------------------------------
   //  SPI to Wishbone state-machine.
   //-------------------------------------------------------------------------
   reg [3:0]            spi = `SPI_IDLE;
   wire                 addr = t_cyc & t_stb & t_we;
   wire [3:0]           mode = t_dato[MSB] ? `SPI_WAIT : `SPI_PULL;
   wire                 bus_we = spi == `SPI_ADDR && t_dato[MSB];

   always @(posedge clk_i)
     if (rst_i || !t_cyc) spi <= #DELAY `SPI_IDLE;
     else
       case (spi)
         `SPI_IDLE: spi <= #DELAY t_cyc & t_stb ? `SPI_ADDR : spi;
         `SPI_ADDR: spi <= #DELAY addr ? mode : spi;
         `SPI_WAIT: spi <= #DELAY t_stb & t_we ? `SPI_PULL : spi;
         `SPI_PULL: spi <= #DELAY ack_i ? `SPI_WAIT : spi;
         `SPI_PUSH: spi <= #DELAY ack_i ? `SPI_WAIT : spi;
       endcase // case (spi)

   always @(posedge clk_i)
     if (rst_i) cyc_o <= #DELAY 1'b0;
     else if (t_cyc)
       case (spi)
         `SPI_ADDR: cyc_o <= #DELAY addr && !bus_we;
         `SPI_WAIT: cyc_o <= #DELAY t_stb && t_we;
         `SPI_PUSH: cyc_o <= #DELAY !ack_i;
       endcase // case (spi)
     else
       cyc_o <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (rst_i) stb_o <= #DELAY 1'b0;
     else if (t_cyc)
       case (spi)
         `SPI_ADDR: stb_o <= #DELAY addr && !bus_we;
         `SPI_WAIT: stb_o <= #DELAY t_stb && t_we;
         `SPI_PULL: stb_o <= #DELAY !ack_i; // 1'b0;
         `SPI_PUSH: stb_o <= #DELAY !ack_i;
       endcase // case (spi)
     else
       stb_o <= #DELAY 1'b0;

   always @(posedge clk_i)
     if (rst_i) t_ack <= #DELAY 1'b0;
     else if (t_cyc && t_stb && !t_we && !t_ack)
       case (spi)
         `SPI_PULL, `SPI_PUSH: {t_ack, t_dati} <= #DELAY {ack_i, dat_i};
         default:              {t_ack, t_dati} <= #DELAY {1'b1, status_i};
       endcase // case (spi)
     else if (t_cyc && t_stb && !t_ack)
       t_ack <= #DELAY 1'b1;
     else
       t_ack <= #DELAY 1'b0;

   // `we_o` stores the last-requested bus read#/write mode.
   always @(posedge clk_i)
     if (rst_i) we_o <= #DELAY 1'b0;
     else if (t_cyc && t_stb && t_we && !t_ack)
       case (spi)
         `SPI_ADDR: {we_o, adr_o} <= #DELAY {t_dato[MSB], t_dato[ASB:0]};
         default:   {we_o, adr_o} <= #DELAY {we_o, adr_o};
       endcase // case (spi)


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
       .dat_i(t_dati),
       .dat_o(t_dato),

       .overflow_o(overflow_o),
       .underrun_o(underrun_o),
       
       .SCK_pin(SCK_pin),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );

   
endmodule // spi_slave
