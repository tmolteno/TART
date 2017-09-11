`timescale 1ns/100ps
/*
 * Module      : verilog/tart_wishbone.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
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
 * Time-multiplexed ones-counter functional unit.
 * 
 * NOTE:
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as multi-
 *    port SRAM's are used;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 * 
 */

`include "tartcfg.v"

// Bus transaction states.
`define BUS_IDLE 0
`define BUS_READ 1
`define BUS_WAIT 2

module ones_count
  #( parameter ACCUM = `ACCUM_BITS,
     parameter MSB   = ACCUM-1,
     parameter MRATE = 12,       // Time-multiplexing rate
     parameter MBITS = 4,
     parameter MSIZE = (2<<MBITS)-1,
     parameter DELAY = 3)
   (
    input              clk_x,   // correlator clock
    input              rst,

    // Wishbone-like bus interface for reading back the counts.
    input              clk_i,   // bus clock
    input              cyc_i,
    input              stb_i,
    input              we_i,    // writes are ignored
    input              bst_i,   // burst-mode transfer?
    output             ack_o,
    input [4:0]        adr_i,
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o,

    // The real component of the signal from the antennas.
    input              enable,  // data acquisition is active
    input              switch,  // switch banks
    input [23:0]       antenna  // the real component from each antenna
    );


   //-------------------------------------------------------------------------
   //  Distributed RAM for the accumulators.
   //-------------------------------------------------------------------------
   // Should use RAM32M primitives (on Xilinx FPGA's).
   reg [MSB:0]         counts0[0:MSIZE];
   reg [MSB:0]         counts1[0:MSIZE];

   reg [MSB:0]         dcnt0, dcnt1, qcnt0, qcnt1;
   reg                 bank = 0, swap = 0, clear = 0;
   reg [3:0]           x_rd_adr = 0;
   reg [4:0]           x_wt_adr = 0, x_wr_adr = 0;
   wire                wrap_x_rd_adr = x_rd_adr == 11;
   wire [4:0]          next_x_rd_adr = wrap_x_rd_adr ? 0 : x_rd_adr + 1 ;

   // Pipelined accumulator requires cycles for:
   //   { read, accumulate, write } .
   always @(posedge clk_x)
     if (rst) begin
        x_rd_adr <= #DELAY 0;
        x_wt_adr <= #DELAY 0;
        x_wr_adr <= #DELAY 0;
     end
     else begin
        x_rd_adr <= #DELAY enable ? next_x_rd_adr : x_rd_adr;
        x_wt_adr <= #DELAY active ? {bank, x_rd_adr} : x_wt_adr;
        x_wr_adr <= #DELAY valid  ? x_wt_adr : x_wr_adr;
     end

   // Banks are switched at the next address-wrap event.
   always @(posedge clk_x)
     if (rst) begin : RAM_RESET_LOGIC
        swap  <= #DELAY 0;
        bank  <= #DELAY 0;
        clear <= #DELAY 1;
     end
     else if (wrap_x_rd_adr && (switch || swap)) begin // swap banks
        swap  <= #DELAY 0;
        bank  <= #DELAY ~bank;
        clear <= #DELAY 1;
     end
     else if (switch && !swap) begin // swap banks @next wrap
        swap  <= #DELAY 1;
     end
     else if (enable && wrap_x_rd_adr && clear) begin // finished restarting counters
        clear <= #DELAY 0;
     end

   // Read and write RAM contents for the correlator.
   always @(posedge clk_x) begin : RAM_READ_WRITE
     if (!rst && enable) begin
        dcnt0 <= #DELAY clear ? 0 : counts0[{bank, x_rd_adr}] ;
        dcnt1 <= #DELAY clear ? 0 : counts1[{bank, x_rd_adr}] ;
     end
     if (!rst && valid) begin
        counts0[x_wr_adr] <= #DELAY qcnt0;
        counts1[x_wr_adr] <= #DELAY qcnt1;
     end
   end


   //-------------------------------------------------------------------------
   //  Bus interface logic.
   //-------------------------------------------------------------------------
   wire [4:0] adr_w = {bank_n, adr_i[3:0]};
   reg [1:0]  bus_state = `BUS_IDLE;
   reg        bank_n = 1;

   assign ack_o = bus_state[0];

   always @(posedge clk_i)
     if (rst)
       bus_state <= #DELAY `BUS_IDLE;
     else
       case (bus_state)
         `BUS_IDLE:
           bus_state <= #DELAY cyc_i && stb_i ? `BUS_READ : bus_state ;

         `BUS_READ:
           bus_state <= #DELAY bst_i ? bus_state : `BUS_WAIT ;

         `BUS_WAIT:
           bus_state <= #DELAY cyc_i ? (stb_i ? `BUS_READ : bus_state) : `BUS_IDLE ;
       endcase // case (bus_state)

   // Synchronise the number of the innactive bank across domains.
   always @(posedge clk_i)
     if (rst) bank_n <= #DELAY 1;
     else     bank_n <= #DELAY ~bank;
   
   // Read only from the innactive bank.
   always @(posedge clk_i)
     if (cyc_i && stb_i)
       dat_o <= #DELAY adr_i[4] ? counts1[adr_w] : counts0[adr_w];


   //-------------------------------------------------------------------------
   //  Time-multiplexed accumulator logic.
   //-------------------------------------------------------------------------
   wire [11:0] ax0 = antenna[11: 0];
   wire [11:0] ax1 = antenna[23:12];
   reg         active = 0, valid = 0, a0, a1;

   // Latch antenna data as it arrives.
   always @(posedge clk_x) begin
      a0  <= #DELAY ax0[x_rd_adr];
      a1  <= #DELAY ax1[x_rd_adr];
   end

   always @(posedge clk_x)
     if (rst || !enable) active <= #DELAY 0;
     else if (enable)    active <= #DELAY 1;
     else                active <= #DELAY active;

   // Accumulate the ones, two per cycle.
   always @(posedge clk_x) begin
      qcnt0 <= #DELAY dcnt0 + a0;
      qcnt1 <= #DELAY dcnt1 + a1;
      valid <= #DELAY rst ? 0 : active;
   end


endmodule // ones_count
