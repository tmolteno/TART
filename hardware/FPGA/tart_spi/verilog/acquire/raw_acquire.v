`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/raw_acquire.v
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
 * Capture raw antenna signals, buffering them in a Spartan 6 SRAM, while
 * streaming to an off-chip SDRAM.
 * 
 * NOTE:
 *  + the `x` suffix is used to tag signals of the (12x by default) sampling/
 *    correlator clock-domain;
 *  + even though the sample-clock's frequency is an integer multiple of the
 *    external clock, the phase relationship is unknown (due to the quirky
 *    Spartan 6 DCM's), thus asynchronous domain-crossing techniques must be
 *    used;
 * 
 * TODO:
 *  + replace the kinda-FIFO stuff with an actual asynchronous FIFO;
 * 
 */

`include "tartcfg.v"

module raw_acquire
  #(//  Bit-width parameters:
    parameter AXNUM = 24,       // number of antennae
    parameter MSB   = AXNUM-1,  // MSB of antenna signal
    parameter ABITS = 20,       // DRAM controller address bit-width
//     parameter ASB   = ABITS-2,  // TODO: MSB of DRAM address??
    parameter ASB   = ABITS-1,  // MSB of DRAM address

    //  Additional settings:
    parameter RESET = 0,        // enable reset-to-zero (0/1)?

    //  Simulation-only parameters:
    parameter DELAY = 3)
   (
    input          clock_i, // bus clock
    input          reset_i, // bus-domain reset

    //  Module control-signals:
    input          capture_i, // enable raw-data capture (0/1)
    input          request_i, // retrieve sample from SDRAM

    //  Raw signal data input:
    //  NOTE: Bus clock-domain (6x, DDR oversampled).
    input          locked_i,
    input          strobe_i, // bus-domain signal-strobe
    input          middle_i, // bus-domain mid-signal strobe
    input [MSB:0]  signal_i, // supersampled & aligned antenna signals

    //  Memory Controller Block (MCB) signals:
    output         mcb_ce_o,
    output         mcb_wr_o,
    input          mcb_rdy_i,
    output [ASB:0] mcb_adr_o,
    output [31:0]  mcb_dat_o,

    //  Debug/info signal outputs:
    output         oflow_o, // FIFO overflow?
    output [2:0]   state_o      // just for debug info
    );


`ifdef __USE_ACQUISITION
   //-------------------------------------------------------------------------
   //  Acquistion-block signals:
   //  TODO: Which domains do these belong to?
   wire [MSB:0]    rdata;
   wire [8:0]      raddr, waddr;


   //-------------------------------------------------------------------------
   //  Xilinx block SRAM for temporary buffering.
   //-------------------------------------------------------------------------
   //  NOTE: Functions as a FIFO, but with the control-logic spread between
   //    this and the 'fifo_sdram_fifo_scheduler' modules.
   block_buffer AQ_BB
     ( // read port:
       .read_clock_i   (clock_i),
       .read_address_i (raddr),
       .read_data_o    (rdata),
       // write port:
       .write_clock_i  (clock_i),
       .write_enable_i (middle_i),
       .write_address_i(waddr),
       .write_data_i   (signal_i)
       );


   //-------------------------------------------------------------------------
   //  Storage block controller.
   //-------------------------------------------------------------------------
   //  NOTE: Bus-clock domain.
   fifo_control
     #(.SDRAM_ADDRESS_WIDTH(ABITS+1))
   SCHEDULER0
     ( .clock_i (clock_i),           // bus-clock
       .reset_i (reset_i),           // global reset (bus-domain)
       .enable_i(capture_i),
       .strobe_i(strobe_i),

       .bb_rd_adr_o(raddr),
       .bb_wr_adr_o(waddr),
       .bb_rd_dat_i(rdata),

       .aq_rd_req_i(request_i),

       .cmd_request(mcb_ce_o),
       .cmd_write  (mcb_wr_o),
       .cmd_waiting(mcb_rdy_i),
       .cmd_address(mcb_adr_o),
       .cmd_data_in(mcb_dat_o),

       .overflow_o (oflow_o),
       .aq_state_o (state_o)
       );


`else // !`ifdef __USE_ACQUISITION
   //-------------------------------------------------------------------------
   //  Just sleep the Memory Controller Block (MCB) if not using the raw-data
   //  acquisition block.
   //-------------------------------------------------------------------------
   assign mcb_ce_o  = 1'b0;
   assign mcb_wr_o  = 1'b0;
   assign mcb_adr_o = {(ABITS-1){1'b0}};
   assign mcb_dat_o = {32{1'b0}};

`endif // !`ifdef __USE_ACQUISITION


endmodule // raw_acquire
