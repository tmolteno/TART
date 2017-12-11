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
 * TART's Wishbone bus-arbiter, address-decoder, and signal-router.
 * 
 * NOTE:
 *  + transfers are Wishbone SPEC B4 (by default);
 *  + when a bus only has a single master then the 'CYC' signals are not
 *    typically needed, and set 'CHECK = 0' to disable the 'CYC' logic (as
 *    'CYC' primarily signals to other masters that the bus is currently in
 *    use by another master);
 *  + bus wait-state requests ('WAT_O == 1') can't be pipelined, so these are
 *    upgraded to retry ('RTY_O == 1') responses.
 * 
 * TODO:
 * 
 */

`include "tartcfg.v"

module tart_wishbone
  #(//  Decoder settings:
    parameter XBITS = 2,        // number of MSB's to decode
    parameter XSB   = XBITS-1,  // MSB of decoder-bits
    parameter USB   = 6,        // upper bit for the address-decoder

    //  Capture-unit parameters:
    parameter CAPEN = 1,        // capture-unit enabled (0/1)?
    parameter CAPAD = 0,        // capture address
    parameter CREGS = 4,        // number of capture-unit registers
    parameter CBITS = 2,        // bit-width of capture-register addresses
    parameter CSB   = CBITS-1,  // MSB of capture-register addresses

    //  Acquisition-unit parameters:
    parameter ACQEN = 1,        // acquisition-unit enabled (0/1)?
    parameter ACQAD = 1,        // acquisition address
    parameter AREGS = 4,        // number of acquire-unit registers
    parameter ABITS = 2,        // bit-width of acquire-register addresses
    parameter ASB   = ABITS-1,  // MSB of acquire-register addresses

    //  DSP-unit parameters:
    parameter DSPEN = 1,        // DSP-unit enabled (0/1)?
    parameter DSPAD = 2,        // DSP address
    parameter DREGS = 4,        // number of DSP-unit registers
    parameter DBITS = 2,        // bit-width of DSP-register addresses
    parameter DSB   = DBITS-1,  // MSB of DSP-register addresses

    //  System-unit parameters:
    parameter SYSEN = 1,        // system-unit enabled (0/1)?
    parameter SYSAD = 3,        // system address
    parameter SREGS = 4,        // number of system-unit registers
    parameter SBITS = 2,        // bit-width of system-register addresses
    parameter SSB   = SBITS-1,  // MSB of system-register addresses

    //  Wisbone mode/settings:
    parameter ASYNC = 1,        // combinational (0/1)?
    parameter RESET = 1,        // enable fast-resets (0/1)?
    parameter CHECK = 1,        // bus-signal sanity-checking (0/1)?
    parameter PIPED = 1,        // WB pipelined transfers (0/1)?

    //  Simulation-only parameters:
    parameter NOISY = 0,        // display extra debug info?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input          bus_clk_i,   // bus clock
    input          bus_rst_i,   // bus-domain reset

    //  SPI/AXI (master) Wishbone (SPEC B4) interconnect:
    input          master_cyc_i,
    input          master_stb_i,
    input          master_we_i,
    output         master_ack_o,
    output         master_wat_o,
    output         master_rty_o,
    output         master_err_o,
    input [6:0]    master_adr_i,
    input [7:0]    master_dat_i,
    output [7:0]   master_dat_o,
    
    //  Capture (slave) Wishbone (SPEC B4) interconnect:
    output         cap_cyc_o,
    output         cap_stb_o,
    output         cap_we_o,
    input          cap_ack_i,
    input          cap_wat_i,
    input          cap_rty_i,
    input          cap_err_i,
    output [CSB:0] cap_adr_o,
    input [7:0]    cap_dat_i,
    output [7:0]   cap_dat_o,
    
    //  Acquisition (slave) Wishbone (SPEC B4) interconnect:
    output         acq_cyc_o,
    output         acq_stb_o,
    output         acq_we_o,
    input          acq_ack_i,
    input          acq_wat_i,
    input          acq_rty_i,
    input          acq_err_i,
    output [ASB:0] acq_adr_o,
    input [7:0]    acq_dat_i,
    output [7:0]   acq_dat_o,
    
    //  DSP (slave) Wishbone (SPEC B4) interconnect:
    output         dsp_cyc_o,
    output         dsp_stb_o,
    output         dsp_we_o,
    input          dsp_ack_i,
    input          dsp_wat_i,
    input          dsp_rty_i,
    input          dsp_err_i,
    output [ASB:0] dsp_adr_o,
    input [7:0]    dsp_dat_i,
    output [7:0]   dsp_dat_o,
    
    //  System (slave) Wishbone (SPEC B4) interconnect:
    output         sys_cyc_o,
    output         sys_stb_o,
    output         sys_we_o,
    input          sys_ack_i,
    input          sys_wat_i,
    input          sys_rty_i,
    input          sys_err_i,
    output [SSB:0] sys_adr_o,
    input [7:0]    sys_dat_i,
    output [7:0]   sys_dat_o
    );


   //-------------------------------------------------------------------------
   //  Internal signals and (optional, pipeline) registers.
   reg             bus_ack_r, bus_rty_r, bus_err_r;
   wire            bus_ack_w, bus_wat_w, bus_rty_w, bus_err_w;
   wire [7:0]      bus_dat_w, mux_dat_w;

   wire            master_cyc_w, master_stb_w;
   wire [XSB:0]    dec_adr_w;
   wire            cap_sel_w, acq_sel_w, dsp_sel_w, sys_sel_w;

   wire            cap_stb_w, acq_stb_w, dsp_stb_w, sys_stb_w;
   wire            cap_ack_w, acq_ack_w, dsp_ack_w, sys_ack_w;
   wire            cap_wat_w, acq_wat_w, dsp_wat_w, sys_wat_w;

   //-------------------------------------------------------------------------
   //  Pipeline registers (optional).
   reg             bus_cyc_r, bus_we_r;
   wire            bus_cyc_w, bus_we_w;
   wire [6:0]      bus_adr_w;
   reg [6:0]       bus_adr_r;
   reg [7:0]       bus_dat_r, mux_dat_r;
   reg             cap_stb_r, acq_stb_r, dsp_stb_r, sys_stb_r;
   reg [XSB:0]     mux_sel_r;


   //-------------------------------------------------------------------------
   //  Compute the device selects/strobes.
   //-------------------------------------------------------------------------
   assign cap_sel_w = CAPEN && master_stb_w && dec_adr_w == CAPAD;
   assign acq_sel_w = ACQEN && master_stb_w && dec_adr_w == ACQAD;
   assign dsp_sel_w = DSPEN && master_stb_w && dec_adr_w == DSPAD;
   assign sys_sel_w = SYSEN && master_stb_w && dec_adr_w == SYSAD;

   assign cap_stb_w = ASYNC == 2 ? cap_sel_w : cap_stb_r;
   assign acq_stb_w = ASYNC == 2 ? acq_sel_w : acq_stb_r;
   assign dsp_stb_w = ASYNC == 2 ? dsp_sel_w : dsp_stb_r;
   assign sys_stb_w = ASYNC == 2 ? sys_sel_w : sys_stb_r;


   //-------------------------------------------------------------------------
   //  Master-device routing.
   //-------------------------------------------------------------------------
   assign master_cyc_w = CHECK ? master_cyc_i : 1'b1;
   assign master_stb_w = master_cyc_w && master_stb_i;

   //  Route the responses back to the Wishbone master (the SPI unit).
   //  NOTE: Bus wait-state requests ('WAT_O == 1') can't be pipelined, so
   //    these are upgraded to retry ('RTY_O == 1') responses.
   assign master_ack_o = ASYNC ? bus_ack_w : bus_ack_r;
   assign master_wat_o = ASYNC ? bus_wat_w : 1'b0; // TODO: has to be ASYNC?
   assign master_rty_o = bus_rty_r;
   assign master_err_o = bus_err_r;
   assign master_dat_o = ASYNC ? mux_dat_w : mux_dat_r;

   //  Extract the address-decoder bits.
   assign dec_adr_w = master_adr_i[USB:USB-XSB];


   //-------------------------------------------------------------------------
   //  Slave-device routing.
   //-------------------------------------------------------------------------
   assign bus_cyc_w = ASYNC == 2 ? master_cyc_w : bus_cyc_r;
   assign bus_we_w  = ASYNC == 2 ? master_we_i  : bus_we_r ;
   assign bus_adr_w = ASYNC == 2 ? master_adr_i : bus_adr_r;
   assign bus_dat_w = ASYNC == 2 ? master_dat_i : bus_dat_r;

   //  And the slaves responses.
   assign bus_ack_w = cap_ack_i || acq_ack_i || dsp_ack_i || sys_ack_i;
   assign bus_wat_w = cap_wat_i || acq_wat_i || dsp_wat_i || sys_wat_i;
   assign bus_rty_w = cap_rty_i || acq_rty_i || dsp_rty_i || sys_rty_i ||
                      ASYNC == 0 && bus_wat_w;
   assign bus_err_w = cap_err_i || acq_err_i || dsp_err_i || sys_err_i;

   //-------------------------------------------------------------------------
   //  Connect the SPI unit to the system-control unit.
   assign cap_cyc_o = CAPEN ? bus_cyc_w : 1'b0;
   assign cap_stb_o = cap_stb_w;
   assign cap_we_o  = bus_we_w;
   assign cap_adr_o = bus_adr_w[CSB:0];
   assign cap_dat_o = bus_dat_w;

   //-------------------------------------------------------------------------
   //  Connect the SPI unit to the system-control unit.
   assign acq_cyc_o = ACQEN ? bus_cyc_w : 1'b0;
   assign acq_stb_o = acq_stb_w;
   assign acq_we_o  = bus_we_w;
   assign acq_adr_o = bus_adr_w[ASB:0];
   assign acq_dat_o = bus_dat_w;

   //-------------------------------------------------------------------------
   //  Connect the SPI unit to the system-control unit.
   assign dsp_cyc_o = DSPEN ? bus_cyc_w : 1'b0;
   assign dsp_stb_o = dsp_stb_w;
   assign dsp_we_o  = bus_we_w;
   assign dsp_adr_o = bus_adr_w[DSB:0];
   assign dsp_dat_o = bus_dat_w;

   //-------------------------------------------------------------------------
   //  Connect the SPI unit to the system-control unit.
   assign sys_cyc_o = SYSEN ? bus_cyc_w : 1'b0;
   assign sys_stb_o = sys_stb_w;
   assign sys_we_o  = bus_we_w;
   assign sys_adr_o = bus_adr_w[SSB:0];
   assign sys_dat_o = bus_dat_w;


   //-------------------------------------------------------------------------
   //  Output-data, 4:1 MUX.
   //-------------------------------------------------------------------------
   assign mux_dat_w = mux_sel_r == CAPAD ? cap_dat_i :
                      mux_sel_r == ACQAD ? acq_dat_i :
                      mux_sel_r == DSPAD ? dsp_dat_i :
                      mux_sel_r == SYSAD ? sys_dat_i : 8'bx ;


   //-------------------------------------------------------------------------
   //  Pipeline-register logic, allowing for shorter combinational delays.
   //-------------------------------------------------------------------------
   //  Pipeline the master's control-signals.
   always @(posedge bus_clk_i)
     if (bus_rst_i && RESET) begin
        bus_cyc_r <= #DELAY 1'b0;
        cap_stb_r <= #DELAY 1'b0;
        acq_stb_r <= #DELAY 1'b0;
        dsp_stb_r <= #DELAY 1'b0;
        sys_stb_r <= #DELAY 1'b0;
     end
     else begin
        bus_cyc_r <= #DELAY master_cyc_w;
        cap_stb_r <= #DELAY cap_sel_w;
        acq_stb_r <= #DELAY acq_sel_w;
        dsp_stb_r <= #DELAY dsp_sel_w;
        sys_stb_r <= #DELAY sys_sel_w;
     end

   //-------------------------------------------------------------------------
   //  Pipeline the address and data signals.
   always @(posedge bus_clk_i)
     begin
        bus_we_r  <= #DELAY master_we_i;
        bus_adr_r <= #DELAY master_adr_i;
        bus_dat_r <= #DELAY master_dat_i;
        mux_dat_r <= #DELAY mux_dat_w;
     end

   //-------------------------------------------------------------------------
   //  Keep track of the selected unit.
   always @(posedge bus_clk_i)
     mux_sel_r <= #DELAY master_stb_w ? dec_adr_w : mux_sel_r;


   //-------------------------------------------------------------------------
   //  Register the slaves' responses.
   //-------------------------------------------------------------------------
   always @(posedge bus_clk_i)
     bus_ack_r <= #DELAY bus_ack_w;

   //-------------------------------------------------------------------------
   //  Always pipeline "slow" signals.
   always @(posedge bus_clk_i)
     {bus_err_r, bus_rty_r} <= #DELAY {bus_err_w, bus_rty_w};


endmodule // tart_wishbone
