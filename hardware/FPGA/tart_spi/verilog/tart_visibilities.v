`timescale 1ns/100ps
/*
 * 
 * Manages the correlators, including its flags and settings.
 * Transfers visibilities from the correlators to a SRAM after each bank-swap.
 * 
 * NOTE:
 *  + 8-bit bus to the SPI interface, and 32-bit bus to the correlators;
 * 
 * TODO:
 *  + redirection ROM;
 * 
 */

// Bus transaction states.
`define BUS_IDLE  0
`define BUS_WAIT  1
`define BUS_READ  2
`define BUS_WRITE 4

module tart_visibilities
  #(parameter BLOCK = 32,
    parameter MSB   = BLOCK-1,
    parameter COUNT = 576,     // correlators and averages
    parameter CBITS = 10,
    parameter CSB   = CBITS,
    parameter MRATE = 12,       // time-multiplexing rate
    parameter MBITS = 4,
    parameter MSKIP = (1 << MBITS - MRATE + 1),
    parameter RSB   = MBITS-1,
    parameter DELAY = 3)
   (
    input              clk_i, // bus clock
    input              rst_i,

    // Wishbone-like bus interface that connects to `tart_spi`.
    input              cyc_i,
    input              stb_i,
    input              we_i, // writes only work for system registers
    input              bst_i, // Bulk Sequential Transfer?
    output reg         ack_o = 0,
    input [CBITS+2:0]  adr_i, // upper address-space for registers
    input [7:0]        byt_i,
    output reg [7:0]   byt_o,

    // Wishbone-like bus interface that connects to the correlators.
    output reg         cyc_o = 0,
    output reg         stb_o = 0,
    output reg         we_o = 0, // writes only work for system registers
    output reg         bst_o = 0, // Bulk Sequential Transfer?
    input              ack_i,
    output reg [CSB:0] adr_o = 0, // upper address-space for registers
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o,

    // Status flags for the correlators and visibilities.
    input              switching, // inicates that banks have switched
    output reg [MSB:0] blocksize = 0, // block size - 1
    output reg         available = 0 // asserted when a window is accessed
    );


   //-------------------------------------------------------------------------
   //  Dual-port SRAM's for all visibilities from the same "window."
   //-------------------------------------------------------------------------
   reg [7:0]           sram0 [0:COUNT-1];
   reg [7:0]           sram1 [0:COUNT-1];
   reg [7:0]           sram2 [0:COUNT-1];
   reg [7:0]           sram3 [0:COUNT-1];


   //-------------------------------------------------------------------------
   //  System registers.
   //  NOTE: Addressed by `{3'b111, reg#}`.
   //  TODO:
   //-------------------------------------------------------------------------
   //  Register#:
   //    00  --  status register;
   //    01  --  logarithm of the bit-width of the visibilities counter;
   //
   wire [7:0]          status = {available, 2'b00, count_log[4:0]};
   reg [7:0]           count_log = 0;

   // TODO: Should be computed from `count_log`, and synchronised across
   //   domains.
   always @(posedge clk_i)
     if (rst_i) begin
        count_log <= #DELAY 0;
        blocksize <= #DELAY 0;
     end
     else if (cyc_i && stb_i && we_i && adr_i == 11'h701) begin
        count_log <= #DELAY byt_i;
        blocksize <= #DELAY (1 << byt_i[4:0]) - 1;
     end

   //-------------------------------------------------------------------------
   //  Correlator read-back.
   //-------------------------------------------------------------------------
   wire                wrap_adr = &adr_o[CSB:MBITS];
   wire                skip_adr = adr_o[RSB:0] == MRATE-1;
   wire [CSB:0]        next_adr = adr_o + (skip_adr ? MSKIP : 1);
   wire                wrap_vis_adr = vis_adr == COUNT-1;
   reg [CSB:0]         vis_adr = 0;

   always @(posedge clk_i)
     if (rst_i)
       {cyc_o, stb_o, we_o, bst_o, adr_o} <= #DELAY 0;
     else if (switching) begin
        {cyc_o, stb_o, bst_o} <= #DELAY 7;
        {we_o, adr_o} <= #DELAY 0;
     end
     else if (cyc_o && stb_o && bst_o) begin
        bst_o <= #DELAY wrap_adr ? 0 : bst_o;
        adr_o <= #DELAY wrap_adr ? adr_o : next_adr;
     end
     else if (cyc_o && stb_o && ack_i && wrap_vis_adr)
       {cyc_o, stb_o, we_o, bst_o, adr_o} <= #DELAY 0;

   //-------------------------------------------------------------------------
   //  Address and data update logic for SRAM writes.
   always @(posedge clk_i)
     if (rst_i || switching && !cyc_o)
       vis_adr <= #DELAY 0;
     else if (cyc_o && stb_o && !we_o && ack_i)
       vis_adr <= #DELAY vis_adr + 1;
     else
       vis_adr <= #DELAY vis_adr;

   always @(posedge clk_i)
     if (cyc_o && stb_o && !we_o && ack_i) begin
        sram0[vis_adr] <= #DELAY dat_i[7:0];
        sram1[vis_adr] <= #DELAY dat_i[15:8];
        sram2[vis_adr] <= #DELAY dat_i[23:16];
        sram3[vis_adr] <= #DELAY dat_i[31:24];
     end


   //-------------------------------------------------------------------------
   //  Bus interface to TART's SPI unit.
   //-------------------------------------------------------------------------
   wire [CSB-1:0] sram_adr = adr_i[CBITS+1:2];
   reg            bst_r = 0;

   always @(posedge clk_i)
     if (rst_i) ack_o <= #DELAY 0;
     else       ack_o <= #DELAY cyc_i && stb_i && (bst_r || !ack_o);

   always @(posedge clk_i)
     if (rst_i) bst_r <= #DELAY 0;
     else       bst_r <= #DELAY cyc_i && stb_i && bst_i;

   // SRAM reads & writes.
   always @(posedge clk_i)
     if (!rst_i && cyc_i && stb_i && !we_i)
       case (adr_i[1:0])
         0: byt_o <= #DELAY sram0[sram_adr];
         1: byt_o <= #DELAY sram1[sram_adr];
         2: byt_o <= #DELAY sram2[sram_adr];
         3: byt_o <= #DELAY sram3[sram_adr];
       endcase // case (adr_i[1:0])
     else if (!rst_i && cyc_i && stb_i && we_i)
       case (adr_i[1:0])
         0: sram0[sram_adr] <= #DELAY byt_i;
         1: sram1[sram_adr] <= #DELAY byt_i;
         2: sram2[sram_adr] <= #DELAY byt_i;
         3: sram3[sram_adr] <= #DELAY byt_i;
       endcase // case (adr_i[1:0])

   //-------------------------------------------------------------------------
   //  Manage the `available` flag.
   always @(posedge clk_i)
     if (rst_i)
       available <= #DELAY 0;
     else if (cyc_o && stb_o && ack_i && wrap_vis_adr)
       available <= #DELAY 1;
     else if (cyc_i && stb_i && !we_i && adr_i == 0)
       available <= #DELAY 0;


endmodule // tart_visibilities
