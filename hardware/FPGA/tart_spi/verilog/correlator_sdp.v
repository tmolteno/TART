`timescale 1ns/100ps
/*
 * Module      : verilog/tart_aquire.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * Time-multiplexed correlator block.
 * 
 * This version operates the Xilinx SRAM's in Simple Dual-Port mode, to make
 * better use of the available SRAM resources.
 * 
 * NOTE:
 *  + typically several of these would be attached to a common set of antenna
 *    and a system bus;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as multi-
 *    port SRAM's are used;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM resources;
 *  + ignores write-attempts, but still generates acknowledges;
 * 
 * Changelog:
 *  + 04/07/2016  --  initial file (refactored from `correlator`);
 * 
 */

`include "tartcfg.v"

module correlator_sdp
  #(parameter ACCUM = `ACCUM_BITS,            // Re/Im accumulator bit-widths
    parameter MSB   = ACCUM - 1,
    parameter WIDTH = ACCUM + ACCUM, // Combined Re & Im components
    parameter WSB   = WIDTH - 1,
    parameter BADDR = 9,        // Buffer address bit-width
    parameter ASB   = BADDR - 1,
    parameter BSIZE = 1 << BADDR,
    parameter MRATE = 12,       // Time-multiplexing rate
    parameter MBITS = 4,
    parameter TSB   = MBITS - 1,
    // Pairs of antennas to correlate:
    parameter PAIRS = 120'hb1a191817161b0a090807060,
    parameter DELAY = 3)
   (
    input           clk_x, // correlator clock
    input           rst,

    // Wishbone-like bus interface for reading visibilities.
    input           clk_i, // bus clock
    input           cyc_i,
    input           stb_i,
    input           we_i, // writes are ignored
    input           bst_i, // burst-mode transfer?
    output reg      ack_o = 0,
    input [BADDR:0] adr_i,
    input [MSB:0]   dat_i,
    output [MSB:0]  dat_o,

    // Real and imaginary components from the antennas.
    input           sw, // switch banks
    input           en, // data is valid
    input [23:0]    re,
    input [23:0]    im,
    output          vld,
    output [ASB:0]  adr,
    output [WSB:0]  vis,

    output reg      overflow_cos = 0,
    output reg      overflow_sin = 0
    );


   //-------------------------------------------------------------------------
   //  Xilinx Distributed & Block RAM's for the accumulators & visibilities.
   //-------------------------------------------------------------------------
   // For Xilinx FPGA's, this should be two `RAM32M's, and operating in SDP
   // mode.
   wire [MSB:0]        dcos_w, dsin_w, qcos, qsin;
   reg [MSB:0]         dcos, dsin;

   // Xilinx Block RAM for the buffered, visibilities data.
   reg [WSB:0]         visram[0:BSIZE-1];
   reg [ASB-MBITS:0]   block = 0;
   reg                 swap = 0, clear = 0;
   reg [TSB:0]         x_rd_adr = 0, x_wt_adr = 0, x_wr_adr = 0;
   wire                wrap_x_rd_adr = x_rd_adr == MRATE - 1;
   wire [TSB:0]        next_x_rd_adr = wrap_x_rd_adr ? 0 : x_rd_adr + 1 ;
   wire                valid, oc, os;


   //-------------------------------------------------------------------------
   //  Signals to a wide, external SRAM.
   //-------------------------------------------------------------------------
   assign vld = valid;
   assign adr = {block, x_wr_adr};
   assign vis = {qsin, qcos};


   //-------------------------------------------------------------------------
   //  Wishbone-like bus interface logic.
   //-------------------------------------------------------------------------
   /*
   wire [MSB:0]        vis_cos, vis_sin;

   assign {vis_sin, vis_cos} = visram[adr_i[BADDR:1]];

   //  Read only from the addressed block, and the LSB determines whether to
   //  return the sine or cosine component.
   always @(posedge clk_i)
     if (cyc_i && stb_i)
       dat_o <= #DELAY adr_i[0] ? vis_sin : vis_cos;
    */

   //-------------------------------------------------------------------------
   reg [WSB:0]         d_reg = 0;
   reg                 m_reg = 0;

   assign dat_o = m_reg ? d_reg[WSB:ACCUM] : d_reg[MSB:0];

   always @(posedge clk_i)
     if (cyc_i && stb_i) begin
        m_reg <= #DELAY adr_i[0];
        d_reg <= #DELAY visram[adr_i[BADDR:1]];
     end

   //  Acknowledge any request, even if ignored.
   always @(posedge clk_i)
     if (rst) ack_o <= #DELAY 0;
     else     ack_o <= #DELAY cyc_i && stb_i;


   //-------------------------------------------------------------------------
   //  Correlator memories.
   //-------------------------------------------------------------------------
   //  Pipelined correlator requires cycles for:
   //    { read, MAC, write } .
   always @(posedge clk_x)
     if (rst) begin
        x_rd_adr <= #DELAY 0;
        x_wt_adr <= #DELAY 0;
        x_wr_adr <= #DELAY 0;
     end
     else begin
        x_rd_adr <= #DELAY en    ? next_x_rd_adr : x_rd_adr;
        x_wt_adr <= #DELAY go    ? x_rd_adr      : x_wt_adr;
        x_wr_adr <= #DELAY valid ? x_wt_adr      : x_wr_adr;
     end

   /*
   //  Banks are switched at the next address-wrap event.
   always @(posedge clk_x)
     if (rst) begin : RAM_RESET_LOGIC
        swap  <= #DELAY 0;
        block <= #DELAY 0;
        clear <= #DELAY 1;
     end
//      else if (wrap_x_rd_adr && swap) begin // swap banks
     else if (wrap_x_rd_adr && (sw || swap)) begin // swap banks
        swap  <= #DELAY 0;
        block <= #DELAY block + 1;
        clear <= #DELAY 1;
     end
     else if (sw && !swap) begin // swap banks @next wrap
        swap  <= #DELAY 1;
     end
     else if (en && wrap_x_rd_adr && clear) begin // finished restarting counters
        clear <= #DELAY 0;
     end
    */

   //  Banks are switched at the next address-wrap event.
   always @(posedge clk_x)
     if (rst) begin
        swap  <= #DELAY 0;
        block <= #DELAY 0;
     end
     else if (wrap_x_rd_adr && (sw || swap)) begin // swap banks
        swap  <= #DELAY 0;
        block <= #DELAY block + 1;
     end
     else if (sw && !swap) begin // swap banks @next wrap
        swap  <= #DELAY 1;
     end

   //  Clear a bank when correlators are enabled, or during the first set of
   //  writes after a bank-switch.
   always @(posedge clk_x)
     if (rst || !en)
        clear <= #DELAY 1;
     else if (wrap_x_rd_adr && (sw || swap))
        clear <= #DELAY 1;
     else if (wrap_x_rd_adr && clear) // finished restarting counters
        clear <= #DELAY 0;

   always @(posedge clk_x)
     if (en) begin : RAM_READ
        dcos <= #DELAY clear ? 0 : dcos_w;
        dsin <= #DELAY clear ? 0 : dsin_w;
     end

   //  Write the current sums to the BRAM, so when a block-switch occurs, the
   //  buffer already contains the desired visibilities.
   always @(posedge clk_x)
     if (valid)
       visram[{block, x_wr_adr}] <= #DELAY {qsin, qcos};


   //-------------------------------------------------------------------------
   //  Select pairs of antenna to correlate.
   //-------------------------------------------------------------------------
   //  TODO: Can more of this be parameterised?
   parameter PAIRS00 = (PAIRS >>   0) & 10'h3ff;
   parameter PAIRS01 = (PAIRS >>  10) & 10'h3ff;
   parameter PAIRS02 = (PAIRS >>  20) & 10'h3ff;
   parameter PAIRS03 = (PAIRS >>  30) & 10'h3ff;
   parameter PAIRS04 = (PAIRS >>  40) & 10'h3ff;
   parameter PAIRS05 = (PAIRS >>  50) & 10'h3ff;
   parameter PAIRS06 = (PAIRS >>  60) & 10'h3ff;
   parameter PAIRS07 = (PAIRS >>  70) & 10'h3ff;
   parameter PAIRS08 = (PAIRS >>  80) & 10'h3ff;
   parameter PAIRS09 = (PAIRS >>  90) & 10'h3ff;
   parameter PAIRS0A = (PAIRS >> 100) & 10'h3ff;
   parameter PAIRS0B = (PAIRS >> 110) & 10'h3ff;

`ifdef __icarus
   // NOTE: Icarus Verilog doesn't seem to support curly-braces for setting
   //   the wire values;
   wire [9:0]   pairs[0:11];

   assign pairs[00] = PAIRS00;
   assign pairs[01] = PAIRS01;
   assign pairs[02] = PAIRS02;
   assign pairs[03] = PAIRS03;
   assign pairs[04] = PAIRS04;
   assign pairs[05] = PAIRS05;
   assign pairs[06] = PAIRS06;
   assign pairs[07] = PAIRS07;
   assign pairs[08] = PAIRS08;
   assign pairs[09] = PAIRS09;
   assign pairs[10] = PAIRS0A;
   assign pairs[11] = PAIRS0B;
`else
   wire [9:0]   pairs[0:11] = {PAIRS00, PAIRS01, PAIRS02, PAIRS03,
                               PAIRS04, PAIRS05, PAIRS06, PAIRS07,
                               PAIRS08, PAIRS09, PAIRS0A, PAIRS0B};
`endif

   wire [9:0]   pairs_index = pairs[x_rd_adr];
   wire [4:0]   a_index = pairs_index[4:0];
   wire [4:0]   b_index = pairs_index[9:5];

   reg          go = 0, ar, br, bi;

   //  Add a cycle of latency to wait for the RAM read.
   always @(posedge clk_x) begin
      go <= #DELAY en;
      ar <= #DELAY re[a_index];
      br <= #DELAY re[b_index];
      bi <= #DELAY im[b_index];
   end


   //-------------------------------------------------------------------------
   //  Time-multiplexed correlator.
   //-------------------------------------------------------------------------
   correlate_cos_sin
     #(  .ACCUM(ACCUM), .DELAY(DELAY) ) CORR_COS_SIN0
       ( .clk(clk_x),
         .rst(rst),

         // Antenna enables and inputs:
         .en(go),
         .ar(ar),
         .br(br),
         .bi(bi),

         // Accumulator inputs and outputs:
         .dcos(dcos),
         .dsin(dsin),
         .valid(valid),
         .qcos(qcos),
         .qsin(qsin),

         // Overflow flags:
         .oc(oc),
         .os(os)
         );

   
   //-------------------------------------------------------------------------
   //  RAM32M's implemented the nerdy way.
   //-------------------------------------------------------------------------
   RAM32X6_SDP
     #( .INITA(64'h0),
        .INITB(64'h0),
        .INITC(64'h0),
        .INITD(64'h0),
        .DELAY(3)
        ) RAM32X6_SDP_COS0 [3:0]
       (.WCLK(clk_x),
        .WE(valid),
        .WADDR({1'b0, x_wr_adr}),
        .DI(qcos),
        .RADDR({1'b0, x_rd_adr}),
        .DO(dcos_w)
        );

   RAM32X6_SDP
     #( .INITA(64'h0),
        .INITB(64'h0),
        .INITC(64'h0),
        .INITD(64'h0),
        .DELAY(3)
        ) RAM32X6_SDP_SIN0 [3:0]
       (.WCLK(clk_x),
        .WE(valid),
        .WADDR({1'b0, x_wr_adr}),
        .DI(qsin),
        .RADDR({1'b0, x_rd_adr}),
        .DO(dsin_w)
        );


/*
   //-------------------------------------------------------------------------
   //  Explicit instantiation, because XST sometimes gets it wrong.
   //-------------------------------------------------------------------------
`ifndef __icarus
   wire [13:0]      ADDRA = {read_address , {14-ADDR_WIDTH{1'b0}}};
   wire [13:0]      ADDRB = {write_address, {14-ADDR_WIDTH{1'b0}}};

   RAMB8BWER
     #( // DATA_WIDTH_A/DATA_WIDTH_B: 'If RAM_MODE="TDP": 0, 1, 2, 4, 9 or 18; If RAM_MODE="SDP": 36'
        .DATA_WIDTH_A(36),
        .DATA_WIDTH_B(36),
        .DOA_REG(0),
        .DOB_REG(0),
        .EN_RSTRAM_A("FALSE"),
        .EN_RSTRAM_B("FALSE"),
        .INIT_A(18'h00000),
        .INIT_B(18'h00000),
        .INIT_FILE("NONE"),
        .RAM_MODE("SDP"),
        .RSTTYPE("SYNC"),
        .RST_PRIORITY_A("CE"),
        .RST_PRIORITY_B("CE"),
        .SIM_COLLISION_CHECK("NONE"),
        .SRVAL_A(18'h00000),
        .SRVAL_B(18'h00000),
        // WRITE_MODE_A/WRITE_MODE_B: "WRITE_FIRST", "READ_FIRST", or "NO_CHANGE" 
        .WRITE_MODE_A("WRITE_FIRST"),
        .WRITE_MODE_B("WRITE_FIRST") 
   )
   VISRAM1 (
      // Port A Data: 16-bit (each) output: Port A data
      .DOADO(DOADO),             // 16-bit output: A port data/LSB data output
      .DOPADOP(DOPADOP),         // 2-bit output: A port parity/LSB parity output
      // Port B Data: 16-bit (each) output: Port B data
      .DOBDO(DOBDO),             // 16-bit output: B port data/MSB data output
      .DOPBDOP(DOPBDOP),         // 2-bit output: B port parity/MSB parity output
            
      // Port A Address/Control Signals: 13-bit (each) input: Port A address and control signals (write port
      // when RAM_MODE="SDP")
      .ADDRAWRADDR(ADDRAWRADDR), // 13-bit input: A port address/Write address input
      .CLKAWRCLK(CLKAWRCLK),     // 1-bit input: A port clock/Write clock input
      .ENAWREN(ENAWREN),         // 1-bit input: A port enable/Write enable input
      .REGCEA(REGCEA),           // 1-bit input: A port register enable input
      .RSTA(RSTA),               // 1-bit input: A port set/reset input
      .WEAWEL(WEAWEL),           // 2-bit input: A port write enable input
      // Port A Data: 16-bit (each) input: Port A data
      .DIADI(DIADI),             // 16-bit input: A port data/LSB data input
      .DIPADIP(DIPADIP),         // 2-bit input: A port parity/LSB parity input
      // Port B Address/Control Signals: 13-bit (each) input: Port B address and control signals (read port
      // when RAM_MODE="SDP")
      .ADDRBRDADDR(ADDRBRDADDR), // 13-bit input: B port address/Read address input
      .CLKBRDCLK(CLKBRDCLK),     // 1-bit input: B port clock/Read clock input
      .ENBRDEN(ENBRDEN),         // 1-bit input: B port enable/Read enable input
      .REGCEBREGCE(REGCEBREGCE), // 1-bit input: B port register enable/Register enable input
      .RSTBRST(RSTBRST),         // 1-bit input: B port set/reset input
      .WEBWEU(WEBWEU),           // 2-bit input: B port write enable input
      // Port B Data: 16-bit (each) input: Port B data
      .DIBDI(DIBDI),             // 16-bit input: B port data/MSB data input
      .DIPBDIP(DIPBDIP)          // 2-bit input: B port parity/MSB parity input
   );
`endif // `ifndef __icarus
*/

endmodule // correlator_sdp
