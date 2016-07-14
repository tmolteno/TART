`timescale 1ns/100ps
/*
 * Module      : verilog/correlator_block.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with Icarus Verilog
 * 
 * Time-multiplexed block of correlator-blocks.
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
 * 
 * Changelog:
 *  + ??/06/2016  --  initial file;
 * 
 */

`include "tartcfg.v"

module correlator_block
  #( // Pairs of antennas to correlate, for each block.
     parameter PAIRS0 = 120'hb1a191817161b0a090807060,
     parameter PAIRS1 = 120'hb3a393837363b2a292827262,
     parameter PAIRS2 = 120'hb5a595857565b4a494847464,
     parameter PAIRS3 = 120'hb1a191817161b0a090807060, // TODO:
     parameter ACCUM = `ACCUM_BITS,
     parameter MSB   = ACCUM - 1,
`ifdef __USE_SDP_DSRAM
     parameter ABITS = 12,
`else
     parameter ABITS = 7,
`endif
     parameter ASB   = ABITS - 1,
     parameter WIDTH = ACCUM+ACCUM, // Combined Re & Im components
     parameter WSB   = WIDTH-1,
     parameter XBITS = WIDTH<<2, // Wide SRAM bit-width
     parameter XSB   = XBITS-1,
     parameter MRATE = 12,       // Time-multiplexing rate
     parameter MBITS = 4,
     parameter TSB   = MBITS - 1,
     parameter BADDR = 9,        // Buffer address bit-width
     parameter BSB   = BADDR - 1,
     parameter BSIZE = 1 << BADDR,
     parameter DELAY = 3)
   (
    input              clk_x, // correlator clock
    input              rst,

    // Wishbone-like bus interface for reading visibilities.
    input              clk_i, // bus clock
    input              cyc_i,
    input              stb_i,
    input              we_i, // writes are ignored
    input              bst_i, // Bulk Sequential Transfer?
    output reg         ack_o,
    input [ASB:0]      adr_i,
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o,

    // Real and imaginary components from the antennas.
    input              sw, // switch banks
    input              en, // data is valid
    input [23:0]       re,
    input [23:0]       im,

    output reg         overflow_cos = 0,
    output reg         overflow_sin = 0
    );


   //-------------------------------------------------------------------------
   //  Visibilities buffer.
   //-------------------------------------------------------------------------
   reg [XSB:0]     visram[0:255];
   reg [XSB:0]     dat;
   reg [3:0]       block = 0;
   wire [XSB:0]    vis;

   always @(posedge clk_x)
     if (vld) visram[{block, x_wr_adr}] <= #DELAY vis;


   //-------------------------------------------------------------------------
   //  Wishbone-like bus interface.
   //-------------------------------------------------------------------------
   wire [3:0]      oc, os;
   reg             ack = 0;
   reg [2:0]       adr = 0;
   wire            vld;

   //  Acknowledge any request, even if ignored.
   always @(posedge clk_i)
     if (rst) {ack_o, ack} <= #DELAY 2'b00;
     else     {ack_o, ack} <= #DELAY {cyc_i && ack, cyc_i && stb_i};

   //  Put data onto the WB bus, in two steps.
   always @(posedge clk_i) begin
      dat <= #DELAY visram[adr_i[ASB:3]];
      adr <= #DELAY adr_i[2:0];
      case (adr)
        0: dat_o <= #DELAY dat[ACCUM*1-1:ACCUM*0];
        1: dat_o <= #DELAY dat[ACCUM*2-1:ACCUM*1];
        2: dat_o <= #DELAY dat[ACCUM*3-1:ACCUM*2];
        3: dat_o <= #DELAY dat[ACCUM*4-1:ACCUM*3];
        4: dat_o <= #DELAY dat[ACCUM*5-1:ACCUM*4];
        5: dat_o <= #DELAY dat[ACCUM*6-1:ACCUM*5];
        6: dat_o <= #DELAY dat[ACCUM*7-1:ACCUM*6];
        7: dat_o <= #DELAY dat[ACCUM*8-1:ACCUM*7];
      endcase // case (adr_i[2:0])
   end


   //-------------------------------------------------------------------------
   //  Correlator memory pointers.
   //-------------------------------------------------------------------------
   reg [TSB:0]         x_rd_adr = 0, x_wt_adr = 0, x_wr_adr = 0;
   reg                 go = 0, swap = 0, clear = 1;
   wire                wrap_x_rd_adr = x_rd_adr == MRATE - 1;
   wire [TSB:0]        next_x_rd_adr = wrap_x_rd_adr ? 0 : x_rd_adr + 1 ;

   always @(posedge clk_x)
     go <= #DELAY en;

   //  Pipelined correlator requires cycles for:
   //    { read, MAC, write } .
   always @(posedge clk_x)
     if (rst) begin
        x_rd_adr <= #DELAY 0;
        x_wt_adr <= #DELAY 0;
        x_wr_adr <= #DELAY 0;
     end
     else begin
        x_rd_adr <= #DELAY en  ? next_x_rd_adr : x_rd_adr;
        x_wt_adr <= #DELAY go  ? x_rd_adr      : x_wt_adr;
        x_wr_adr <= #DELAY vld ? x_wt_adr      : x_wr_adr;
     end


   //-------------------------------------------------------------------------
   //  Banks are switched at the next address-wrap event.
   //-------------------------------------------------------------------------
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


   //-------------------------------------------------------------------------
   //  Correlator instances.
   //-------------------------------------------------------------------------
   correlator_sdp
     #(  .ACCUM(ACCUM),
         .PAIRS(PAIRS0),
         .DELAY(DELAY)
         ) CORRELATOR0
       ( .clk_x(clk_x),
         .rst(rst),

         .sw(clear),
         .en(en),
         .re(re),
         .im(im),
         .rd(x_rd_adr),
         .wr(x_wr_adr),

         .vld(vld),
         .vis(vis[WSB:0]),

         .overflow_cos(oc[0]),
         .overflow_sin(os[0])
         );

   correlator_sdp
     #(  .ACCUM(ACCUM),
         .PAIRS(PAIRS1),
         .DELAY(DELAY)
         ) CORRELATOR1
       ( .clk_x(clk_x),
         .rst(rst),

         .sw(clear),
         .en(en),
         .re(re),
         .im(im),
         .rd(x_rd_adr),
         .wr(x_wr_adr),

         .vld(),
         .vis(vis[WIDTH+WSB:WIDTH]),

         .overflow_cos(oc[1]),
         .overflow_sin(os[1])
         );

   correlator_sdp
     #(  .ACCUM(ACCUM),
         .PAIRS(PAIRS2),
         .DELAY(DELAY)
         ) CORRELATOR2
       ( .clk_x(clk_x),
         .rst(rst),

         .sw(clear),
         .en(en),
         .re(re),
         .im(im),
         .rd(x_rd_adr),
         .wr(x_wr_adr),

         .vld(),
         .vis(vis[WIDTH+WIDTH+WSB:WIDTH+WIDTH]),

         .overflow_cos(oc[2]),
         .overflow_sin(os[2])
         );

   correlator_sdp
     #(  .ACCUM(ACCUM),
         .PAIRS(PAIRS3),
         .DELAY(DELAY)
         ) CORRELATOR3
       ( .clk_x(clk_x),
         .rst(rst),

         .sw(clear),
         .en(en),
         .re(re),
         .im(im),
         .rd(x_rd_adr),
         .wr(x_wr_adr),

         .vld(),
         .vis(vis[WIDTH+WIDTH+WIDTH+WSB:WIDTH+WIDTH+WIDTH]),

         .overflow_cos(oc[3]),
         .overflow_sin(os[3])
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

   
endmodule // correlator_block
