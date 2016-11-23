`timescale 1ns/100ps
/*
 * Module      : bench/tart_dsp_tb.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
 * 
 * Simulates two visibility calculations, using TART's 'tart_dsp' logic core,
 * and then reads them back, via a streaming data-transfer interface.
 * 
 * NOTE:
 * 
 * TODO:
 * 
 */

`include "tartcfg.v"

module tart_dsp_tb;

   //-------------------------------------------------------------------------
   //
   //  Settings.
   //
   //-------------------------------------------------------------------------
   //  Antenna + accumulator settings:
   parameter AXNUM = `NUM_ANTENNA; // Number of antenna inputs
   parameter NSB   = AXNUM-1;
   parameter ACCUM = `ACCUM_BITS;  // Bit-width of the accumulators
   parameter BLOCK = ACCUM;        // Block samples-counter bits
   parameter MSB   = BLOCK-1;      // Accumulator MSB
   parameter TRATE = `TMUX_RATE;   // Time-multiplexing rate
   parameter TBITS = `TMUX_BITS;   // TMUX bits

   //  Settings for the visibilities data banks:
   parameter BREAD = NREAD << 2;
   parameter BANKS = 1 << XBITS;
   parameter XBITS = `BANK_BITS; // Bank-counter bit-width; of the b
   parameter XSB   = XBITS-1;    // MSB of the bank-counter

   //  Internal correlator data-bus settings:
   parameter CBITS = XBITS+FBITS;
   parameter CSB   = CBITS-1;

   //  External Wishbone-like bus setttings:
   parameter ABITS = `WBADR_BITS;  // Address bit-width
   parameter ASB   = ABITS-1;      // Address MSB
   parameter BBITS = `WBBUS_BITS;  // Bit-width for the SoC WB bus
   parameter BSB   = BBITS-1;      // Bus MSB

   //  Read-back settings:
   parameter FBITS = `READ_BITS; // Fetch-counter bit-width
   parameter FSB   = FBITS-1;    // MSB of fetch-counter
   parameter READS = 2;         // Banks of visibilities to read

//    parameter COUNT = 3; // count down from:  (1 << COUNT) - 1;
   parameter COUNT = 6; // count down from:  (1 << COUNT) - 1;
//    parameter COUNT = 8; // count down from:  (1 << COUNT) - 1;
//    parameter COUNT = 10; // count down from:  (1 << COUNT) - 1;
//    parameter COUNT = 12; // count down from:  (1 << COUNT) - 1;

//    parameter NREAD = 4;
//    parameter NREAD = 48;
   parameter NREAD = 72;
//    parameter NREAD = 144;
//    parameter NREAD = 288;
//    parameter NREAD = 576;

   //  Additional simulation settings:
   parameter RNG   = `RANDOM_DATA; // Use random antenna data?
   parameter DELAY = `DELAY;       // Simulated combinational delay
   parameter NOISY = 1;            // display extra debug info


   //-------------------------------------------------------------------------
   //
   //  Signals.
   //
   //-------------------------------------------------------------------------
   //  Wishbone bus signals.
   wire [BSB:0] dat, val, drx;
   reg [BSB:0]  dtx;
   wire         cyc, stb, we, ack, wat, rty, err;
   reg [1:0]    adr;

   //  Clock & reset signals.
   reg          clk_x = 1'b1, b_clk = 1'b1, rst = 1'b0;

   //  Correlator-domain signals.
   reg          vld_x = 1'b0, new_x = 1'b0;
   reg [NSB:0]  sig_x;

   //  Bus-cycle control signals.
   reg          set = 1'b0, get = 1'b0;
   wire         fin, bsy, fail;

   //  DSP settings & signals.
   reg          aq_enabled = 1'b0;
   reg          read_en = 1'b0;
   wire [MSB:0] blocksize;
   wire [31:0]  checksum;
   wire         dsp_en, enabled, newblock, streamed;
   wire         overflow, accessed, available, switched;
   wire         dsp_stuck, dsp_limp;
   wire [XSB:0] dsp_bank;

   //  Data memories & signals.
   reg [NSB:0]  data [0:255];
   reg [31:0]   viz = 32'h0;
   reg [4:0]    log_bsize = COUNT[4:0];
   reg [XSB:0]  viz_bank = {XBITS{1'b0}};

   assign dsp_en  = enabled;


   //-------------------------------------------------------------------------
   //
   //  TIMEOUT.
   //
   //-------------------------------------------------------------------------
   //  Exit if the simulation appears to have stalled.
   parameter LIMIT = 1000 + (1 << COUNT) * 320 + NREAD * 80;
//    parameter LIMIT = 2000;

   initial begin : SIM_FAILED
      $display("%12t: Simulation TIMEOUT limit:\t%12d", $time, LIMIT);
      #LIMIT $display ("\nTIMEOUT!\n");
      $finish;
   end // SIM_FAILED


   //-------------------------------------------------------------------------
   //
   //  SIMULATION STIMULI.
   //
   //-------------------------------------------------------------------------
   //  Setup correlator and bus clocks, respectively.
   always #`CLK_X  clk_x <= ~clk_x;
   always #`CLK_B  b_clk <= ~b_clk;


   //-------------------------------------------------------------------------
   //  Simulate two visibility calculations.
   integer      num = 0;
   integer      ptr = 0;
   initial begin : SIM_BLOCK
      if (COUNT < 8) begin
         $dumpfile ("vcd/dsp_tb.vcd");
         $dumpvars;
      end

      //----------------------------------------------------------------------
      $display("\n%12t: TART DSP settings:", $time);
      $display(  "%12t:  TART I/O bus settings:", $time);
      $display(  "%12t:   SPI data-bus bit-width:  \t\t%4d", $time, BBITS);
      $display(  "%12t:   Number of visibilities banks:    \t%4d", $time, BANKS);
      $display(  "%12t:  TART visibilities read-back settings:", $time);
      $display(  "%12t:   Visibility-data address-width:   \t%4d", $time, ABITS);
      $display(  "%12t:   Data prefetch block-size (words):\t%4d", $time, NREAD);
      $display(  "%12t:   Data prefetch block-size (bytes):\t%4d", $time, BREAD);
      $display(  "%12t:  TART correlator settings:", $time);
      $display(  "%12t:   Accumulator bit-width:   \t\t%4d", $time, ACCUM);
      $display(  "%12t:   Correlator bus data-width:       \t%4d", $time, BLOCK);
      $display(  "%12t:   Correlator bus address-width:    \t%4d", $time, CBITS);

      //----------------------------------------------------------------------
      $display("\n%12t: Generating fake antenna data:", $time);
      if (RNG)
        $display("%12t:  (Data is random values)", $time);
      else
        $display("%12t:  (Data is just increasing counter values)", $time);
      for (ptr = 0; ptr < 256; ptr = ptr+1) begin
         if (RNG) data[ptr] <= $random;
         else     data[ptr] <= ptr;
      end


      //----------------------------------------------------------------------
      #20 $display("\n%12t: Issuing RESET.", $time);
      #13 rst <= 1; #40 rst <= 0;

      //----------------------------------------------------------------------
      //  Start the DSP unit.
      $display("%12t: Setting block-size (2^%1d)", $time, log_bsize);
      $display("%12t: Beginning correlation (bank %1d)", $time, viz_bank);
      #40 set <= 1; num <= 1; dtx <= {1'b1, 2'b00, log_bsize}; adr <= 2'b11;
      while (!fin) #10;


      //----------------------------------------------------------------------
      //  Fill 'READS' banks with visibilities, then stop.
      $display("%12t: Waiting for %1d banks of visibilities", $time, READS);
      while (viz_bank < READS) #10;

      while (bsy) #10;
      $display("%12t: Stopping data-correlation.", $time);
      #10 set <= 1; num <= 1; dtx <= 8'h00; adr <= 2'b11;
      while (!fin) #10;


      //----------------------------------------------------------------------
      //  Read back the visibilities.
      $display("%12t: Waiting for all visibilities to be fetched.", $time);
      #10 while (dsp_bank < READS) #80;


      //----------------------------------------------------------------------
      #80 $display("%12t: Simulation finished:", $time);
      $finish;
   end // block: SIM_BLOCK


   //-------------------------------------------------------------------------
   //  Trigger visibilities read-backs.
   //-------------------------------------------------------------------------
   initial begin : SIM_READ
      #83;
      while (1'b1) begin
         while (!newblock) #10;
         while (bsy) #10;
         #10 $display("%12t: Reading back visibilities (bank %1d)", $time, dsp_bank);
         #10 get <= 1; num <= BREAD; adr <= 2'b00;
         while (!fin) #10;
      end
   end // block: SIM_READ

   //  Enable the streaming read-back module whenever there is data, or on
   //  access.
   always @(posedge b_clk)
     if (b_rst || streamed)
       read_en <= #DELAY 1'b0;
     else if (newblock || get && adr == 2'b00)
       read_en <= #DELAY 1'b1;
     else
       read_en <= #DELAY read_en;


   //-------------------------------------------------------------------------
   //  Track the active correlator-bank.
   //-------------------------------------------------------------------------
   always @(posedge b_clk)
     if (switched) begin
        $display("%12t: Bank switched (to bank %1d)", $time, viz_bank);
        viz_bank <= #DELAY viz_bank + 1;
     end


   //-------------------------------------------------------------------------
   //  Display simulation progress waypoints.
   //-------------------------------------------------------------------------
   always @(posedge b_clk)
     if (newblock)
       $display("%12t: New block available.", $time);

   always @(posedge b_clk)
     if (streamed)
       $display("%12t: Block streamed (bank = %2d).", $time, dsp_bank);



   //-------------------------------------------------------------------------
   //  
   //  SIMULATED WISHBONE BUS MASTER.
   //  
   //-------------------------------------------------------------------------
   reg [15:0]  rxd = 0, req = 0;
   reg         bx = 0, rd = 0, wr = 0;


   //-------------------------------------------------------------------------
   //  Initiate bus cycles/transfers.
   //-------------------------------------------------------------------------
   always @(posedge b_clk)
     if (rst) begin
        {get, set} <= #DELAY 2'b00;
     end
     else if (set) begin
        $display("%12t: WB write beginning (bytes = %1d)", $time, num);
        {get, set} <= #DELAY 2'b00;
     end
     else if (get) begin
        $display("%12t: WB read beginning (bytes = %1d)", $time, num);
        {get, set} <= #DELAY 2'b00;
     end

   always @(posedge fin)
     $display("%12t: WB transfer ending", $time);

   always @(posedge b_clk)
     if (rd && req != num)
       {rd, wr} <= #DELAY 2'b10;
     else
       {rd, wr} <= #DELAY {get, set};

   always @(posedge b_clk)
     if (rst || fin)
       req <= #DELAY 1;
     else if (bx && rd && req != num)
//      else if (rd)
       req <= #DELAY req + 1;

   always @(posedge b_clk)
     if (rst || rxd == 1 && ack)
       bx <= #DELAY 1'b0;
     else if (get || set)
       bx <= #DELAY 1'b1;
     else
       bx <= #DELAY bx;


   //-------------------------------------------------------------------------
   //  Count the number of received bytes, for multi-byte transfers.
   //-------------------------------------------------------------------------
   //  NOTE: Burst-writes aren't supported.
   always @(posedge b_clk)
     if (get || set)
       rxd <= #DELAY get ? num : 1;
     else if (cyc && ack)
       rxd <= #DELAY rxd - 1;


   //-------------------------------------------------------------------------
   //
   //  SIMULATION RESULTS.
   //
   //-------------------------------------------------------------------------
   wire [FBITS:0] f_nxt = f_adr + 1;
   reg [MSB:0]    fetched [0:NREAD-1];
   reg [FSB:0]    f_adr = {FBITS{1'b0}};
   reg [MSB:0]    f_dat = {ACCUM{1'b0}};
   reg [1:0]      f_cnt = 2'b00;

   wire [1:0]     dst = cyc ? adr : 'bz;
   wire           rdy = cyc && !we && ack;

   assign val = cyc && we ? dtx : 'bz;
//    assign dat = rdy       ? drx : 'bz;


   //-------------------------------------------------------------------------
   //  Assemble bytes into words, and stored them until the transfer has been
   //  completed.
   always @(posedge b_clk)
     if (rst || fin) begin
        f_cnt <= #DELAY 2'b00;
        f_adr <= #DELAY {FBITS{1'b0}};
     end
     else if (rdy) begin
        f_cnt <= #DELAY f_cnt + 1;
        f_adr <= #DELAY f_cnt == 2'b11 ? f_nxt : f_adr;
        f_dat <= #DELAY {dat, f_dat[MSB:BBITS]};
        fetched[f_adr] <= #DELAY f_dat;
     end

   //-------------------------------------------------------------------------
   //  Format and display the retrieved visibilities.
   integer idx;
   parameter CHUNK = NREAD / 6;

   always @(posedge fin)
     if (f_adr >= NREAD-1) begin
        $display("\n%12t: Fetched visibilities (num = %1d):", $time, f_adr);
        for (idx = 0; idx < NREAD; idx = idx + CHUNK) begin
           for (ptr = 0; ptr < CHUNK; ptr = ptr + TRATE) begin
              $write("\t");
              for (num = 0; num < TRATE; num = num + 1)
                $write("%06x ", fetched[idx + ptr + num]);
              $write("\n");
           end
           $write("\n");
        end
        $write("\n");
     end


   //-------------------------------------------------------------------------
   //  
   //  GENERATE FAKE DRAM CONTENTS.
   //  
   //-------------------------------------------------------------------------
   wire [NSB:0] antenna;
   reg [3:0]    cnt = 0;
   wire [3:0]   next_cnt = wrap_cnt ? 0 : cnt + 1 ;
   wire         wrap_cnt = cnt == TRATE-1;
   integer      adr_x = 0;
   wire [32:0]  nxt_x = adr_x + 1;
   reg          end_x = 1'b0;
   wire [NSB:0] sig_w = vld_x ? sig_x : {AXNUM{1'bz}};

   assign antenna = data[adr_x[7:0]];

   always @(posedge clk_x)
     if (rst)
       cnt <= #DELAY 0;
     else
       cnt <= #DELAY dsp_en || vld_x ? next_cnt : cnt;

   always @(posedge clk_x)
     if (rst)
       {end_x, new_x} <= #DELAY 0;
     else if (dsp_en)
       {end_x, new_x} <= #DELAY {1'b0, wrap_cnt};
     else
       {end_x, new_x} <= #DELAY {vld_x && wrap_cnt, 1'b0};

   always @(posedge clk_x)
     if (rst)
       vld_x <= #DELAY 0;
     else if (end_x)
       vld_x <= #DELAY 0;
     else if (new_x)
       vld_x <= #DELAY 1;
     else
       vld_x <= #DELAY vld_x;

   always @(posedge clk_x)
     sig_x <= #DELAY antenna;

   always @(posedge clk_x)
     if (rst)
       adr_x <= #DELAY 0;
     else
       adr_x <= #DELAY dsp_en && wrap_cnt ? nxt_x[31:0] : adr_x;



   //-------------------------------------------------------------------------
   //     
   //     DATA-ACQUISITION CONTROL AND READ-BACK.
   //     
   //-------------------------------------------------------------------------
   wire       b_rst = rst;
   wire       aq_valid, aq_debug, aq_shift, aq_count;
   wire       vx_enabled, overwrite;


   //-------------------------------------------------------------------------
   //  Drive the bus signals, for setting the DSP-unit's parameters.
   //-------------------------------------------------------------------------
   //  TODO: Use the 'FRAME' mode to implement burst-transfers?
   wb_transfer
     #( .ASYNC(1), .CHECK(1), .PIPED(1), .READ(1), .WRITE(1),
        .FRAME(1), .BURST(1),
        .DELAY(DELAY)
        ) XFER
       (
        .clk_i(b_clk),
        .rst_i(b_rst),

        .cyc_o(cyc),
        .stb_o(stb),
        .we_o (we ),
        .ack_i(ack),
        .wat_i(wat),
        .rty_i(rty),
        .err_i(err),

        .frame_i(bx),           // burst?
        .read_i (rd),
        .write_i(wr),
        .busy_o (bsy),
        .done_o (fin),
        .fail_o (fail)
        );


   //-------------------------------------------------------------------------
   //  The visibilities are computed by 24 correlators, each with 12x time-
   //  multiplexing, so that 576 correlations are performed for each antenna
   //  sample.
   //-------------------------------------------------------------------------
   tart_dsp
     #( .AXNUM(AXNUM),
        .ACCUM(ACCUM),
        .TRATE(TRATE),
        .TBITS(TBITS),
        .NREAD(NREAD),
        .RBITS(FBITS),
        .XBITS(XBITS),
        .CBITS(CBITS),
        .PIPED(1),
        .CHECK(1),
        .VIZWR(1),               // TODO: test
        .DELAY(DELAY)
        ) TART_DSP
       (
        .clk_i(b_clk),          // bus-domain control signals
        .rst_i(rst),

        .clk_x(clk_x),          // correlator-domain signals
        .vld_x(vld_x),
        .new_x(new_x),
        .sig_x(sig_x),

        .cyc_i(cyc),            // Wishbone interface
        .stb_i(stb),
        .we_i (we ),
        .ack_o(ack),
        .wat_o(wat),
        .rty_o(rty),
        .err_o(err),
        .adr_i(adr),
        .dat_i(val),
        .dat_o(dat),

        .sce_i(read_en),        // enable streaming read-back?

        .enabled_o (enabled ),  // DSP status signals
        .newblock_o(newblock),
        .streamed_o(streamed),
        .switched_o(switched),
        .checksum_o(checksum),

        .bank_o (dsp_bank),
        .stuck_o(dsp_stuck),        // debug signals
        .limp_o (dsp_limp)
        );



   //-------------------------------------------------------------------------
   //     
   //     MOAR DEBUGGING STUFF.
   //     
   //-------------------------------------------------------------------------
   parameter RDMAX = BREAD + 4;

   reg                wb_stuck = 1'b0;
   reg [15:0]         wb_count = 16'h0;
   wire [16:0]        wb_cnext = wb_count + 1;


   //-------------------------------------------------------------------------
   //  Assert 'wb_stuck' if transfers are taking too long.
   //-------------------------------------------------------------------------
   always @(posedge b_clk)
     if (b_rst || fin) begin
        wb_stuck <= #DELAY 0;
        wb_count <= #DELAY 0;
     end
     else if (wb_stuck) begin
        $display("%12t: DSP -> Wishbone bus is stuck", $time);
        $display("\t\tRXD = %5d\n\t\tREQ = %5d", rxd, req);
        $display("\t\tCYC = %5x\n\t\tSTB = %5d", cyc, stb);
        $display("\t\tWE  = %5x\n\t\tACK = %5d", we , ack);
        $display("\t\tADR = %5x\n\t\tDAT = %5d", adr, dat);
        $finish;
     end
     else if (bsy) begin
        wb_stuck <= #DELAY RDMAX == wb_count;
        wb_count <= #DELAY wb_cnext[15:0];
     end

   always @(posedge b_clk)
     if ((get || set) && (rd || wr)) begin
        $display("%12t: Attempting to issue commands while already busy", $time);
        $finish;
     end


endmodule // tart_dsp_tb
