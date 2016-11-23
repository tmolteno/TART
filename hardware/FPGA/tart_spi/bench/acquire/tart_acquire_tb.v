`timescale 1ns/100ps
/*
 * Module      : bench/tart_acquire_tb.v
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

module tart_acquire_tb;

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
   parameter COUNT = 6; // count down from:  (1 << COUNT) - 1;
//    parameter COUNT = 10; // count down from:  (1 << COUNT) - 1;
//    parameter COUNT = 12; // count down from:  (1 << COUNT) - 1;
`ifdef __USE_FAKE_DSP
   parameter NREAD = 9;
`else
//    parameter NREAD = 48;
//    parameter NREAD = 144;
   parameter NREAD = 576;
`endif
   parameter READS = 2;         // Banks of visibilities to read

   //  Additional simulation settings:
   parameter RNG   = `RANDOM_DATA; // Use random antenna data?
   parameter DELAY = `DELAY;       // Simulated combinational delay


   //-------------------------------------------------------------------------
   //
   //  Signals.
   //
   //-------------------------------------------------------------------------
   wire [MSB:0] blocksize, checksum;
   wire [BSB:0] dat, val, drx;
   reg          clk_x = 1'b1, b_clk = 1'b1, rst = 1'b0;
   reg          cyc = 1'b0, stb = 1'b0, we = 1'b0;
   reg [3:0]    adr;
   reg [BSB:0]  dtx;
   reg          set = 1'b0, get = 1'b0, fin = 1'b0;
   wire         dsp_en, stuck, limp, ack, wat, rty, err;
   reg [NSB:0]  data [0:255];
   reg [31:0]   viz = 32'h0;
   reg [4:0]    log_bsize = COUNT[4:0];
   reg [XSB:0]  bank = {XBITS{1'b0}};
   wire [XSB:0] vx_bank;
   reg          busy = 1'b0;
   wire [2:0]   cap_state;


   assign dsp_en = vx_enabled;


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
      if (COUNT < 7) begin
`ifdef __USE_FAKE_DSP
         $dumpfile ("vcd/fake_tb.vcd");
`else
         $dumpfile ("vcd/dsp_tb.vcd");
`endif
         $dumpvars;
      end

      //----------------------------------------------------------------------
      $display("\n%12t: TART DSP settings:", $time);
      $display(  "%12t:  TART I/O bus settings:", $time);
      $display(  "%12t:   SPI data-bus bit-width:  \t\t%4d", $time, BBITS);
      $display(  "%12t:   Number of visibilities banks:    \t%4d", $time, XBITS);
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
      #13 b_rst <= 1; #40 b_rst <= 0;

      //----------------------------------------------------------------------
      $display("%12t: Setting block-size (2^%1d)", $time, log_bsize);
      $display("%12t: Beginning correlation (bank %1d)", $time, bank);
      #40 set <= 1; num <= 1; dtx <= {1'b1, 2'b00, log_bsize}; ptr <= 4'ha;
      while (!fin) #10;

      //----------------------------------------------------------------------
      //  Fill two banks with visibilities, then stop.
      while (vx_bank < READS) begin
         while (!switching) #10; while (switching) #10;
         $display("%12t: Bank switched (to bank %1d)", $time, vx_bank);
      end

      while (busy) #10;
      $display("%12t: Stopping data-correlation.", $time);
      #10 set <= 1; num <= 1; dtx <= 8'h00; ptr <= 4'ha;
      while (!fin) #10;

      $display("%12t: Waiting for all visibilities to be fetched.", $time);
      #10 while (bank < READS) #80;

      //----------------------------------------------------------------------
      #80 $display("%12t: Simulation finished:", $time);
      $finish;
   end // block: SIM_BLOCK


   //-------------------------------------------------------------------------
   //  Trigger visibilities read-backs.
   //-------------------------------------------------------------------------
   initial begin : SIM_READ
      while (1'b1) begin
         while (!newblock) #10;
         while (busy) #10;
         #10 $display("%12t: Reading back visibilities (bank %1d)", $time, bank);
         #10 get <= 1; num <= BREAD; ptr <= 4'h8;
         while (!fin) #10;
         bank <= bank + 1;
      end
   end // block: SIM_READ


   //-------------------------------------------------------------------------
   //  Exit if the simulation appears to have stalled.
   //-------------------------------------------------------------------------
   parameter LIMIT = 1000 + (1 << COUNT) * 320 + NREAD * 80;

   initial begin : SIM_FAILED
      $display("%12t: Simulation TIMEOUT limit:\t%12d", $time, LIMIT);
      #LIMIT $display ("\nTIMEOUT!\n");
      $finish;
   end // SIM_FAILED


   //-------------------------------------------------------------------------
   //  Display simulation progress waypoints.
   //-------------------------------------------------------------------------
   always @(posedge b_clk)
     if (newblock)
       $display("%12t: New block available.", $time);

   always @(posedge b_clk)
     if (streamed)
       $display("%12t: Block streamed (bank = %2d).", $time, bank);


   //-------------------------------------------------------------------------
   //  
   //  SIMULATED WISHBONE BUS MASTER.
   //  
   //-------------------------------------------------------------------------
   wire       cyc_n = cyc && rxd == 1 && ack;
   wire       stb_w = stb && (rxd > 2 || wat);
   integer    rxd = 0;

   //-------------------------------------------------------------------------
   //  The `busy` flag is so that other bus-masters can tell if the another
   //  master is using the bus.
   always @(posedge b_clk)
     if (b_rst || fin)
       busy <= #DELAY 1'b0;
     else if (get || set)
       busy <= #DELAY 1'b1;
     else
       busy <= #DELAY busy;

   always @(posedge b_clk)
     if (b_rst) begin
        {fin, get, set} <= #DELAY 3'h0;
        {cyc, stb, we } <= #DELAY 3'h0;
     end
     else if (set) begin
        $display("%12t: WB write beginning (bytes = %1d)", $time, num);
        {fin, get, set} <= #DELAY 3'h0;
        {cyc, stb, we } <= #DELAY 3'h7;
     end
     else if (get) begin
        $display("%12t: WB read beginning (bytes = %1d)", $time, num);
        {fin, get, set} <= #DELAY 3'h0;
        {cyc, stb, we } <= #DELAY 3'h6;
     end
     else if (cyc) begin
`ifdef __WB_CLASSIC
        {fin, get, set} <= #DELAY { cyc_n, get, set};
        {cyc, stb, we } <= #DELAY {!cyc_n, !cyc_n, we && !cyc_n};
`else
        if (!stb && ack) $display("%12t: WB transfer ending", $time);
        {fin, get, set} <= #DELAY { cyc_n, get, set};
        {cyc, stb, we } <= #DELAY {!cyc_n, stb_w, we && !cyc_n};
`endif
     end
     else begin
        {fin, get, set} <= #DELAY 3'h0;
        {cyc, stb, we } <= #DELAY 3'h0;
     end

   always @(posedge b_clk)
     if (set || get) adr <= #DELAY ptr;

   always @(posedge fin)
     $display("%12t: WB transfer ending", $time);

`ifdef __WB_CLASSIC
   always @(posedge b_clk)
     if (cyc && stb && ack) num <= #DELAY num - 1;
`else
   always @(posedge b_clk)
     if (cyc && stb && !wat) num <= #DELAY num - 1;
`endif // __WB_CLASSIC

   always @(posedge b_clk)
     if (get || set)      rxd <= #DELAY num;
     else if (cyc && ack) rxd <= #DELAY rxd - 1;


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

   wire [2:0]     dst = cyc ? adr : 'bz;
   wire           rdy = cyc && !we && ack;

   assign val = cyc && we ? dtx : 'bz;
   assign dat = rdy ? drx : 'bz;


   //-------------------------------------------------------------------------
   //  Assemble bytes into words, and stored them until the transfer has been
   //  completed.
   always @(posedge b_clk)
     if (b_rst || fin) begin
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
   //  Generate fake antenna data, from the fake DRAM contents.
   wire [NSB:0] antenna;
   reg [3:0]  cnt = 0;
   wire [3:0] next_cnt = wrap_cnt ? 0 : cnt + 1 ;
   wire       wrap_cnt = cnt == TRATE-1;
   integer    rd_adr = 0;

   assign antenna = data[rd_adr[7:0]];

   always @(posedge clk_x)
     if (b_rst)
       cnt <= #DELAY 0;
     else
       cnt <= #DELAY dsp_en ? next_cnt : cnt;

   always @(posedge clk_x)
     if (b_rst)
       rd_adr <= #DELAY 0;
     else
       rd_adr <= #DELAY dsp_en && wrap_cnt ? rd_adr + 1 : rd_adr;



   //-------------------------------------------------------------------------
   //     
   //     DATA-ACQUISITION CONTROL AND READ-BACK.
   //     
   //-------------------------------------------------------------------------
   wire         aq_enabled;
   wire         rst_x = b_rst;


   //-------------------------------------------------------------------------
   //  Controls raw-data aquisition.
   //-------------------------------------------------------------------------
   tart_acquire
     #( .AXNUM(AXNUM),
        .BBITS(BBITS),
        .DELAY(DELAY)
        ) ACQ
       (
        .clk_i(b_clk),
        .rst_i(b_rst),

        .clock_x (clk_x),
        .reset_x (rst_x),
        .strobe_x(new_x),
        .signal_x(sig_x),

        //  Wishbone (SPEC B4) interconnect.
        .cyc_i    (cyc),
        .stb_i    (stb),
        .we_i     (we),
        .ack_o    (ack),
        .wat_o    (wat),
        .rty_o    (rty),
        .err_o    (err),
        .adr_i    (adr),
        .dat_i    (dtx),
        .dat_o    (drx),

        .io_busy_i(spi_busy),

        //  Memory controller signals (bus-domain).
        .mcb_ce_o (cmd_enable),
        .mcb_wr_o (cmd_write),
        .mcb_rdy_i(cmd_ready),
        .mcb_ack_i(data_out_ready),
        .mcb_adr_o(cmd_address),
        .mcb_dat_i(data_out),
        .mcb_dat_o(cmd_data_in),

        .enabled_o(aq_enabled),
        .state_o  (cap_state)
        );


   //-------------------------------------------------------------------------
   //  Fake memory-controller.
   //-------------------------------------------------------------------------
   mcb_dummy
     #( .WIDTH(32), .ABITS(10)
        ) MCB
       (
        .clock_i  (b_clk),
        .reset_i  (b_rst),
        .active_o (cmd_ready),
        .request_i(cmd_enable),
        .write_i  (cmd_write),
        .ready_o  (data_out_ready),
        .address_i(cmd_address[9:0]),
        .bytes_i  (4'b1111),
        .data_i   (cmd_data_in),
        .data_o   (data_out)
        );


endmodule // tart_acquire_tb
