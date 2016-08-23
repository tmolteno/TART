`timescale 1ns/100ps

`include "tartcfg.v"

module tart_dsp_tb;

   parameter BLOCK = `ACCUM_BITS;        // Number of bits of a block
   parameter MSB   = BLOCK-1;
   parameter ACCUM = BLOCK;     // Bit-width of the accumulators
   parameter ABITS = 12;        // Address bit-width
   parameter ASB   = ABITS-1;
   parameter BBITS = 8;
   parameter BSB   = BBITS-1;
   parameter MRATE = 12;
   parameter DELAY = 3;

   parameter COUNT = 4; // count down from:  (1 << COUNT) - 1;
//    parameter COUNT = 9; // count down from:  (1 << COUNT) - 1;
//    parameter COUNT = 12; // count down from:  (1 << COUNT) - 1;
   parameter NREAD = 8;
//    parameter NREAD = 24;
//    parameter NREAD = 96;
//    parameter NREAD = 120;
//    parameter NREAD = `READ_COUNT >> 2;
//    parameter NREAD = `READ_COUNT;

   parameter BREAD = NREAD << 2;
   parameter XBITS = `BLOCK_BITS; // Bit-width of the block-counter
   parameter XSB   = XBITS-1;     // MSB of the block-counter
`ifdef __USE_SDP_DSRAM
   parameter CBITS = 14;
`else
   parameter CBITS = 10;
`endif
   parameter CSB   = CBITS-1;

   wire [MSB:0] c_dat, c_val, blocksize, checksum;
   wire [CSB:0] c_adr;
   wire [BSB:0] dat, val, drx;
   reg          clk_x = 1, b_clk = 1, rst = 0;
   reg          cyc = 0, stb = 0, we = 0, bst = 0;
   reg [2:0]    adr;
   reg [BSB:0]  dtx;
   reg          set = 0, get = 0, fin = 0;
   wire         ack;
   wire         c_cyc, c_stb, c_we, c_bst, c_ack;
   reg [23:0]   data [0:255];
   reg [31:0]   viz = 32'h0;

   assign dsp_en = aq_enabled;


   //-------------------------------------------------------------------------
   //  Setup correlator and bus clocks, respectively.
   always #5  clk_x <= ~clk_x;
   always #5  b_clk <= ~b_clk;
//    always #10 b_clk <= ~b_clk;


   //-------------------------------------------------------------------------
   //  Simulate two visibility calculations.
   integer      num = 0;
   integer      ptr = 0;
   initial begin : SIM_BLOCK
      if (COUNT < 6) begin
         $dumpfile ("dsp_tb.vcd");
         $dumpvars;
      end

      //----------------------------------------------------------------------
      $display("\n%12t: Generating fake antenna data:", $time);
      $display("%12t: (Data is just increasing counter values)", $time);
      for (ptr = 0; ptr < 256; ptr = ptr+1)
        data[ptr] <= ptr;
//         data[ptr] <= $random;

      //----------------------------------------------------------------------
      #20 $display("\n%12t: Issuing RESET:\n", $time);
      #13 rst <= 1; #40 rst <= 0;

      //----------------------------------------------------------------------
      $display("\n%12t: Setting up the block-size:", $time);
      #40 set <= 1; num <= 1; dtx <= COUNT; ptr <= 3'h5;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("%12t: Beginning data-correlation (bank 0):", $time);
      #40 set <= 1; num <= 1; dtx <= 8'h01; ptr <= 3'h7;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("%12t: Switching banks (bank 1):", $time);
      while (!switching) #10;
      while (switching) #10;

      //----------------------------------------------------------------------
      while (!newblock) #10;
      #10 $display("\n%12t: Reading back visibilities (bank 0):", $time);
      #10 get <= 1; num <= BREAD; ptr <= 3'h4;
      while (!fin) #10;

      //----------------------------------------------------------------------
      while (!switching) #10; while (switching) #10;
      $display("\n%12t: Stopping data-correlation (bank 1):", $time);
      #10 set <= 1; num <= 1; dtx <= 8'h00; ptr <= 3'h7;
      while (!fin) #10;

      //----------------------------------------------------------------------
      while (!newblock) #10;
      #10 $display("\n%12t: Reading back visibilities (bank 1):", $time);
      #10 get <= 1; num <= BREAD; ptr <= 3'h4;
      while (!fin) #10;

      //----------------------------------------------------------------------
//       $display("\n%12t: Reading back counts (bank 1):", $time);
//       #80 get <= 1;
//       while (!fin) #10;

      //----------------------------------------------------------------------
      #80 $display("\n%12t: Simulation finished:", $time);
      $finish;
   end


   //-------------------------------------------------------------------------
   //  Exit if the simulation appears to have stalled.
   //-------------------------------------------------------------------------
   parameter LIMIT = 1000 + (1 << COUNT) * 300;

   initial begin : SIM_FAILED
      $display("%12t: Simulation TIMEOUT limit:\t%12d", $time, LIMIT);
      #LIMIT $display ("\nTIMEOUT!\n");
      $finish;

//       if (COUNT < 6) begin
//          #12000 $display ("TIMEOUT!");
//          $finish;
//       end
   end // SIM_FAILED

   always @(posedge b_clk)
     if (newblock)
       $display("%12t: New block available.", $time);

   /*
   initial begin
      $dumpfile ("dsp_tb.vcd");
      #60000 $dumpvars;
      #4000  $finish;
   end
    */


   //-------------------------------------------------------------------------
   //  Read back visibility data, from the correlators' registers.
   //-------------------------------------------------------------------------
   wire       cyc_n = cyc && rxd == 1 && ack;
   integer    rxd = 0;

`ifndef __WB_CLASSIC
   always @(posedge b_clk)
     if (rst) bst <= #DELAY 0;
     else if ((set || get) && num > 2) bst <= #DELAY 1;
     else if (bst && num == 2 && !wat) bst <= #DELAY 0;
`endif

   always @(posedge b_clk)
     if (rst) begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 0;
     end
     else if (set) begin
        $display("%12t: write beginning (num = %1d)", $time, num);
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 7;
     end
     else if (get) begin
        $display("%12t: read beginning (num = %1d)", $time, num);
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 6;
     end
     else if (cyc) begin
        if (!stb && ack) $display("%12t: transfer ending", $time);
`ifdef __WB_CLASSIC
        {fin, get, set} <= #DELAY { cyc_n, get, set};
        {cyc, stb, we } <= #DELAY {!cyc_n, !cyc_n, we && !cyc_n};
`else
        {fin, get, set} <= #DELAY { cyc_n, get, set};
        {cyc, stb, we } <= #DELAY {!cyc_n, bst || wat, we && !cyc_n};
`endif
     end
     else begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 0;
     end

   always @(posedge b_clk)
     if (set || get) adr <= #DELAY ptr;

`ifdef __WB_CLASSIC
   always @(posedge b_clk)
     if (cyc && stb && ack) num <= #DELAY num - 1;
`else
   always @(posedge b_clk)
     if (cyc && stb && !wat) num <= #DELAY num - 1;
`endif // __WB_CLASSIC

   always @(posedge b_clk)
     if (get || set) rxd <= num;
     else if (cyc && ack) rxd <= #DELAY rxd - 1;

   //-------------------------------------------------------------------------
   // Display the data, and which correlator and register it is from.
   reg [10:0]   adr_r;
   reg [5:0]    ci;
   reg [4:0]    ri;
   reg [3:0]    rdys = 0, vals = 0;
   wire         rdy = cyc && !we && ack;

//    assign dat = |{rdys, rdy} ? drx : 'bz;
//    assign val = |{vals, set} ? dtx : 'bz;
   wire [2:0]   dst = cyc ? adr : 'bz;
   assign val = cyc && we ? dtx : 'bz;
//    assign dat = rdy || (|rdys) && cyc ? drx : 'bz;
   assign dat = rdy || rdys[0] ? drx : 'bz;

   always @(posedge b_clk) begin
      rdys <= #DELAY {rdys[2:0], rdy};
      vals <= #DELAY {vals[2:0], set};
   end

   reg [1:0] rxc = 0;
   always @(posedge b_clk) begin
      adr_r    <= #DELAY rd_adr;
      {ci, ri} <= #DELAY adr_r;
      if (cyc && stb && !we && ack) begin
         rxc <= #DELAY rxc + 1;
         viz = {dat, viz[31:8]};
         if (rxc == 2'b11)
           $display("%12t: Vis = %08x (c: %02x, r:%02x)", $time, viz, ci, ri);
      end
   end


   //-------------------------------------------------------------------------
   //  Generate fake DRAM contents.
   //-------------------------------------------------------------------------
   wire [23:0] data_w = data[data_index[7:0]];
   integer     data_index = 0;
   reg         ready = 0;
   reg         aq_start = 0, aq_done = 1;
   wire        spi_busy, aq_request;
   wire [2:0]  aq_sample_delay;

   always @(posedge b_clk)
     if (rst)
       {aq_done, aq_start} <= #DELAY 2'b10;
     else if (!aq_start && aq_done && aq_enabled)
       {aq_done, aq_start} <= #DELAY 2'b01;
     else if (!aq_done && aq_start && aq_enabled)
       aq_start <= #DELAY 0;
     else
       aq_done  <= #DELAY aq_done ? aq_done : !aq_enabled;

   always @(posedge b_clk)
     if (rst)
       ready <= #DELAY 0;
     else if (aq_start || aq_request)
       ready <= #DELAY 1;
     else
       ready <= #DELAY 0;

   always @(posedge b_clk)
     if (rst)
       data_index <= #DELAY 0;
     else if (aq_request)
       data_index <= #DELAY data_index + 1;
     else
       data_index <= #DELAY data_index;

   //-------------------------------------------------------------------------
   //  Generate fake antenna data, from the fake DRAM contents.
   wire [23:0] antenna;
   reg [3:0]  cnt = 0;
   wire [3:0] next_cnt = wrap_cnt ? 0 : cnt + 1 ;
   wire       wrap_cnt = cnt == MRATE-1;
   integer    rd_adr = 0;

   assign antenna = data[rd_adr[7:0]];

   always @(posedge clk_x)
     if (rst) cnt <= #DELAY 0;
     else     cnt <= #DELAY dsp_en ? next_cnt : cnt;

   always @(posedge clk_x)
     if (rst) rd_adr <= #DELAY 0;
     else     rd_adr <= #DELAY dsp_en && wrap_cnt ? rd_adr + 1 : rd_adr;


   //-------------------------------------------------------------------------
   //     DATA-ACQUISITION CONTROL AND READ-BACK.
   //-------------------------------------------------------------------------
   wire [BSB:0] s_dat;
   wire [XSB:0] v_blk;
   wire         s_cyc, s_stb, s_we, s_ack;
   reg          wat = 0;
   wire         newblock, streamed, accessed, available, switching;
   wire         aq_debug_mode, aq_enabled;

   wire         dsp_cyc, dsp_stb, dsp_we, dsp_bst, dsp_ack;
   wire [XSB:0] dsp_blk;
   wire [7:0]   dsp_dat, dsp_val;

   assign dsp_bst = 1'b0;

   always @(posedge b_clk)
     wat <= #DELAY stb && bst && !ack;


   tart_acquire
     #( .WIDTH(BBITS), .ACCUM(ACCUM), .BBITS(XBITS)
        ) TART_ACQUIRE0
     ( .clk_i(b_clk),
       .rst_i(rst),
       .cyc_i(cyc),
       .stb_i(stb),
       .we_i (we),
       .ack_o(ack),
       .adr_i(adr),
       .dat_i(dtx),
       .dat_o(drx),

       .data_ready(ready),
       .data_request(aq_request),
       .data_in(data_w),

       .spi_busy(spi_busy),

       .vx_cyc_o(dsp_cyc),
       .vx_stb_o(dsp_stb),
       .vx_we_o (dsp_we ),
       .vx_ack_i(dsp_ack),
       .vx_blk_o(dsp_blk),
       .vx_dat_i(dsp_dat),
       .newblock(newblock),
       .streamed(streamed), // has an entire block finished streaming?
       .accessed(accessed),
       .available(available),
       .checksum(checksum),
       .blocksize(blocksize),

       .aq_debug_mode(aq_debug_mode),
       .aq_enabled(aq_enabled),
       .aq_sample_delay(aq_sample_delay)
       );

   tart_dsp
     #(.NREAD(NREAD)
       ) TART_DSP
     ( .clk_x(clk_x),
       .rst_i(rst),
       .aq_clk_i(b_clk),
       .aq_cyc_i(dsp_cyc),
       .aq_stb_i(dsp_stb),
       .aq_we_i (dsp_we ),
       .aq_bst_i(dsp_bst),
       .aq_ack_o(dsp_ack),
       .aq_blk_i(dsp_blk),
       .aq_dat_i(dsp_val),
       .aq_dat_o(dsp_dat),

       .aq_enable(aq_enabled),
       .antenna  (antenna),
       .switching(switching),
       .blocksize(blocksize),

       .newblock (newblock),
       .checksum (checksum),
       .streamed (streamed)
       );


endmodule // tart_dsp_tb
