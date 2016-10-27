`timescale 1ns/100ps

// Support CLASSIC Wishbone-like bus transactions?
// `define __WB_CLASSIC
`undef __WB_CLASSIC

module tart_soc_tb;

   parameter WIDTH = 8;         // bus parameters
   parameter MSB   = WIDTH-1;
   parameter ASB   = WIDTH-2;
   parameter DELAY = 3;

   parameter ACCUM = 32;        // correlator/visibilities parameters
   parameter BLOCK = ACCUM;
   parameter BSB   = BLOCK-1;


   //-------------------------------------------------------------------------
   //  Bus signals to the SPI master:
   wire [MSB:0] drx;
   reg [ASB:0]  adr;
   reg [MSB:0]  dtx;
   wire         ack, rdy, wat;
   reg          cyc = 0, stb = 0, we = 0, bst = 0;

   // System signals:
   reg          clk = 1, rst = 0;
   reg          set = 0, get = 0, fin = 0;
   reg [MSB:0]  status;
   wire         oflow, uflow;

   // SPI signals:
   reg          SCK = 1;
   wire         MOSI, MISO, SSEL;
   wire         SCK_pin = SCK_en ? SCK : 1'b0;

   wire         spi_busy, aq_request;
   reg [7:0]    aq_count = 0;
   reg          aq_start = 0, aq_done = 1;
   reg [23:0]   data [0:(1 << MSB)-1];

   wire         aq_debug_mode, aq_enabled;
   wire [2:0]   aq_sample_delay;


   //-------------------------------------------------------------------------
   //  Clocks:
   always #5  clk <= ~clk;
//    always #4  SCK <= ~SCK;      // Fastest without warnings or errors
//    always #6  SCK <= ~SCK;
//    always #8  SCK <= ~SCK;
   always #10 SCK <= ~SCK;
//    always #20 SCK <= ~SCK;


   //-------------------------------------------------------------------------
   //  Simulation sequence:
   integer      num = 0;
   reg [ASB:0]  ptr = 0;
   initial begin : TART_SPI_TB
      $dumpfile ("vcd/soc_tb.vcd");
      $dumpvars;

      for (ptr = 0; ptr < 16; ptr = ptr+1)
        data[ptr] <= $random;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing global reset:\n", $time);
      #33 rst <= 1; #40 rst <= 0;
      #10 status <= 8'hfc; // $random;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing SPI reset:", $time);
      #40 set <= 1; num <= 1; ptr <= 7'h0f; dtx <= 8'h01;
      while (!fin) #10;

      $display("\n%8t: Enabling data acquisition:", $time);
      #120 set <= 1; num <= 1; ptr <= 7'h07; dtx <= 8'h80;
      while (!fin) #10;

      $display("\n%8t: Read back acquisition status:", $time);
      #60 get <= 1; num <= 2; ptr <= 7'h04;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("\n%8t: Reading back acquisition data:", $time);
      #60 get <= 1; num <= 7; ptr <= 7'h00;
      while (!fin) #10;

      $display("\n%8t: Reading back some more acquisition data:", $time);
      #60 get <= 1; num <= 7; ptr <= 7'h00;
      while (!fin) #10;

      $display("\n%8t: Reading back even more acquisition data:", $time);
      #60 get <= 1; num <= 7; ptr <= 7'h00;
      while (!fin) #10;

      //----------------------------------------------------------------------
      #200 $finish;
   end // SPI_SLAVE_TB

   initial begin : SPI_FAILED
      #12000 $display ("TIMEOUT!");
      $finish;
   end // SPI_FAILED


   //-------------------------------------------------------------------------
   //  Display data that has been received from the SPI slave.
   always @(posedge clk)
     if (rdy) begin
        $display ("%10t: SPI data = %08b (0x%02x)", $time, drx, drx);
     end

   wire [MSB:0] dat = |{rdys, rdy} ? drx : 'bz;
   reg [3:0]    rdys = 0;

   always @(posedge clk)
     rdys <= #DELAY {rdys[2:0], rdy};
   

   //-------------------------------------------------------------------------
   //  Generate WB-like transactions.
   //-------------------------------------------------------------------------
   integer cnt;
   wire    cyc_n = cyc && cnt == 0 && rdy;

   always @(posedge clk)
     if (rst) bst <= #DELAY 0;
     else if ((set || get) && num > 1) bst <= #DELAY 1;
     else if (bst && num == 1 && !wat) bst <= #DELAY 0;

   always @(posedge clk)
     if (rst) begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 0;
     end
     else if (set) begin
        $display("%8t: write beginning (num = %1d)", $time, num);
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 7;
     end
     else if (get) begin
        $display("%8t: read beginning (num = %1d)", $time, num);
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 6;
     end
     else if (cyc) begin
        if (!stb && ack) $display("%8t: transfer ending", $time);
        {fin, get, set} <= #DELAY {cyc_n, get, set};
        {cyc, stb, we } <= #DELAY {!cyc_n, num > 0 && bst, we && num > 0};
     end
     else begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 0;
     end

   wire [ASB:0] next_adr = bst && !wat ? adr + 1 : adr;

   always @(posedge clk)
     if (rst)             adr <= #DELAY 0;
     else if (set || get) adr <= #DELAY ptr[6:0];
     else if (cyc)        adr <= #DELAY next_adr;

   always @(posedge clk)
     if (cyc && stb && !wat) num <= #DELAY num - 1;

   always @(posedge clk)
     if (set || get) cnt <= #DELAY num;
     else if (rdy)   cnt <= #DELAY cnt - 1;


   //-------------------------------------------------------------------------
   //  Generate fake DRAM contents.
   wire [23:0] data_w = data[data_index];
   integer     data_index = 0;
   reg         ready = 0;

   always @(posedge clk)
     if (rst)
       {aq_done, aq_start} <= #DELAY 2'b10;
     else if (!aq_start && aq_done && aq_enabled)
       {aq_done, aq_start} <= #DELAY 2'b01;
     else if (!aq_done && aq_start && aq_enabled)
       aq_start <= #DELAY 0;
     else
       aq_done  <= #DELAY aq_done ? aq_done : !aq_enabled;

   always @(posedge clk)
     if (rst)
       ready <= #DELAY 0;
     else if (aq_start || aq_request)
       ready <= #DELAY 1;
     else
       ready <= #DELAY 0;

   always @(posedge clk)
     if (rst)
       data_index <= #DELAY 0;
     else if (aq_request)
       data_index <= #DELAY data_index + 1;
     else
       data_index <= #DELAY data_index;


   //-------------------------------------------------------------------------
   //  
   //  Devices Under Test (DUT's).
   //  
   //-------------------------------------------------------------------------
   wire        SCK_en;

   //  Generate SPI transactions, for testing.
   spi_master SPI_MASTER0
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_i(cyc),
       .stb_i(stb),
       .we_i (we ),
       .bst_i(bst),
       .ack_o(ack),
       .wat_o(wat),
       .rdy_o(rdy),
       .adr_i(adr),
       .dat_i(dtx),
       .dat_o(drx),
       
       .SCK(SCK),
       .SCK_enable(SCK_en),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );


   wire [MSB:0] b_dtx, b_drx;   // bus master's signals
   wire [ASB:0] b_adr;
   wire         b_clk = clk;
   wire         b_rst = rst || reset;
   wire         b_cyc, b_stb, b_we;
   wire         b_ack, b_wat, b_rty, b_err;

   wire [MSB:0] r_drx, r_dtx;   // reset handler's signals
   wire         r_stb, r_ack;

   wire [MSB:0] a_drx, a_dtx;   // data-acquisition controller's signals
   wire [3:0]   a_adr = b_adr[3:0];
   wire         a_stb, a_ack;

   reg          r_sel = 0, a_sel = 0;

   //-------------------------------------------------------------------------
   //  Connections to slave devices:
   assign r_dtx = b_drx;        // redirect output-data to slaves
   assign a_dtx = b_drx;

   assign a_stb = b_adr[6:3] == 4'h0 && b_stb; // decoder for acquire
//    assign r_stb = b_adr == 7'h0f && b_stb; // address decoder for reset unit
   assign r_stb = b_adr[6:2] == 5'h03 && b_stb; // address decoder for reset unit

   //-------------------------------------------------------------------------
   //  Keep the selected device active until the transaction has been
   //  acknowledged.
   always @(posedge b_clk)
     if (b_rst || !b_cyc)
       {a_sel, r_sel} <= #DELAY 2'b00;
     else begin
        r_sel <= #DELAY r_sel ? !r_ack || r_stb : r_stb;
        a_sel <= #DELAY a_sel ? !a_ack || a_stb : a_stb;
     end


   //-------------------------------------------------------------------------
   //     SPI SLAVE
   //-------------------------------------------------------------------------
   assign b_ack = r_ack || a_ack;
`ifdef __WB_CLASSIC
   assign b_dtx = r_stb ? r_drx : (a_stb ? a_drx : 'bz);
`else
   assign b_dtx = r_stb || r_sel ? r_drx : (a_stb || a_sel ? a_drx : 'bz);
`endif

   assign b_wat = 1'b0;
   assign b_rty = 1'b0;
   assign b_err = 1'b0;


`define __USE_SLAVE_WB
`ifdef  __USE_SLAVE_WB
   assign b_cyc = n_cyc;
   assign b_stb = n_stb;
   assign b_we  = n_we ;
   assign b_adr = n_adr;
   assign b_drx = n_drx;

   assign spi_busy = n_spi_busy;
   assign oflow = n_oflow;
   assign uflow = n_uflow;
   assign MISO = n_MISO;

`else // !`ifdef __USE_SLAVE_WB
   assign b_cyc = o_cyc;
   assign b_stb = o_stb;
   assign b_we  = o_we ;
   assign b_adr = o_adr;
   assign b_drx = o_drx;

   assign spi_busy = o_spi_busy;
   assign oflow = o_oflow;
   assign uflow = o_uflow;
   assign MISO = o_MISO;

`endif // !`ifdef __USE_SLAVE_WB


   //-------------------------------------------------------------------------
   //     NEW SPI SLAVE
   //-------------------------------------------------------------------------
   wire [MSB:0] n_drx;   // new slave's signals
   wire [ASB:0] n_adr;
   wire         n_cyc, n_stb, n_we;
   wire         n_spi_busy, n_oflow, n_uflow, n_MISO;

   spi_slave_wb #( .WIDTH(WIDTH) ) NEW_SLAVE0
     ( .clk_i(b_clk),
       .rst_i(b_rst),
       .cyc_o(n_cyc),
       .stb_o(n_stb),
       .we_o (n_we),
       .ack_i(b_ack),
       .wat_i(b_wat),
       .rty_i(b_rty),
       .err_i(b_err),
       .adr_o(n_adr),
       .dat_i(b_dtx),
       .dat_o(n_drx),

       .active_o(n_spi_busy),
       .status_i(status),
       .overflow_o(n_oflow),
       .underrun_o(n_uflow),
       
       .SCK_pin(SCK_pin),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(n_MISO)
       );


   //-------------------------------------------------------------------------
   //     OLD SPI SLAVE
   //-------------------------------------------------------------------------
   wire [MSB:0] o_drx;   // old slave's signals
   wire [ASB:0] o_adr;
   wire         o_rst, o_SCK;
   wire         o_cyc, o_stb, o_we;
   wire         o_spi_busy, o_oflow, o_uflow, o_MISO;

   reg          o_ack = 1'b0;

`ifdef __USE_SLAVE_WB
   assign o_rst = 1'b1;
   assign SCK_o = 1'b1;
`else
   assign o_rst = b_rst;
   assign o_SCK = SCP_pin;
`endif
   
   always @(posedge b_clk)
     o_ack <= #DELAY b_ack;

   spi_slave #( .WIDTH(WIDTH) ) OLD_SLAVE0
     ( .clk_i(b_clk),
       .rst_i(o_rst),
       .cyc_o(o_cyc),
       .stb_o(o_stb),
       .we_o (o_we),
       .ack_i(o_ack),
       .adr_o(o_adr),
       .dat_i(b_dtx),
       .dat_o(o_drx),

       .active_o(o_spi_busy),
       .status_i(status),
       .overflow_o(o_oflow),
       .underrun_o(o_uflow),
       
       .SCK_pin(o_SCK),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(o_MISO)
       );


   //-------------------------------------------------------------------------
   //     RESET HANDLER
   //-------------------------------------------------------------------------
   wire         reset;
   wire         reset_n = 1'b1;
   wire [23:0]  checksum;
   wire [MSB:0] spi_status = status;

   tart_control #( .WIDTH(WIDTH), .COUNT(24) ) WB_RESET0
     ( .clk_i(b_clk),
       .rst_i(b_rst),
       .cyc_i(b_cyc),
       .stb_i(r_stb),
       .we_i (b_we),
       .ack_o(r_ack),
       .adr_i(b_adr[1:0]),
       .dat_i(r_dtx),
       .dat_o(r_drx),

       .status_i(spi_status),
       .checksum_i(checksum),
       .reset_ni(reset_n),
       .reset_o (reset)
       );


   //-------------------------------------------------------------------------
   //     DATA-ACQUISITION CONTROL AND READ-BACK.
   //-------------------------------------------------------------------------
   wire         s_cyc, s_stb, s_we, s_wat, s_rty, s_err;
   wire [3:0]   s_adr;
   wire [MSB:0] s_dat = {WIDTH{1'b1}};
   wire         accessed, overflow, newblock, streamed, available, overwrite;
   wire         vx_enabled;
   wire         limp, stuck;
   wire [23:0]  blocksize = 31;
   reg          s_ack = 1'b0;

   assign s_wat = 1'b0;
   assign s_rty = 1'b0;
   assign s_err = 1'b0;

   always @(posedge b_clk)
     s_ack <= #DELAY s_cyc && s_stb;

   tart_acquire
     #( .WIDTH(WIDTH)
        ) TART_ACQUIRE0
       (
        .clk_i(b_clk),
        .rst_i(reset),
        .cyc_i(b_cyc),
        .stb_i(a_stb),
        .we_i (b_we),
        .ack_o(a_ack),
        .adr_i(a_adr),
        .dat_i(a_dtx),
        .dat_o(a_drx),

        .spi_busy(spi_busy),

        //  Visibilities status & data access:
        .vx_cyc_o(s_cyc),     // WB-like bus for visibilities read-back
        .vx_stb_o(s_stb),
        .vx_we_o (s_we ),
        .vx_ack_i(s_ack),
        .vx_wat_i(s_wat),
        .vx_rty_i(s_rty),
        .vx_err_i(s_err),
        .vx_adr_o(s_adr),
        .vx_dat_i(s_dat),

        .overflow (overflow),
        .newblock (newblock), // strobes when new block ready
        .streamed (streamed), // has an entire block finished streaming?
        .accessed (accessed), // asserts once visibilities are read back
        .available(available),
        .checksum (checksum),
        .blocksize(blocksize),

        .vx_enabled(vx_enabled),
        .vx_overwrite(overwrite), // overwrite when buffers full?
        .vx_stuck_i(stuck),
        .vx_limp_i (limp),

        .data_ready(ready),
        .data_request(aq_request),
        .data_in(data_w),

        .aq_debug_mode(aq_debug_mode),
        .aq_enabled(aq_enabled),
        .aq_sample_delay(aq_sample_delay),
        .aq_adr_i(25'h1234567)
        );


endmodule // tart_soc_tb
