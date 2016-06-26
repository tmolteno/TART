`timescale 1ns/1ps
module tart_spi_tb;

   parameter WIDTH = 8;
   parameter MSB   = WIDTH-1;
   parameter ASB   = WIDTH-2;
   parameter DELAY = 3;

   // Bus signals:
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

   wire         request, spi_reset, spi_start, spi_debug;
   wire [2:0]   sdelay;

   reg [7:0]    aq_count = 0;
   reg          aq_start = 0, aq_done = 1;
   reg [23:0]   data [0:(1 << MSB)];


   //-------------------------------------------------------------------------
   //  Clocks:
   always #5  clk <= ~clk;
//    always #6  SCK <= ~SCK;      // Fastest without warnings or errors
//    always #8  SCK <= ~SCK;
   always #10 SCK <= ~SCK;
//    always #20 SCK <= ~SCK;


   //-------------------------------------------------------------------------
   //  Simulation sequence:
   integer      num = 0;
   reg [ASB:0]  ptr = 0;
   initial begin : TART_SPI_TB
      $dumpfile ("tart_tb.vcd");
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

      $display("\n%8t: Enabling data aquisition:", $time);
      #120 set <= 1; num <= 1; ptr <= 7'h07; dtx <= 8'h01;
      while (!fin) #10;

      $display("\n%8t: Read back aquisition status:", $time);
      #60 get <= 1; num <= 2; ptr <= 7'h07;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("\n%8t: Reading back aquisition data:", $time);
      #60 get <= 1; num <= 7; ptr <= 7'h00;
      while (!fin) #10;

      $display("\n%8t: Reading back some more aquisition data:", $time);
      #60 get <= 1; num <= 7; ptr <= 7'h00;
      while (!fin) #10;

      $display("\n%8t: Reading back even more aquisition data:", $time);
      #60 get <= 1; num <= 7; ptr <= 7'h00;
      while (!fin) #10;

      /*
      //----------------------------------------------------------------------
      $display("\n%8t: Burst write:", $time);
      #40 set <= 1; num <= 4; ptr <= $random;
      while (!fin) #10;

      $display("\n%8t: Burst read:", $time);
      #40 get <= 1; num <= 3;
      while (!fin) #10;

      $display("\n%8t: Burst read:", $time);
      ptr <= ptr + 2;
      #10 get <= 1; num <= 3;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("\n%10t: Issue a reset command to the slave device:", $time);
      #30  cyc  <= 1; stb <= 1;
      data_tx <= {1'b1, 3'b000, 4'b1111};
      while (!ack) #10 ;
      
      #10 data_tx <= 8'b00000001;
      while (!ack) #10 ;
      #10 stb <= 0;
      while (!rdy) #10 ;
      #10 while (!rdy) #10 ;
      cyc <= 0; #60 ;

      //----------------------------------------------------------------------
      $display("\n%10t: Set the aquisition mode:", $time);
      #30  cyc  <= 1; stb <= 1;
      data_tx <= {1'b1, 3'b000, 4'b0001};
      while (!ack) #10 ;
      
      #10 data_tx <= 8'b00000001;
      while (!ack) #10 ;
      #10 stb <= 0;
      while (!rdy) #10 ;
      #10 while (!rdy) #10 ;
      cyc <= 0; #60 ;

      //----------------------------------------------------------------------
      $display("\n%10t: Read back the aquisition mode:", $time);
      #30  cyc  <= 1; stb <= 1;
      data_tx <= {1'b0, 3'b000, 4'b0001};
      while (!ack) #10 ;
      
      #10 data_tx <= $random;
      while (!ack) #10 ;
      #10 data_tx <= $random;
      while (!ack) #10 ;
      #10 stb <= 0;
      while (!rdy) #10 ;
      #10 while (!rdy) #10 ;
      #10 while (!rdy) #10 ;
      cyc <= 0; #60 ;

      #20 ready <= 1;

      //----------------------------------------------------------------------
      //  Aquire some data, wait and then get some more.
      $display("\n%10t: Now get some data:", $time);
      #20 aq_count <= 6; aq_start <= 1;
      #10 while (!aq_done) #10 ;
      #200 ;

      //----------------------------------------------------------------------
      $display("\n%10t: Second data transfer:", $time);
      #20 aq_count <= 6; aq_start <= 1;
      #10 while (!aq_done) #10 ;
      */

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
   //  Make the returned data easier to see in GtkWave.
   wire [MSB:0] dat = |{rdys, rdy} ? drx : 'bz;
   reg [3:0]    rdys = 0;

   always @(posedge clk)
     rdys <= #DELAY {rdys[2:0], rdy};


   //-------------------------------------------------------------------------
   //  Generate fake DRAM contents.
   wire [23:0] data_w = data[data_index];
   integer     data_index = 0;
   reg         ready = 0;

   always @(posedge clk)
     if (rst)
       {aq_done, aq_start} <= #DELAY 2'b10;
     else if (!aq_start && aq_done && spi_start_aq)
       {aq_done, aq_start} <= #DELAY 2'b01;
     else if (!aq_done && aq_start && spi_start_aq)
       aq_start <= #DELAY 0;
     else
       aq_done  <= #DELAY aq_done ? aq_done : !spi_start_aq;

   always @(posedge clk)
     if (rst)
       ready <= #DELAY 0;
     else if (aq_start || request)
       ready <= #DELAY 1;
     else
       ready <= #DELAY 0;

   always @(posedge clk)
     if (rst)
       data_index <= #DELAY 0;
     else if (request)
       data_index <= #DELAY data_index + 1;
     else
       data_index <= #DELAY data_index;


   //-------------------------------------------------------------------------
   //  Devices Under Test (DUT's).
   //-------------------------------------------------------------------------
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
   
   tart_spi
     #( .ADDR_READ_DATA1  (4'h0),
        .ADDR_READ_DATA2  (4'h1),
        .ADDR_READ_DATA3  (4'h2),
        .ADDR_SAMPLE_DELAY(4'h5),
        .ADDR_DEBUG       (4'h6),
        .ADDR_STARTAQ     (4'h7),
        .ADDR_STATUS      (4'he),
        .ADDR_RESET       (4'hf)
        ) TART_SPI0
     ( .clk(clk),
       .rst(rst || spi_reset),
       
       .data_ready(ready),
       .data_request(request),
       .data_in(data_w),

       .debug_o(debug_o),
       
       .spi_reset(spi_reset),
       .spi_debug(spi_debug),
       .spi_status(status),
       .spi_start_aq(spi_start_aq),
       .data_sample_delay(sdelay),
       
       .SCK (SCK_pin),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );

   /*
   //-------------------------------------------------------------------------
   //     RESET HANDLER
   //-------------------------------------------------------------------------
   wb_reset #( .WIDTH(WIDTH) ) SPI_SLAVE0
     ( .clk_i(b_clk),
       .rst_i(b_rst),
       .cyc_o(b_cyc),
       .stb_o(r_stb),
       .we_o (b_we),
       .ack_i(r_ack),
       .dat_i(r_dtx),
       .dat_o(r_drx),

       .reset_ni(reset_n),
       .reset_o(reset)
       );
    */


endmodule // tart_spi_tb
