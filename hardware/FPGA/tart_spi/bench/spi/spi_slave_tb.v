`timescale 1ns/100ps
module spi_slave_tb;

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

   wire [MSB:0] s_drx;
   wire [ASB:0] s_adr;
   reg [MSB:0]  s_dtx;
   wire         s_cyc, s_stb, s_bst, s_we;
   reg          s_ack = 0;

   // System signals:
   reg          clk = 1, rst = 0;
   reg          set = 0, get = 0, fin = 0;
   reg [MSB:0]  stat;
   wire         oflow, uflow;

   // SPI signals:
   reg          SCK = 1;
   wire         MOSI, MISO, SSEL;
   wire         SCK_pin = SCK_en ? SCK : 1'b0;


   //-------------------------------------------------------------------------
   //  Setup bus clock.
   always #5  clk <= ~clk;
   always #5  SCK <= ~SCK;


   //-------------------------------------------------------------------------
   //  Simulate SRAM accesses.
   integer      num = 0;
   reg [ASB:0]  ptr = 0;
   initial begin : SIM_BLOCK
      $dumpfile ("spi_tb.vcd");
      $dumpvars;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing reset:\n", $time);
      #33 rst <= 1; #40 rst <= 0;
      #10 stat <= $random;

      //----------------------------------------------------------------------
      $display("\n%8t: Single write:", $time);
      #40 set <= 1; num <= 2; ptr <= $random;
      while (!fin) #10;

      $display("\n%8t: Single read:", $time);
      #10 get <= 1; num <= 3;
      while (!fin) #10;

      //----------------------------------------------------------------------
      $display("\n%8t: Burst write:", $time);
      #40 set <= 1; num <= 16; ptr <= $random;
      while (!fin) #10;

      $display("\n%8t: Burst read:", $time);
      #40 get <= 1; num <= 8;
      while (!fin) #10;

      $display("\n%8t: Burst read:", $time);
      ptr <= ptr + 8;
      #10 get <= 1; num <= 8;
      while (!fin) #10;

      //----------------------------------------------------------------------
      #40 $display("\n%8t: Simulation finished:", $time);
      $finish;
   end

   initial begin : SIM_FAILED
      #12000 $display ("TIMEOUT!");
      $finish;
   end // SIM_FAILED


   //-------------------------------------------------------------------------
   //  Generate write data for both the master and slave SPI devices.
   always @(posedge clk) begin
     dtx <= #DELAY set || ack ? $random : dtx;
     s_dtx <= #DELAY s_cyc && s_stb && !s_we ? $random : s_dtx;
   end

   always @(negedge cyc)
     stat <= $random;


   //-------------------------------------------------------------------------
   //  Generate WB-like transactions.
   //-------------------------------------------------------------------------
   integer cnt;
   wire    cyc_n = cyc && cnt == 1 && rdy;

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
       .rdy_o(rdy),
       .wat_o(wat),
       .adr_i(adr),
       .dat_i(dtx),
       .dat_o(drx),
       
       .SCK(SCK),
       .SCK_enable(SCK_en),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );

   spi_slave #( .WIDTH(WIDTH) ) SPI_SLAVE0
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_o(s_cyc),
       .stb_o(s_stb),
       .bst_o(s_bst),
       .we_o (s_we),
       .ack_i(s_ack),
       .adr_o(s_adr),
       .dat_i(s_dtx),
       .dat_o(s_drx),

       .status_i(stat),
       .overflow_o(oflow),
       .underrun_o(uflow),
       
       .SCK_pin(SCK_pin),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );
   

endmodule // spi_slave_tb
