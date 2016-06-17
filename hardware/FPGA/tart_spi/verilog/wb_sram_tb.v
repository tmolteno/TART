`timescale 1ns/100ps
module sram_tb;

   parameter WIDTH = 32;        // Number of bits of a block
   parameter MSB   = WIDTH-1;
   parameter SBITS = 10;
   parameter SIZE  = 1 << SBITS;
   parameter ASB   = SBITS-1;
   parameter DELAY = 3;

   wire [MSB:0] dat;
   reg [MSB:0]  val;
   reg [ASB:0]  adr;
   reg          clk = 1, rst = 0;
   reg          cyc = 0, stb = 0, we = 0, bst = 0;
   reg          set = 0, get = 0, fin = 0;
   wire         ack, bst_w;

   //-------------------------------------------------------------------------
   //  Setup bus clock.
   always #5  clk <= ~clk;


   //-------------------------------------------------------------------------
   //  Simulate SRAM accesses.
   integer      num = 0;
   integer      ptr = 0;
   initial begin : SIM_BLOCK
      $dumpfile ("sram_tb.vcd");
      $dumpvars;

      //----------------------------------------------------------------------
      $display("\n%8t: Issuing reset:\n", $time);
      #33 rst <= 1; #40 rst <= 0;

      //----------------------------------------------------------------------
      $display("\n%8t: Single write:", $time);
      #40 set <= 1; num <= 1; ptr <= $random;
      while (!fin) #10;

      $display("\n%8t: Single read:", $time);
      #10 get <= 1; num <= 1;
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
      #1200 $display ("TIMEOUT!");
      $finish;
   end // SIM_FAILED


   //-------------------------------------------------------------------------
   //  Generate write data.
   always @(posedge clk)
     if (set || bst && we)
       val <= #DELAY $random;


   //-------------------------------------------------------------------------
   //  Generate WB-like transactions.
   //-------------------------------------------------------------------------
   assign bst_w = num > 2 && cyc;

   always @(posedge clk)
     if (rst) bst <= #DELAY 0;
     else     bst <= #DELAY bst_w || (set || get) && num > 1;

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
        {fin, get, set} <= #DELAY {!stb && ack, get, set};
        {cyc, stb, we } <= #DELAY {stb || !ack, bst, we && (stb || !ack)};
     end
     else begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 0;
     end

   wire [ASB:0] next_adr = bst ? adr + 1 : adr;

   always @(posedge clk)
     if (rst)             adr <= #DELAY 0;
     else if (set || get) adr <= #DELAY ptr;
     else if (cyc)        adr <= #DELAY next_adr;

   always @(posedge clk)
     if (cyc && stb) num <= #DELAY num - 1;


   //-------------------------------------------------------------------------
   //  Device under test (DUT).
   //-------------------------------------------------------------------------
   wb_sram #( .WIDTH(WIDTH), .SBITS(SBITS) ) SRAM0
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_i(cyc),
       .stb_i(stb),
       .we_i (we),
       .bst_i(bst),
       .ack_o(ack),
       .adr_i(adr),
       .dat_i(val),
       .dat_o(dat)
       );


endmodule // sram_tb
