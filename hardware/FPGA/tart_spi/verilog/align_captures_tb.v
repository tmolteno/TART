`timescale 1ns/1ps
module align_captures_tb;

   parameter RATE = 12;
   parameter HALF = RATE >> 1;
   parameter BITS = 4;
   parameter SIGS = 8;
   parameter MSB  = SIGS-1;

   wire [MSB:0] daq, sig, cap, strobes, lockeds, invalids;
   wire         ready, locked, invalid;
   reg          clk12x = 1, clk = 1, rst = 0, ce = 0;
   reg [MSB:0]  raw;

   always #2.50 clk12x <= ~clk12x;
   always #30.0 clk <= ~clk;


   initial begin : SIGNAL_TB
      $dumpfile ("align_tb.vcd");
      $dumpvars;
      
      #45 rst = 1; #90 rst = 0;

      #720; #720; #720;
      #720 $finish;
   end


   always @(posedge clk)
     raw <= $random;

   always @(posedge clk12x)
     if (ready) $display("%8t: DATA = %08b (%02x)", $time, daq, daq);

   // Start aquisition after a reset.
   always @(posedge clk)
     ce <= !rst ? 1 : 0 ;


   //-------------------------------------------------------------------------
   //  Align the staggered data.
   align_captures
     #( .NUM_SIGNALS(SIGS),
        .CLOCK_RATE (RATE),
        .CLEAR_GAP  (HALF + 1),
        .STROBE_GAP (HALF - 1),
        .BITS_COUNT (BITS)
        ) ALIGNS0
       (
        .clk(clk12x),
        .rst(rst),
        .ce(ce),
        
        .data_in(cap),
        .strobes(strobes),
        .lockeds(lockeds),
        .invalids(invalids),

        .data_out(daq),
        .ready(ready),
        .locked(locked),
        .invalid(invalid)
        );

   signal_capture SIGCAP0 [MSB:0]
     ( .clk(clk12x),
       .rst(rst),
       .ce(ce),
       .d(sig),
       .q(cap),
       .ready(strobes),
       .locked(lockeds),
       .invalid(invalids)
       );

   // Offset and jitter the given signals.
   signal_stagger
     #( .PHASE_JITTER(1),
        .PHASE_OFFSET(2),
        .CYCLE_JITTER(0)
        ) STAGGER0 [MSB:0]
     (  .clk(clk12x),
        .rst(rst),
        .ce (ce),
        .d  (raw),
        .q  (sig)
        );


endmodule // align_captures_tb
