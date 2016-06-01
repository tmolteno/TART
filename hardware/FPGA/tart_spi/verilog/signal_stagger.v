`timescale 1ns/1ps
/*
 * Testing module, for adding jitter and offsets to the input signal.
 * 
 */

module signal_stagger
  #(
    parameter WIDTH_SIGNAL = 1,
    parameter MSB = WIDTH_SIGNAL-1,
    parameter PHASE_JITTER = 1,  // #bits of edge jitter
    parameter PHASE_OFFSET = 1,  // max amount of consistent offset from edge
    parameter CYCLE_JITTER = 1)  // #bits of cycle-length jitter
   (
    input              clk,
    input              rst,
    input              ce,
    input [MSB:0]      d,
    output reg [MSB:0] q = 0
    );

   reg signed [3:0]    phase_jitter; // TODO
   reg signed [3:0]    phase_offset;
   reg signed [3:0]    cycle_jitter; // TODO

   reg signed [4:0]    count = 0;

   //-------------------------------------------------------------------------
   //  Compute the jitters and offsets.
   //-------------------------------------------------------------------------
   // Choose a phase-offset on start-up.
   initial begin : INITIAL_VALUES
      phase_offset = $random;
      phase_offset = phase_offset % (PHASE_OFFSET + 1);
      $display("Phase offset:\t%d", phase_offset);
   end

   always @(posedge clk)
     if (rst || ce && count_wrap) begin
        phase_jitter = $random;
        phase_jitter = phase_jitter % (PHASE_JITTER + 1);
     end

   //-------------------------------------------------------------------------
   //  Change the phase of the incoming data.
   //-------------------------------------------------------------------------
   wire count_wrap = count == 11;
   wire [4:0] phase = count + phase_offset + phase_jitter;

   always @(posedge clk)
     if (rst)     count <= 0;
     else if (ce) count <= count_wrap ? 0 : count+1 ;
     else         count <= count;

   always @(posedge clk)
     if (ce) q <= phase == 6 ? d : q ;

endmodule // signal_stagger
