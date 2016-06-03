`timescale 1ns/1ps
/*
 * Captures data by sampling at a higher (synchronous) clock-rate, and
 * presenting the captured data halfway through each period.
 * 
 * NOTE:
 *  + output is "sample-and-hold" and valid once `ready` asserts;
 * 
 * TODO:
 *  + parameterised window size, when deciding if the edges is allowable;
 *  + hardware testing;
 *  + clean up the combinational paths;
 * 
 */

module signal_capture
  (
   input      clk,
   input      rst,
   input      ce,

   input      d,
   output reg q,
   output reg ready = 0,
   output reg locked = 0,
   output reg invalid = 0,
   input      ack               // acknowledge any invalid data
   );

   // Supersample at the given rate.
   parameter CLOCK_RATE = 12;
   parameter HALF = (CLOCK_RATE-1) >> 1;
   parameter BITS_COUNT = 4;

   reg [BITS_COUNT-1:0] count = 0;

   //-------------------------------------------------------------------------
   //  Capture within an IOB's register, and use two samples for edge
   //  detection.
   //-------------------------------------------------------------------------
   (* IOB = "TRUE" *)
   reg                  d_iob = 0;
   reg [HALF-1:0]       d_reg = 0;
   wire [HALF:0]        samples = {d_reg, d_iob};

   always @(posedge clk)
     if (ce) {d_reg, d_iob} <= {samples[HALF-1:0], d};

   //-------------------------------------------------------------------------
   //  Count the number of cycles between edges.
   //-------------------------------------------------------------------------
   wire [BITS_COUNT-1:0] count_next = edge_found || count_wrap ? count_init : count+1 ;
   wire                  count_wrap = count >= CLOCK_RATE-1 && no_edges;
   wire [BITS_COUNT-1:0] count_init = count == CLOCK_RATE+1 ? 1 : count == CLOCK_RATE-3 ? -1 : 0 ;
   wire                  no_edges = &samples || ~|samples;
   wire                  edge_found = samples == {1'b1, {HALF{1'b0}}} || samples == {1'b0, {HALF{1'b1}}};

   always @(posedge clk)
     if (rst)     count <= 0;
     else if (ce) count <= count_next;
     else         count <= count;

   //-------------------------------------------------------------------------
   //  The signal is considered locked after four clean transitions.
   //-------------------------------------------------------------------------
   reg [1:0] locked_count = 0;
   reg       found = 0;
   wire      valid_count = count >= CLOCK_RATE-3 && count < CLOCK_RATE+2;

   always @(posedge clk)
     if (rst) begin
        locked       <= 0;
        locked_count <= 0;
     end
     else if (ce && edge_found && valid_count) begin
        locked       <= locked_count == 3;
        locked_count <= locked_count  < 3 ? locked_count + 1 : locked_count ;
     end
     else if (ce && edge_found) begin // Edge too far from acceptable.
        locked       <= 0;
        locked_count <= 0;
     end

   // If the signal is `locked`, and `edge_found`, but occuring to far to be
   // considered a `valid_count`, then assert `invalid`.
   // Clear `invalid` whenever any earlier `invalid` data is acknowledged.
   always @(posedge clk)
     if (rst)
       invalid <= 0;
     else if (ce)
       invalid <= !ack && invalid || locked && edge_found && !valid_count;

   //-------------------------------------------------------------------------
   //  Output the captured data-samples.
   //-------------------------------------------------------------------------
   wire ready_w = ce && locked && count == HALF;

   always @(posedge clk) begin
      ready <= ready_w;
      q <= ready_w ? samples[HALF] : q ;
   end

   //-------------------------------------------------------------------------
   //  Simulation stuff.
   //-------------------------------------------------------------------------
   reg  [BITS_COUNT-1:0] predictor = 0;
   wire                  expected = predictor == CLOCK_RATE-1;

   always @(posedge clk)
     if (rst)
       predictor <= 0;
     else if (ce) begin
        if (expected && count_next < HALF)
          predictor <= count_next;
        else if (edge_found && predictor < HALF)
          predictor <= count_next;
        else if (expected)
          predictor <= 0;
        else
          predictor <= predictor+1;
     end


endmodule // sigcap
