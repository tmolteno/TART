`timescale 1ns/100ps
/*
 * Module      : verilog/signal_capture.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : simulation file, and only tested with Icarus Verilog
 * 
 * Captures data by sampling at a higher (synchronous) clock-rate, and
 * presenting the captured data halfway through each period.
 * 
 * NOTE:
 *  + output is "sample-and-hold" and valid once `ready` asserts;
 *  + only tested for the configuration with the sampling-clock having 12x
 *    the frequency of the data-clock;
 * 
 * TODO:
 *  + parameterised window size, when deciding if the edges is allowable;
 *  + hardware testing;
 *  + clean up the combinational paths;
 * 
 */

module signal_capture
  #(//  Clock ratio settings:
    parameter RATIO = 12,       // clock (sampling:data) ratio
    parameter RBITS = 4,        // bit-width of down-sampling counter
    parameter MSB   = RBITS-1,  // MSB of counter
    parameter RMAX  = RATIO-1,  // max clock-counter value
    parameter HALF  = RMAX>>1,  // the floor of half the count
    parameter HSB   = RBITS-2,  // MSB of a half-cycle counter

    //  Additional features:
    parameter RESET = 0,        // fast-resets (0/1)?

    //  Simulation-only parameters:
    parameter DELAY = 3)        // simulated combinational delay (ns)
  (
   input      clk_i, // (sampling) clock input
   input      rst_i, // (sample domain) reset
   input      ce_i,  // (sample domain) core enable

   output reg rdy_o = 1'b0,
   output     locked_o,
   output     invalid_o,
   input      ack_i,            // acknowledge any invalid data

   input      dat_i,            // raw data
   output reg dat_o             // captured data
   );

   // Supersample at the given rate.
   reg [MSB:0] count = 0;
   reg         locked = 1'b0, invalid = 1'b0;

   assign locked_o  = locked;
   assign invalid_o = invalid;

   //-------------------------------------------------------------------------
   //  Capture within an IOB's register, and use two samples for edge
   //  detection.
   //-------------------------------------------------------------------------
   (* IOB = "TRUE" *)
   reg                  d_iob = 0;
   reg [HALF-1:0]       d_reg = 0;
   wire [HALF:0]        samples = {d_reg, d_iob};

   always @(posedge clk_i)
     if (ce_i) {d_reg, d_iob} <= {samples[HALF-1:0], d};

   //-------------------------------------------------------------------------
   //  Count the number of cycles between edges.
   //-------------------------------------------------------------------------
   wire [BITS_COUNT-1:0] count_next = edge_found || count_wrap ? count_init : count+1 ;
   wire                  count_wrap = count >= RATIO-1 && no_edges;
   wire [BITS_COUNT-1:0] count_init = count == RATIO+1 ? 1 : count == RATIO-3 ? -1 : 0 ;
   wire                  no_edges = &samples || ~|samples;
   wire                  edge_found = samples == {1'b1, {HALF{1'b0}}} || samples == {1'b0, {HALF{1'b1}}};

   always @(posedge clk_i)
     if (rst_i)     count <= 0;
     else if (ce_i) count <= count_next;
     else         count <= count;

   //-------------------------------------------------------------------------
   //  The signal is considered locked after four clean transitions.
   //-------------------------------------------------------------------------
   reg [1:0] locked_count = 0;
   reg       found = 0;
   wire      valid_count = count >= RATIO-3 && count < RATIO+2;

   always @(posedge clk_i)
     if (rst_i) begin
        locked       <= 0;
        locked_count <= 0;
     end
     else if (ce_i && edge_found && valid_count) begin
        locked       <= locked_count == 3;
        locked_count <= locked_count  < 3 ? locked_count + 1 : locked_count ;
     end
     else if (ce_i && edge_found) begin // Edge too far from acceptable.
        locked       <= 0;
        locked_count <= 0;
     end

   // If the signal is `locked`, and `edge_found`, but occuring to far to be
   // considered a `valid_count`, then assert `invalid`.
   // Clear `invalid` whenever any earlier `invalid` data is acknowledged.
   always @(posedge clk_i)
     if (rst_i)
       invalid <= 0;
     else if (ce_i)
       invalid <= !ack_i && invalid || locked && edge_found && !valid_count;

   //-------------------------------------------------------------------------
   //  Output the captured data-samples.
   //-------------------------------------------------------------------------
   wire ready_w = ce_i && locked && count == HALF;

   always @(posedge clk_i)
     if (rst_i && RESET)
       rdy_o <= #DELAY 1'b0;
     else
       begin
          rdy_o <= #DELAY ready_w;
          dat_o <= #DELAY ready_w ? samples[HALF] : q ;
       end


   //-------------------------------------------------------------------------
   //
   //  SIMULATION STUFF.
   //
   //-------------------------------------------------------------------------
   reg  [BITS_COUNT-1:0] predictor = 0;
   wire                  expected = predictor == RATIO-1;

   always @(posedge clk_i)
     if (rst_i)
       predictor <= 0;
     else if (ce_i) begin
        if (expected && count_next < HALF)
          predictor <= count_next;
        else if (edge_found && predictor < HALF)
          predictor <= count_next;
        else if (expected)
          predictor <= 0;
        else
          predictor <= predictor+1;
     end


endmodule // signal_capture
