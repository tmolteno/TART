`timescale 1ns/100ps
/*
 * Module      : verilog/acquire/signal_capture.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
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
    parameter HSB   = HALF-1,   // MSB of a half-cycle counter

    //  Additional features:
    parameter RESET = 1,        // fast-resets (0/1)?

    //  Simulation-only parameters:
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input      clk_i, // (sampling) clock input
    input      rst_i, // (sample domain) reset
    input      ce_i,  // (sample domain) core enable

    output reg rdy_o = 1'b0,
    input      ack_i, // acknowledge any invalid data
    output     locked_o,
    output     invalid_o,

    input      dat_i, // raw data
    output reg dat_o  // captured data
    );


   //  Supersample at the given rate.
   wire [HALF:0] samples;
   reg [MSB:0]   count = {RBITS{1'b0}};
   reg           locked = 1'b0, invalid = 1'b0;
   reg [HSB:0]   d_reg = {HALF{1'b0}};

   wire [MSB:0]  count_init, count_next;
   wire          count_wrap;

   reg [1:0]     locked_count = 2'h0;
   reg           found = 1'b0;
   wire          vld_w, rdy_w, bad_w;
   wire          no_edges, edge_pos, edge_neg, edge_found;

   //  Place the first capture-register
   (* IOB = "TRUE" *)
   reg           d_iob = 1'b0;


   //-------------------------------------------------------------------------
   //  Assignments to outputs.
   assign locked_o  = locked;
   assign invalid_o = invalid;

   //  Concatenate all registered signals values, to be indexed.
   assign samples = {d_reg, d_iob};

   //  Internal, combinational conditionals.
   assign rdy_w = ce_i && locked && count == HALF;
   assign vld_w = count >= RATIO-3 && count < RATIO+2;
   assign bad_w = locked && edge_found && !vld_w;

   assign no_edges = &samples || ~|samples;
   assign edge_pos = samples == {1'b0, {HALF{1'b1}}};
   assign edge_neg = samples == {1'b1, {HALF{1'b0}}};
   assign edge_found = edge_neg || edge_pos;

   //  Count the spacing between edges.
   assign count_next = edge_found || count_wrap ? count_init : count+1 ;
   assign count_wrap = count >= RATIO-1 && no_edges;
   assign count_init = count == RATIO+1 ? 1 : count == RATIO-3 ? -1 : 0 ;


   //-------------------------------------------------------------------------
   //  Capture within an IOB's register, and use two samples for edge
   //  detection.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (ce_i)
       {d_reg, d_iob} <= #DELAY {samples[HSB:0], dat_i};

   //-------------------------------------------------------------------------
   //  Count the number of cycles between edges.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i)
       count <= #DELAY {RBITS{1'b0}};
     else if (ce_i)
       count <= #DELAY count_next;
     else
       count <= #DELAY count;

   //-------------------------------------------------------------------------
   //  The signal is considered locked after four clean transitions.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i) begin
        locked       <= #DELAY 1'b0;
        locked_count <= #DELAY 2'h0;
     end
     else if (ce_i && edge_found && vld_w) begin
        locked       <= #DELAY locked_count == 3;
        locked_count <= #DELAY locked_count  < 3 ? locked_count + 1 : locked_count ;
     end
     else if (ce_i && edge_found) begin // Edge too far from acceptable.
        locked       <= #DELAY 1'b0;
        locked_count <= #DELAY 2'h0;
     end

   // If the signal is `locked`, and `edge_found`, but occuring to far to be
   // considered a valid count, then assert `invalid`.
   // Clear `invalid` whenever any earlier `invalid` data is acknowledged.
   always @(posedge clk_i)
     if (rst_i)
       invalid <= #DELAY 1'b0;
     else if (ce_i && bad_w)
       invalid <= #DELAY 1'b1;
     else if (ack_i)
       invalid <= #DELAY 1'b0;
     else
       invalid <= #DELAY invalid;

   //-------------------------------------------------------------------------
   //  Output the captured data-samples.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i && RESET)
       rdy_o <= #DELAY 1'b0;
     else
       begin
          rdy_o <= #DELAY rdy_w;
          dat_o <= #DELAY rdy_w ? samples[HALF] : dat_o ;
       end


   //-------------------------------------------------------------------------
   //
   //  SIMULATION STUFF.
   //
   //-------------------------------------------------------------------------
   reg  [MSB:0] predictor = {RBITS{1'b0}};
   wire         expected  = predictor == RATIO-1;

   always @(posedge clk_i)
     if (rst_i)
       predictor <= #DELAY {RBITS{1'b0}};
     else if (ce_i) begin
        if (expected && count_next < HALF)
          predictor <= #DELAY count_next;
        else if (edge_found && predictor < HALF)
          predictor <= #DELAY count_next;
        else if (expected)
          predictor <= #DELAY {RBITS{1'b0}};
        else
          predictor <= #DELAY predictor + 1;
     end


endmodule // signal_capture
