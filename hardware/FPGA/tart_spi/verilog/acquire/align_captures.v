`timescale 1ns/100ps
/*
 * Aligns a set of super-sampled signals, each with varying delay.
 * 
 * Data from multiple, independent sources can vary in phase, but if the data
 * is assumed to have total phase differences of less than half of a sample-
 * period, then it is possible to determine the common alignments of all the
 * signals.
 * 
 * Copyright (C) 2016 Tim Molteno
 * Copyright (C) 2016 Max Scheel
 * Copyright (C) 2016 Patrick Suggate
 * 
 * NOTE:
 *  + assumes that each `data_in[i]` remains constant until its next strobe;
 *  + the supersampling clock is assumed to be synchronous with the data
 *    clock;
 * 
 * TODO:
 *  + window sizes for invalid calculations;
 *  + clearing and counting assertions of `invalid`;
 * 
 */

module align_captures
  #( parameter NUM_SIGNALS = 24,
     parameter CLOCK_RATE  = 12,
     parameter CLEAR_GAP   = 7,
     parameter STROBE_GAP  = 5,
     parameter BITS_COUNT  = 4 )
   (
    input                        clk,
    input                        rst,
    input                        ce,

    input [NUM_SIGNALS-1:0]      data_in,
    input [NUM_SIGNALS-1:0]      strobes,
    input [NUM_SIGNALS-1:0]      lockeds,
    input [NUM_SIGNALS-1:0]      invalids,

    output reg [NUM_SIGNALS-1:0] data_out,
    output reg                   ready = 0,
    output reg                   locked = 0,
    output reg                   invalid = 0,
    input                        ack
    );

   reg [BITS_COUNT-1:0]          count = 0, clear = 0;
   reg                           strobe = 0, window = 0;
   wire                          locked_w = &lockeds;
   wire                          strobe_w = |strobes;

   //-------------------------------------------------------------------------
   //  Track the arrivals of all strobes.
   //-------------------------------------------------------------------------
   reg [NUM_SIGNALS-1:0]         strobes_reg = 0;
   reg                           strobes_all = 0;
   wire [NUM_SIGNALS-1:0]        strobes_upd = strobes_reg | strobes;
   wire                          strobes_end = &strobes_upd;

   // Pipeline the strobe 24-bit OR gate.
   always @(posedge clk)
     if (rst) strobe <= 0;
     else     strobe <= ce ? strobe_w : strobe ;

   // A window ends once all strobes have arrived.
   always @(posedge clk)
     if (rst) begin
        strobes_reg <= 0;
        strobes_all <= 0;
     end
     else if (ce) begin
        if (strobes_all && !strobe) begin
           strobes_reg <= 0;
           strobes_all <= 0;
        end
        else begin
           strobes_reg <= strobes_upd;
           strobes_all <= strobes_end;
        end
     end

   //-------------------------------------------------------------------------
   //  An acquisition window begins with any strobe, and ends once all have
   //  been received.
   //-------------------------------------------------------------------------
   always @(posedge clk)
     if (rst)
       window <= 0;
     else if (ce) begin
        if (strobe_w)
          window <= 1;
        else if (strobes_end)
          window <= 0;
        else
          window <= window;
     end

   //-------------------------------------------------------------------------
   //  Count the number of clock-ticks for a capture-period, and for the case
   //  without any data arriving.
   //-------------------------------------------------------------------------
   always @(posedge clk)
     if (rst)
       count <= 0;
     else if (ce) begin
//         if (clear >= CLEAR_GAP-1 && strobe)
        if (!window && strobe)
          count <= 0;
        else
          count <= count + 1;
     end

   always @(posedge clk)
     if (rst)     clear <= 0;
     else if (ce) clear <= strobe || window ? 0 : clear + 1 ;
     else         clear <= clear;

   //-------------------------------------------------------------------------
   //  Acquisition is locked if aligned.
   //-------------------------------------------------------------------------
   wire clear_valid = strobe && clear >= CLEAR_GAP;

   always @(posedge clk)
     if (rst)     locked <= 0;
     else if (ce) locked <= clear_valid ? locked_w : locked ;
     else         locked <= locked;

   //-------------------------------------------------------------------------
   //  Present captured & aligned data.
   //-------------------------------------------------------------------------
   always @(posedge clk)
     if (rst)
       ready <= 0;
     else if (ce && strobes_all && locked && !ready) begin
        ready    <= 1;
        data_out <= data_in;
     end
     else
       ready <= 0;

   //-------------------------------------------------------------------------
   //  Assert `invalid` whenever behaviour falls outside of:
   //   1) all 24 strobes arrive within 5 cycles;
   //   2) this is followed by at least 7 cycles of no strobes; and
   //   3) all signals remain locked.
   //-------------------------------------------------------------------------
   wire has_overlaps    = |(strobes_reg & strobes); // strobed twice?
   wire invalid_input   = |invalids;
   wire too_much_spread = locked && locked_w && strobe && clear > 0 && clear < CLEAR_GAP;
   wire capture_error   = has_overlaps || invalid_input || too_much_spread;

   // TODO:
   always @(posedge clk)
     if (rst)
       invalid <= 0;
     else if (ce)
       invalid <= !ack && invalid || locked && capture_error;
     else
       invalid <= invalid;


endmodule // align_captures
