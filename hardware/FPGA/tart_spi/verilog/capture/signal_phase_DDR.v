`timescale 1ns/100ps
/*
 * Module      : verilog/capture/signal_phase_DDR.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan 6)
 * 
 * Performs clock-recovery to determine the phase of the incoming signal (as
 * the frequency is assumed to known and fixed).
 * 
 * NOTE:
 *  + only tested for the configuration with the sampling-clock having 6x the
 *    frequency of the data-clock (and due to the DDR sampling, this has the
 *    same resolution as using a 12x clock with only SDR);
 *  + DDR sampling is used so that the input signals can be captured, and then
 *    distributed, with clocks slow enough for XST to easily meet timing;
 *  + by oversampling the external (XTAL) clock, then edge (positive,
 *    or negative) transitions can be used as a reference, as to which
 *    phase shifts from the source signals will be measured.
 *  + assumes that the input signals are fairly well-behaved in that they are
 *    largely free from transients, have low jitter, and that the maximum
 *    phase-difference between any pair of antennas is less than quarter of a
 *    wavelength;
 * 
 * TODO:
 *  + parameterised window size, when deciding if the edges is allowable;
 *  + hardware testing;
 *  + handle signed comparisons;
 * 
 */

module signal_phase_DDR
  #(//  Clock ratio settings:
    parameter RATIO = 6,        // (DDR) sample-clock to external-clock ratio
    parameter RBITS = 3,        // bit-width of down-sampling counter
    parameter RSB   = RBITS-1,  // MSB of counter
    parameter RMAX  = RATIO-1,  // max clock-counter value
    parameter RZERO = 3'h0,

    //  Additional options for the reference signal:
    parameter TICKS = 2,        // clock-cycle delay of the source?
    parameter TSB   = TICKS-1,  // MSB of the delay shift-register
    parameter TSC   = TSB-1,    // MSB of the retained shift-register
    parameter POLAR = 0,        // use inverted polarity for reference?

    //  Signal-locking settings:
    parameter LIMIT = 2,        // signal valid if delta doesn't exceed this
    parameter COUNT = 3,        // number of samples for a lock
    parameter CBITS = 2,        // counter bit-width
    parameter CSB   = CBITS-1,  // MSB of counter
    parameter CZERO = 0,

    //  Additional features:
    parameter RESET = 0,        // TODO: fast-resets (0/1)?

    //  Simulation-only parameters:
    parameter NOISY = 0,        // display extra debug info (0/1)?
    parameter DELAY = 3)        // simulated combinational delay (ns)
   (
    input            clk_e_i, // external (and reference) clock
    input            clk_s_i, // half-rate (sampling) clock input
    input            clk_n_i, // inverted, half-rate (sampling) clock input
    input            reset_i, // (sample domain) reset

    input            enable_i, // (sample domain) core enable
    input            invert_i, // use negative edges for reference?
    output           strobe_o, // strobes for each new sample
    output           middle_o, // strobes mid-sample
    input            sig_p_i, // DDR signal requiring clock-recovery
    input            sig_n_i, // DDR signal requiring clock-recovery

    //  Recovered-signal info:
    output [RBITS:0] phase_o, // recovered phase
    output [RBITS:0] delta_o, // the computed error
    output           locked_o, // signal locked
    output           error_o, // lock lost
    input            retry_i, // acknowledge any invalid data

    //  Debug/info outputs:
    output           ref_p_o, // output the reference signals
    output           ref_n_o
    );


   //-------------------------------------------------------------------------
   //  Reference signals, synchronisers, and delays.
   reg               pos_e = 1'b0, neg_e = 1'b0;
   reg [TSB:0]       pos, neg;
   reg               stb_p, stb_n;
   wire              pos_w, neg_w, cycled;

   //-------------------------------------------------------------------------
   //  Input signals, synchronisers, and edge-detectors.
   (* NOMERGE = "TRUE" *)
   reg               sig_n, sig_p;
   reg [1:0]         xedge = 2'b00;
   wire              edg_0, edg_1, edg_w;

   //-------------------------------------------------------------------------
   //  Cycle-count and phase signals and registers.
   wire [RBITS:0]    cnext = cycle + 1;
   wire              cwrap = cycle == RMAX;
   reg [RSB:0]       cycle = RZERO;
   reg [RBITS:0]     phase = RZERO;
   reg signed [RBITS:0] delta;
   wire [RBITS:0]    phase_w = {cycle, xedge[0]};
   wire [RBITS:0]    delta_w = phase_w - phase;

   // TODO:
   reg               valid = 1'b0, error = 1'b0;



   //-------------------------------------------------------------------------
   //  Assignments to outputs.
   //-------------------------------------------------------------------------
   assign strobe_o = stb_p;
   assign middle_o = stb_n;
   assign phase_o  = phase;
   assign delta_o  = delta;

   assign locked_o = valid;     // TODO:
   assign error_o  = error;

   assign ref_p_o  = pos[TSB];
   assign ref_n_o  = neg[TSB];


   //-------------------------------------------------------------------------
   //  Detect the positive and negative edges of the reference signal.
   //-------------------------------------------------------------------------
   assign pos_w  = pos[TSB] ^ pos[TSC];
   assign neg_w  = neg[TSB] ^ neg[TSC];

   assign cycled = POLAR ? (pos_w && !invert_i || neg_w && invert_i) : pos_w;


   //-------------------------------------------------------------------------
   //  Source-signal edge detection.
   //-------------------------------------------------------------------------
   //  There are two (unique) possible locations for transitions, at each
   //  positive-edge of the (half-rate) sampling-clock.
   assign edg_0   = sig_p ^ sig_p_i && sig_n ^ sig_n_i;
   assign edg_1   = sig_p ^ sig_n;
//    assign edg_1   = sig_p_i ^ sig_n_i;

   //-------------------------------------------------------------------------
   //  An edge has been found whenever any of the previous values differ from
   //  the current values.
//    assign edg_w   = sig_p ^ sig_p_i;
   assign edg_w   = sig_n ^ sig_n_i;
//    assign edg_w   = sig_p ^ sig_p_i || sig_n ^ sig_n_i;



   //-------------------------------------------------------------------------
   //
   //  ESTABLISH A REFERENCE SIGNAL.
   //
   //-------------------------------------------------------------------------
   //  Toggle a register at the external-clock rate, to simulate a signal to
   //  use as a reference for clock-recovery.
   //-------------------------------------------------------------------------
   always @(posedge clk_e_i)
     pos_e <= #DELAY ~pos_e;

   always @(negedge clk_e_i)
     neg_e <= #DELAY ~neg_e;

   //-------------------------------------------------------------------------
   //  And synchronise (plus delay) the reference signal.
   always @(posedge clk_s_i)
     if (reset_i && RESET) begin
        pos <= #DELAY {TICKS{1'b0}};
        neg <= #DELAY {TICKS{1'b0}};
     end
     else begin
        pos <= #DELAY {pos[TSC:0], pos_e};
        neg <= #DELAY {neg[TSC:0], neg_e};
     end

   //-------------------------------------------------------------------------
   //  Strobe at each new edge.
   always @(posedge clk_s_i)
     if (reset_i && RESET)
       {xedge, stb_n, stb_p} <= #DELAY 4'h0;
     else begin
        stb_p <= #DELAY pos_w;
        stb_n <= #DELAY neg_w;
        xedge <= #DELAY {edg_1, edg_0};
     end



   //-------------------------------------------------------------------------
   //
   //  COMPUTE THE RELATIVE PHASES.
   //
   //-------------------------------------------------------------------------
   //  Register the current inputs, for future edge comparisons.
   //-------------------------------------------------------------------------
   always @(posedge clk_s_i)
     if (reset_i && RESET) begin
        sig_p <= #DELAY 1'b0;
        sig_n <= #DELAY 1'b0;
     end
     else begin
        sig_p <= #DELAY sig_p_i;
        sig_n <= #DELAY sig_n_i;
     end


   //-------------------------------------------------------------------------
   //  Count the cycles/periods since the signal-clock positive-edge.
   //-------------------------------------------------------------------------
   //  NOTE: This is used to compute the relative phases, of the signal-clock
   //    vs the clock of the input-signal.
   always @(posedge clk_s_i)
     if (reset_i && RESET || cycled)
       cycle <= #DELAY RZERO;
     else
       cycle <= #DELAY cnext[RSB:0];

   //-------------------------------------------------------------------------
   //  After every positive (negative) edge of the external (XTAL) clock,
   //  count the number of fast-clock transitions until an edge is found.
   //  Once found, compute the new relative phase, and the difference vs. the
   //  previous phase value.
   always @(posedge clk_s_i)
     if (reset_i && RESET) begin
        phase <= #DELAY RZERO;
        delta <= #DELAY RZERO;
     end
     else if (xedge[0] || xedge[1]) begin
        phase <= #DELAY phase_w;
        delta <= #DELAY delta_w;
     end


`ifdef __USE_UNFINISHED_SIGNAL_LOCKING_EVEN_THOUGH_IT_DOES_NOT_WORK
   //-------------------------------------------------------------------------
   //
   //  SIGNAL LOCKING.
   //
   //-------------------------------------------------------------------------
   wire [CBITS:0] lnext = count + 1;
   wire           lwrap = lnext == COUNT[CBITS:0];
   wire           llost = delta < -LIMIT || delta > LIMIT;
   reg [CSB:0]    count = CZERO;

   always @(posedge clk_s_i)
     if (reset_i || llost)
       count <= #DELAY CZERO;
     else if (enable_i && stb_p && !lwrap)
       count <= #DELAY lnext;
   
   always @(posedge clk_s_i)
     if (reset_i || !enable_i || llost)
       valid <= #DELAY 1'b0;
     else if (enable_i && lwrap)
       valid <= #DELAY 1'b1;
`else // !`ifdef __USE_UNFINISHED_SIGNAL_LOCKING_EVEN_THOUGH_IT_DOES_NOT_WORK
   
`endif // !`ifdef __USE_UNFINISHED_SIGNAL_LOCKING_EVEN_THOUGH_IT_DOES_NOT_WORK



endmodule // signal_phase_DDR
