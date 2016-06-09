`timescale 1ns/1ps
/*
 *
 * Time-multiplexed block of correlator-blocks.
 * 
 * NOTE:
 *  + typically several of these would be attached to a common set of antenna
 *    and a system bus;
 *  + a bank-switch command causes accumulator values to be cleared upon first
 *    access after a switch, by giving the accumulator a zero input;
 *  + the bus clock can be much slower than the correlation clock, as multi-
 *    port SRAM's are used;
 *  + bus transactions read from the currently-innactive bank, to prevent
 *    possible metastability/corruption;
 *  + potentially uses quite a lot of the FPGA's distributed-RAM resources;
 * 
 */

module correlator_block
  #( parameter ACCUM = 32,
     // Pairs of antennas to correlate, for each block.
     parameter PAIRS0 = 96'hb1a191817161b0a090807060,
     parameter PAIRS1 = 96'hb3a393837363b2a292827262,
     parameter PAIRS2 = 96'hb5a595857565b4a494847464,
     parameter PAIRS3 = 96'hb1a191817161b0a090807060, // TODO:
     parameter MSB   = ACCUM - 1,
     parameter DELAY = 3)
   (
    input          clk_x,       // correlator clock
    input          rst,

    // Wishbone-like bus interface for reading visibilities.
    input          clk_i,       // bus clock
    input          cyc_i,
    input          stb_i,
    input          we_i, // writes are ignored
    output         ack_o,
    input [4:0]    adr_i,
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,

    // Real and imaginary components from the antennas.
    input          sw,          // switch banks
    input          en,          // data is valid
    input [11:0]   re,
    input [11:0]   im,

    output reg     overflow_cos = 0,
    output reg     overflow_sin = 0,
    );
  
endmodule // correlator_block
