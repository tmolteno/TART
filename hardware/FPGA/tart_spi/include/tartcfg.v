/*
 * Module      : include/tartcfg.v
 * Copyright   : (C) Tim Molteno     2016
 *             : (C) Max Scheel      2016
 *             : (C) Patrick Suggate 2016
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 * 
 * TART setttings.
 * 
 * NOTE:
 * 
 * Changelog:
 *  + 28/06/2016  --  initial file;
 *  + 19/10/2016  --  cleaned up, for release;
 * 
 * TODO:
 * 
 */


//==========================================================================//
//                                                                          //
//  GLOBAL SETTTINGS                                                        //
//                                                                          //
//==========================================================================//


//----------------------------------------------------------------------------
//
//    SELECT FEWER DEBUG FEATURES?
//
//----------------------------------------------------------------------------
// `define __RELEASE_BUILD
`undef  __RELEASE_BUILD

`ifndef __RELEASE_BUILD
 `define USE_DEBUG 1
`else
 `define USE_DEBUG 0
`endif



//----------------------------------------------------------------------------
//
//    SELECT THE CLOCK GENERATOR TO USE
//
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
//  Required to use the correlators.
//  NOTE: This clock generator uses two DCM's, to:
//   a) condition the external, data-sampling clock;
//   b) generate the 6x system bus clock; and,
//   c) synthesise the 12x correlator clock.
`define __USE_DCM_CLKGEN

//----------------------------------------------------------------------------
//  Sufficient for acquisition only.
// `define __USE_OLD_CLOCKS


//----------------------------------------------------------------------------
//  
//  VISIBILITIES AND CORRELATOR COUNTER & BIT-WIDTH SETTINGS
//  
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
//  Antenna count, and visibilities accumulator settings.
`define NUM_ANTENNA 24
`define ACCUM_BITS  24          // Bit-width of the accumulators
`define VISB_LOG2   20          // = 2^20 samples/block;
`define USE_ALIGN    0          // extra signal-alignment needed?

//----------------------------------------------------------------------------
//  Correlators use time-multiplexing, and this results in each correlator
//  performing `TMUX_RATE` calculations per input sample-clock period.
`define TMUX_RATE   12          // Time-multiplexing rate
`define TMUX_BITS    4          // Number of bits for TMUX counters

//----------------------------------------------------------------------------
//  Number of words, for the visibilities data.
`define READ_COUNT 576          // Number of visibilities to read back
`define READ_BITS   10          // = ceiling{log2(`READ_COUNT)};

//----------------------------------------------------------------------------
//  Settings for the banks of visibilities data.
`define READ_BANKS  16          // Number of banks of visibilities
`define BANK_BITS    4          // Bit-width of bank-counter


//----------------------------------------------------------------------------
//  Allow multiple sources of fake (antenna) data?
`define MULTI_SOURCE 1
// `define MULTI_SOURCE 0

//----------------------------------------------------------------------------
//  Random data can be (internally) generated, for debugging purposes, and
//  using a MFSR (Multiple-Feedback Shift Register, which is similar to a
//  Linear-Feedback Shift Register).
// `define RANDOM_DATA  1          // Use a RNG for fake data?
`define RANDOM_DATA  0          // Use a RNG for fake data?

//----------------------------------------------------------------------------
//  Constant data can be used to test correlators.
// `define CONST_DATA 0
`define CONST_DATA 1
`define CONST_WORD 24'h000000



//==========================================================================//
//                                                                          //
//  OPTIONAL TART MODULES & SETTINGS                                        //
//                                                                          //
//==========================================================================//


//----------------------------------------------------------------------------
//
//    OPTIONAL ACQUISITION OF RAW ANTENNA DATA?
//
//----------------------------------------------------------------------------
`define __USE_ACQUISITION

//----------------------------------------------------------------------------
//  There are two versions of the Papilio Pro LX9, V1.3 -- one has 8 MB of
//  SDRAM, and the other has 64 MB (512 Mb).
`define __512Mb_SDRAM
// `undef  __512Mb_SDRAM



//----------------------------------------------------------------------------
//
//    ENABLE HARDWARE CALCULATION OF THE VISIBILITIES?
//
//----------------------------------------------------------------------------
`define __USE_CORRELATORS

//----------------------------------------------------------------------------
//  If banks of visibilities take to long to be read, then they can be over-
//  written, and this would typically be something worth knowing.
`define __USE_OVERFLOW_DETECTION

//----------------------------------------------------------------------------
//  Generic SRAM's are more difficult to floorplan, than Xilinx-specific
//  SRAM primitives.
// `define __USE_GENERIC_SRAM
`undef __USE_GENERIC_SRAM

`define __USE_EXPLICT_BRAM


//----------------------------------------------------------------------------
//  Enable a fake DSP unit, for testing & development of new correlators?
// `define __USE_FAKE_DSP
`undef __USE_FAKE_DSP

//----------------------------------------------------------------------------
//  Memory-map the block counter, to allow the SPI interface to set the block-
//  address/counter?
`define __USE_SETTABLE_BLOCK_COUNTER

//----------------------------------------------------------------------------
//  Use a lookup-table for computing the blocksize?
`define __LOOKUP_BLOCKSIZE



//----------------------------------------------------------------------------
//
//    WISHBONE INTERCONNECT SETTINGS
//
//----------------------------------------------------------------------------
//  Choose whether to use classic Wishbone bus cycles, or faster, pipelined,
//  burst-mode transfers.
// `define __WB_CLASSIC
`undef  __WB_CLASSIC
// `define __WB_BURSTMODE

`ifdef  __WB_CLASSIC
 `undef  __WB_BURSTMODE
 `undef  __WB_PIPELINED
`else
 `undef  __WB_CLASSIC
 `define __WB_PIPELINED
`endif // !__WB_CLASSIC

`define __USE_ASYNC_FETCH
`define __USE_FANCY_PREFETCH

//----------------------------------------------------------------------------
//  Enable Wishbone SPEC B4 pipelined, burst-mode transfers for devices that
//  support them.
`define __WB_SPEC_B4

//----------------------------------------------------------------------------
//  Choose whether to use classic, or burst-mode, transfers when prefetching
//  visibilities.
// `define __WB_PREFETCH_CLASSIC

//----------------------------------------------------------------------------
//  Choose whether to use classic, or burst-mode, transfers when accessing
//  SRAM's.
// `define __WB_SRAM_CLASSIC

//----------------------------------------------------------------------------
//  Choose whether to use classic, or burst-mode, transfers when accessing
//  SRAM's.
// `define __WB_CORRELATOR_CLASSIC

//----------------------------------------------------------------------------
//  Wishbone bus bit-width settings for the visibilities, read-back bus.
`define WBADR_BITS 12           // Address bit-width
`define WBBUS_BITS  8           // Bit-width of the SoC data bus



//----------------------------------------------------------------------------
//  
//    OPTIMISATION SETTINGS
//  
//----------------------------------------------------------------------------
//  Duplicate any of the address registers?
`define __NO_DSP_DUPS
// `define __NO_SDP_DUPS

//  Reduce latency, but increase combinational delays?
// `define __USE_DSP_SLOW
// `define __USE_SDP_SLOW



//----------------------------------------------------------------------------
//    
//    SIMULATION SETTINGS
//    
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
//  Simulated combinational delay, in `ns`.
`define DELAY 3

//----------------------------------------------------------------------------
//  Simulation half-period delays, in `ns`.
`define CLK_X  2.5
`define CLK_B  5.0
`define CLK_E 30.0
`define CLK_S  8.0

//----------------------------------------------------------------------------
//  Output format of the read-back visibilities.
// `define __USE_COLUMN_DISPLAY
`undef  __USE_COLUMN_DISPLAY

//----------------------------------------------------------------------------
//  Selects whether to simulate Xilinx primitives, or to use (hopefully)
//  equivalent code instead.
// `define __SIMULATE_XILINX_PRIMITIVES
`undef  __SIMULATE_XILINX_PRIMITIVES



//----------------------------------------------------------------------------
//
//    LEGACY SETTINGS
//
//----------------------------------------------------------------------------
//  The correlators can be set to use only distributed SRAM's, or a mix of
//  distributed SRAM's and block SRAM's. The latter uses few resources (if
//  there are enough block SRAM's), but imposes additional placement
//  constraints, potentially leading to lower circuit performance.
// `define __USE_SDP_DSRAM
