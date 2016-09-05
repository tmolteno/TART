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
 * 
 * TODO:
 * 
 */

//----------------------------------------------------------------------------
//  Visibilities and correlator settings.
`define NUM_ANTENNA 24
`define ACCUM_BITS  24          // Bit-width of the accumulators
`define TMUX_RATE   12          // Time-multiplexing rate
`define VISB_LOG2   20          // = 2^20 samples/block;
`define READ_COUNT 576          // Number of visibilities to read back
`define READ_BITS   10          // = ceiling{log2(`READ_COUNT)};
// `define RANDOM_DATA  1          // Use a RNG for fake data?
`define RANDOM_DATA  0          // Use a RNG for fake data?


//
//  OPTIONAL FEATURES
//
`define __NO_DSP_DUPS
// `define __NO_SDP_DUPS


//----------------------------------------------------------------------------
//
//  Data acquisition is optional.
//
//----------------------------------------------------------------------------
`define __USE_ACQUISITION

//----------------------------------------------------------------------------
//
//  As is hardware calculation of the visibilities.
//
//----------------------------------------------------------------------------
`define __USE_CORRELATORS


//----------------------------------------------------------------------------
//
//  There are two versions of the Papilio Pro LX9, V1.3 -- one has 8 MB of
//  SDRAM, and the other has 64 MB (512 Mb).
//
//----------------------------------------------------------------------------
`define __512Mb_SDRAM


//----------------------------------------------------------------------------
//
//  The new clocks are unfinished.
//
//----------------------------------------------------------------------------
// `define __USE_OLD_CLOCKS
`define __USE_DCM_CLKGEN


//----------------------------------------------------------------------------
//
//  The version using Wishbone cores is the more current version.
//
//----------------------------------------------------------------------------
//  Choose whether to use classic Wishbone bus cycles, or the newer, faster
//  pipelined transfers.
`define __WB_CLASSIC

`ifdef  __WB_CLASSIC
 `undef  __WB_PIPELINED
`else
 `undef  __WB_CLASSIC
 `define __WB_PIPELINED
`endif // __WB_CLASSIC

//----------------------------------------------------------------------------
//  Wishbone bus settings for reading back the visibilities.
`define WBADR_BITS 12           // Address bit-width
`define WBBUS_BITS  8           // Bit-width of the SoC data bus
`define BLOCK_BITS  4           // Bit-width of the block-counter


//----------------------------------------------------------------------------
//  Simulation settings
//----------------------------------------------------------------------------
`define DELAY 3


//
//  LEGACY
//
//----------------------------------------------------------------------------
//  The correlators can be set to use only distributed SRAM's, or a mix of
//  distributed SRAM's and block SRAM's. The latter uses few resources (if
//  there are enough block SRAM's), but imposes additional placement
//  constraints, potentially leading to lower circuit performance.
`define __USE_SDP_DSRAM
