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
`define ACCUM_BITS  32          // Bit-width of the accumulators
`define TMUX_RATE   12          // Time-multiplexing rate
`define VISB_LOG2   20          // = 2^20 samples/block;
`define READ_COUNT 576          // Number of visibilities to read back
`define READ_BITS   10          // = ceiling{log2(`READ_COUNT)};

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


//----------------------------------------------------------------------------
//
//  The version using Wishbone cores is the more current version.
//
//----------------------------------------------------------------------------
`define __USE_WISHBONE_CORES

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
//  The Wishbone bus also has some correlators.
`define __USE_CORRELATORS

//----------------------------------------------------------------------------
//  Wishbone bus settings.
`define WBADR_BITS 12           // Address bit-width
`define WBBUS_BITS  8           // Bit-width of the SoC data bus
