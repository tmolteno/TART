`timescale 1ns/100ps
/*
 * Module      : verilog/tart_dual_dcm.v
 * Copyright   : (C) Tim Molteno     2017
 *             : (C) Max Scheel      2017
 *             : (C) Patrick Suggate 2017
 * License     : LGPL3
 * 
 * Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
 * Stability   : Experimental
 * Portability : only tested with a Papilio board (Xilinx Spartan VI)
 *
 * 
 * This file is part of TART.
 * 
 * TART is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * TART is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser Public License along with
 * TART.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * 
 * Description:
 * Uses two Xilinx DCM's for generating all of TART's system clocks.
 * 
 * NOTE:
 *  + now OBSOLETE;
 * 
 * TODO:
 *  + needs a pair of DCM's, so that phase can be locked?
 * 
 */

// Create Date:    17:29:42 09/02/2014 
module tart_dcm
  (
   input  clk_pin_i, // 16.368 MHZ
   input  clk_rst_i, // Active HIGH, to reset the DCM
     
   output clk_ext_o, // 16.368 MHZ buffered
   output clk6x_o,   // 16.368x6 = 98.208 MHz
   output clk6n_o,   // 16.368x6 = 98.208 MHz
   output clk12x_o,  // 16.368x12 = 196.416 MHz
   output reset_no,  // LOW when the clock isn't locked
   output status_no  // LOW indicates that the clocks are toggling
   );

   wire [7:0] STATUS;
   wire       clk_ibufg, clk_buf, clk_1, clk_2, clk_xtal, clk_mux, clk_fb;

   (* PERIOD = "10.18 ns" *)
   wire       clk6x_l;
   (* PERIOD = "10.18 ns" *)
   wire       clk6n_l;
   (* PERIOD = "5.091 ns" *)
   wire       clk12x_l;

   assign reset_no = reset0_n && reset1_n;


   //-------------------------------------------------------------------------
   //  Local I/O clock resources.
   //-------------------------------------------------------------------------
   // Bring clock signal from the I/O pin into a local clock buffer.
   IBUFG
     #(  .IOSTANDARD("DEFAULT")
        ) IBUFG_SYSCLK0
       ( .I(clk_pin_i),
         .O(clk_ibufg)
         );

   //-------------------------------------------------------------------------
   // Pass the clock through the local BUFIO2 primitive (which can provide
   // optional delays, and frequency-division).
   BUFIO2
     #( .DIVIDE(1),             // DIVCLK divider (1-8)
        .DIVIDE_BYPASS("TRUE"), // Bypass the divider circuitry (TRUE/FALSE)
        .I_INVERT("FALSE"),     // Invert clock (TRUE/FALSE)
        .USE_DOUBLER("FALSE")   // Use doubler circuitry (TRUE/FALSE)
        ) BUFIO2_IOCLK0
       (
        .I(clk_ibufg),         // Clock input (connect to IBUFG)
        .DIVCLK(clk_buf),      // Divided (optional) clock output
        .IOCLK(IOCLK),         // 1-bit output: I/O output clock
        .SERDESSTROBE()        // unused
        );

   //-------------------------------------------------------------------------
   // Compute the feedback signal, for the external clock.
   BUFIO2FB
     #( .DIVIDE_BYPASS("TRUE")  // Bypass the divider circuitry (TRUE/FALSE)
        ) BUFIO2_FBCLK0
       (
        .I(clk_ext_o),          // Clock input (connect to IBUFG)
        .O(clk_fb)              // Divided (optional) clock output
        );


   //-------------------------------------------------------------------------
   //  Xilinx DCM instances.
   //-------------------------------------------------------------------------
   // Condition the incoming clock.
   DCM_SP
     #(  .CLKDV_DIVIDE(2.0),             // CLKDV divide value
         // (1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,9,10,11,12,13,14,15,16).
         .CLKFX_DIVIDE(1),               // Divide value on CLKFX outputs - D - (1-32)
         .CLKFX_MULTIPLY(6),             // Multiply value on CLKFX outputs - M - (2-32)
         .CLKIN_DIVIDE_BY_2("FALSE"),    // CLKIN divide by two (TRUE/FALSE)
         .CLKIN_PERIOD(61.095),          // Input clock period specified in nS
         .CLKOUT_PHASE_SHIFT("NONE"),    // Output phase shift (NONE, FIXED, VARIABLE)
         .CLK_FEEDBACK("1X"),            // Feedback source (NONE, 1X, 2X)
         .DESKEW_ADJUST("SYSTEM_SYNCHRONOUS"), // SYSTEM_SYNCHRNOUS or SOURCE_SYNCHRONOUS
         .DFS_FREQUENCY_MODE("LOW"),     // Unsupported - Do not change value
         .DLL_FREQUENCY_MODE("LOW"),     // Unsupported - Do not change value
         .DSS_MODE("NONE"),              // Unsupported - Do not change value
         .DUTY_CYCLE_CORRECTION("TRUE"), // Unsupported - Do not change value
         .FACTORY_JF(16'hc080),          // Unsupported - Do not change value
         .PHASE_SHIFT(0),                // Amount of fixed phase shift (-255 to 255)
         .STARTUP_WAIT("FALSE")
         ) DCM0
       ( .CLKIN (clk_buf),  // 1-bit input: Clock input
         .RST   (clk_rst_i),// 1-bit input: Active high reset input
         .LOCKED(reset0_n), // 1-bit output: DCM_SP Lock Output
//          .CLKFB (clk_ext_o),// 1-bit input: Clock feedback input
         .CLKFB (clk_fb),   // 1-bit input: Clock feedback input

         .CLK0  (clk_1),    // 1-bit output: 0 degree clock output
         .CLKFX (clk6x_l),  // 1-bit output: Digital Frequency Synthesizer (DFS)
         .CLKFX180(clk6n_l),// 1-bit output: Digital Frequency Synthesizer (DFS)

         .DSSEN(1'b0),      // 1-bit input: Unsupported, specify to GND.
         .PSCLK(1'b0),      // 1-bit input: Phase shift clock input
         .PSEN(1'b0),       // 1-bit input: Phase shift enable
         .PSINCDEC(1'b0)    // 1-bit input: Phase shift inc/dec input
         );


`ifdef __USE_DCM_CLKGEN
   //-------------------------------------------------------------------------
   //  Clock-generator DCM mode (that supports free-running clocks, but isn't
   //  locked to the input-clock, by a constant ratio).
   //  
   //  NOTE: Not synchronous with `DCM0`, which makes clock domain-crossing
   //    much more difficult.
   //-------------------------------------------------------------------------
   assign status_no = STATUS[2];
   assign clk_xtal  = clk_1;

   // Synthesise the TART system clocks.
   (* DFS_OSCILLATOR_MODE = "PHASE_FREQ_LOCK" *)
   DCM_CLKGEN
     #(  .SPREAD_SPECTRUM("NONE"),
         .CLKIN_PERIOD(61.095),  // Input clock period specified in nS
         .CLKFX_MULTIPLY(12),    // Multiply value on CLKFX outputs - M - (2-256)
         .CLKFX_DIVIDE(1),       // Divide value on CLKFX outputs - D - (1-256)
         .CLKFXDV_DIVIDE(2),     // CLKFXDV divide value, from {(2),4,8,16,32}
         .STARTUP_WAIT("FALSE")  // Delay config DONE until DCM_CLKGEN LOCKED (TRUE/FALSE)
         ) TART_DCM1
       ( .CLKIN   (clk_buf),     // 1-bit input: Clock input
         .RST     (clk_rst_i),   // 1-bit input: Active high reset input
         .CLKFX   (clk12x_l),    // 1-bit output: Digital Frequency Synthesizer output (DFS)
         .CLKFX180(clk12x_n_l),  // 1-bit output: 180 degree CLKFX output
         .CLKFXDV (clk6x_gen),   // 1-bit output: Digital Frequency Synthesizer output (DFS)
         .STATUS  (STATUS),      // 8-bit output: DCM_CLKGEN status output
         .LOCKED  (reset1_n)     // 1-bit output: DCM_CLKGEN Lock Output
         );


`else // !`ifdef __USE_DCM_CLKGEN
   //-------------------------------------------------------------------------
   //  Standard DCM mode.
   //  
   //  NOTE: This is the DEFAULT.
   //-------------------------------------------------------------------------
   assign status_n = 1'b1;
   assign clk_xtal = clk_2;

   DCM_SP
     #(  .CLKDV_DIVIDE(2.0),                   // CLKDV divide value
         // (1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,9,10,11,12,13,14,15,16).
         .CLKFX_DIVIDE(1),                     // Divide value on CLKFX outputs - D - (1-32)
         .CLKFX_MULTIPLY(12),                  // Multiply value on CLKFX outputs - M - (2-32)
         .CLKIN_DIVIDE_BY_2("FALSE"),          // CLKIN divide by two (TRUE/FALSE)
         .CLKIN_PERIOD(61.095),                // Input clock period specified in nS
         .CLKOUT_PHASE_SHIFT("NONE"),          // Output phase shift (NONE, FIXED, VARIABLE)
         .CLK_FEEDBACK("1X"),                // Feedback source (NONE, 1X, 2X)
         .DESKEW_ADJUST("SYSTEM_SYNCHRONOUS"), // SYSTEM_SYNCHRNOUS or SOURCE_SYNCHRONOUS
         .DFS_FREQUENCY_MODE("LOW"),           // Unsupported - Do not change value
         .DLL_FREQUENCY_MODE("LOW"),           // Unsupported - Do not change value
         .DSS_MODE("NONE"),                    // Unsupported - Do not change value
         .DUTY_CYCLE_CORRECTION("TRUE"),       // Unsupported - Do not change value
         .FACTORY_JF(16'hc080),                // Unsupported - Do not change value
         .PHASE_SHIFT(0),                      // Amount of fixed phase shift (-255 to 255)
         .STARTUP_WAIT("FALSE")
         ) TART_DCM1
       ( .CLKIN (clk_buf),  // 1-bit input: Clock input
         .RST   (clk_rst_i),// 1-bit input: Active high reset input
         .LOCKED(reset1_n), // 1-bit output: DCM_SP Lock Output
//          .CLKFB (clk_ext_o),// 1-bit input: Clock feedback input
         .CLKFB (clk_fb),   // 1-bit input: Clock feedback input

         .CLK0  (clk_2),    // 1-bit output: 0 degree clock output
         .CLKFX (clk12x_l), // 1-bit output: Digital Frequency Synthesizer output (DFS)

         .DSSEN(0),         // 1-bit input: Unsupported, specify to GND.
         .PSCLK(0),         // 1-bit input: Phase shift clock input
         .PSEN(0),          // 1-bit input: Phase shift enable
         .PSINCDEC(0)       // 1-bit input: Phase shift increment/decrement input
         );
`endif //  `ifdef !__USE_DCM_CLKGEN


   //-------------------------------------------------------------------------
   //  Global clock network resources.
   //-------------------------------------------------------------------------
   // Global clock buffers for TART's system clocks.
   BUFG BUFG_SYSCLK ( .I(clk_xtal), .O(clk_ext_o) );
   BUFG BUFG_CLK6X  ( .I( clk6x_l), .O(  clk6x_o) );
   BUFG BUFG_CLK6N  ( .I( clk6n_l), .O(  clk6n_o) );
   BUFG BUFG_CLK12X ( .I(clk12x_l), .O( clk12x_o) );


endmodule // tart_dcm
