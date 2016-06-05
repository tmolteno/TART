`timescale 1ns / 1ps
/*
 *
 * TODO:
 *  + needs a pair of DCM's, so that phase can be locked?
 * 
 */

// Create Date:    17:29:42 09/02/2014 
module tart_dcm
  ( input  clk_pin,             // 16.368 MHZ
    input  clk_rst,             // Active HIGH, to reset the DCM
     
    output clk,                 // 16.368 MHZ buffered
    output reset_n,             // LOW when the clock isn't locked
    output status_n,            // LOW indicates that the clocks are toggling
    output clk6x,               // 16.368x6 = 98.208 MHz
    output clk12x               // 16.368x12 = 196.416 MHz
    );

   wire [7:0] STATUS;
   wire       clk_ibufg, clk_buf, clk6x_dcm, clk12x_dcm;
   wire       status_n = STATUS[2];

   //-------------------------------------------------------------------------
   //  Local I/O clock resources.
   //-------------------------------------------------------------------------
   // Bring clock signal from the I/O pin into a local clock buffer.
   IBUFG #( .IOSTANDARD("DEFAULT") )
   IBUFG_SYSCLK0 ( .I(clk_pin), .O(clk_ibufg) );

   // Pass the clock through the local BUFIO2 primitive (which can provide
   // optional delays, and frequency-division).
   BUFIO2
     #(  .DIVIDE(1),             // DIVCLK divider (1-8)
         .DIVIDE_BYPASS("TRUE"), // Bypass the divider circuitry (TRUE/FALSE)
         .I_INVERT("FALSE"),     // Invert clock (TRUE/FALSE)
         .USE_DOUBLER("FALSE")   // Use doubler circuitry (TRUE/FALSE)
         ) BUFIO2_IOCLK0
       ( .I(clk_ibufg),         // Clock input (connect to IBUFG)
         .DIVCLK(clk_buf),      // Divided (optional) clock output
         .IOCLK(IOCLK),         // 1-bit output: I/O output clock
         .SERDESSTROBE(SERDESSTROBE) // unused
         );

   //-------------------------------------------------------------------------
   //  Global clock network resources.
   //-------------------------------------------------------------------------
   // Synthesise the TART system clocks.
   DCM_CLKGEN
     #(  .SPREAD_SPECTRUM("NONE"),
         .CLKIN_PERIOD(61.095),  // Input clock period specified in nS
         .CLKFX_MULTIPLY(12), // Multiply value on CLKFX outputs - M - (2-256)
         .CLKFX_DIVIDE(1), // Divide value on CLKFX outputs - D - (1-256)
         .CLKFXDV_DIVIDE(2), // CLKFXDV divide value, from {(2),4,8,16,32}
//          .DFS_OSCILLATOR_MODE("PHASE_FREQ_LOCK"),
         .STARTUP_WAIT("FALSE") // Delay config DONE until DCM_CLKGEN LOCKED (TRUE/FALSE)
         ) TART_DCM0
       ( .CLKIN(clk_buf),        // 1-bit input: Clock input
         .RST(clk_rst),          // 1-bit input: Active high reset input
         .CLKFX(clk12x_dcm),     // 1-bit output: Digital Frequency Synthesizer output (DFS)
         .CLKFX180(clk12x_n_dcm),// 1-bit output: 180 degree CLKFX output
         .CLKFXDV(clk6x_dcm),    // 1-bit output: Digital Frequency Synthesizer output (DFS)
         .STATUS(STATUS),        // 8-bit output: DCM_CLKGEN status output
         .LOCKED(reset_n)        // 1-bit output: DCM_CLKGEN Lock Output
         );

   // Global clock buffers for TART's system clocks.
   BUFG BUFG_SYSCLK ( .I(   clk_buf), .O(   clk) );
   BUFG BUFG_CLK6X  ( .I( clk6x_dcm), .O( clk6x) );
   BUFG BUFG_CLK12X ( .I(clk12x_dcm), .O(clk12x) );


endmodule // tart_dcm
