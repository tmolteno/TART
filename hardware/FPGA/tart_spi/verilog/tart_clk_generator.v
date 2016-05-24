`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    17:29:42 09/02/2014 
// Design Name: 
// Module Name:    tart_clk_generator 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////
module tart_clk_generator(
      input wire  CLKIN, // 16.368 MHZ
      output wire CLKOUT0, // 16.368 MHZ buffered
      output wire CLKOUT1, // 16.368x6 = 98.208 MHz
      output      reset_n
    );

   wire GND_PIN; assign GND_PIN = 0;
   wire CLK0;
   
   IBUFG #(
         .IOSTANDARD("DEFAULT")
      ) IBUFG_inst (
         .O(IBUFG_OUT), // Clock buffer output
         .I(CLKIN)      // Clock buffer input (connect directly to top-level port)
      );

   BUFIO2 #(
      .DIVIDE(1),             // DIVCLK divider (1-8)
      .DIVIDE_BYPASS("TRUE"), // Bypass the divider circuitry (TRUE/FALSE)
      .I_INVERT("FALSE"),     // Invert clock (TRUE/FALSE)
      .USE_DOUBLER("FALSE")   // Use doubler circuitry (TRUE/FALSE)
   )
   BUFIO2_inst (
      .DIVCLK(DIVCLK),               // 1-bit output: Divided clock output
      //.IOCLK(IOCLK),               // 1-bit output: I/O output clock
      //.SERDESSTROBE(SERDESSTROBE), // 1-bit output: Output SERDES strobe (connect to ISERDES2/OSERDES2)
      .I(IBUFG_OUT)                  // 1-bit input: Clock input (connect to IBUFG)
   );

   DCM_SP #(
      .CLKDV_DIVIDE(2.0),                   // CLKDV divide value
                                            // (1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,9,10,11,12,13,14,15,16).
      .CLKFX_DIVIDE(1),                     // Divide value on CLKFX outputs - D - (1-32)
      .CLKFX_MULTIPLY(6),                   // Multiply value on CLKFX outputs - M - (2-32)
      .CLKIN_DIVIDE_BY_2("FALSE"),          // CLKIN divide by two (TRUE/FALSE)
      .CLKIN_PERIOD(61.095),                // Input clock period specified in nS
      .CLKOUT_PHASE_SHIFT("NONE"),          // Output phase shift (NONE, FIXED, VARIABLE)
      .CLK_FEEDBACK("1X"),                  // Feedback source (NONE, 1X, 2X)
      .DESKEW_ADJUST("SYSTEM_SYNCHRONOUS"), // SYSTEM_SYNCHRNOUS or SOURCE_SYNCHRONOUS
      .DFS_FREQUENCY_MODE("LOW"),           // Unsupported - Do not change value
      .DLL_FREQUENCY_MODE("LOW"),           // Unsupported - Do not change value
      .DSS_MODE("NONE"),                    // Unsupported - Do not change value
      .DUTY_CYCLE_CORRECTION("TRUE"),       // Unsupported - Do not change value
      .FACTORY_JF(16'hc080),                // Unsupported - Do not change value
      .PHASE_SHIFT(0),                      // Amount of fixed phase shift (-255 to 255)
      .STARTUP_WAIT("FALSE")                // Delay config DONE until DCM_SP LOCKED (TRUE/FALSE)
   )
   DCM_SP_inst (
      .CLK0(CLK0),         // 1-bit output: 0 degree clock output
      //.CLK180(CLK180),     // 1-bit output: 180 degree clock output
      //.CLK270(CLK270),     // 1-bit output: 270 degree clock output
      //.CLK2X(CLK2X),       // 1-bit output: 2X clock frequency clock output
      //.CLK2X180(CLK2X180), // 1-bit output: 2X clock frequency, 180 degree clock output
      //.CLK90(CLK90),       // 1-bit output: 90 degree clock output
      //.CLKDV(CLKDV),       // 1-bit output: Divided clock output
      .CLKFX(CLKFX),         // 1-bit output: Digital Frequency Synthesizer output (DFS)
      //.CLKFX180(CLKFX180), // 1-bit output: 180 degree CLKFX output
      .LOCKED(reset_n),     // 1-bit output: DCM_SP Lock Output
      //.PSDONE(PSDONE),     // 1-bit output: Phase shift done output
      //.STATUS(STATUS),     // 8-bit output: DCM_SP status output
      .CLKFB(CLK0),       // 1-bit input: Clock feedback input
      .CLKIN(DIVCLK),        // 1-bit input: Clock input
      .DSSEN(GND_PIN),       // 1-bit input: Unsupported, specify to GND.
      .PSCLK(GND_PIN),       // 1-bit input: Phase shift clock input
      .PSEN(GND_PIN),        // 1-bit input: Phase shift enable
      .PSINCDEC(GND_PIN),    // 1-bit input: Phase shift increment/decrement input
      .RST(GND_PIN)          // 1-bit input: Active high reset input
   );



   BUFG BUFG_inst0 (
      .O(CLKOUT0), // 1-bit output: Clock buffer output
      .I(DIVCLK)   // 1-bit input: Clock buffer input
   );
   
   BUFG BUFG_inst1 (
      .O(CLKOUT1), // 1-bit output: Clock buffer output
      .I(CLKFX)    // 1-bit input: Clock buffer input
   );


endmodule
