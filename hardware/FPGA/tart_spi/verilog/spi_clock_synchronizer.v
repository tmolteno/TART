/*************************** TART OPERATION REGISTERS ***********************
   by Tim Molteno. tim@elec.ac.nz
   & Max Scheel   max@max.ac.nz
 ****************************************************************************/

`timescale 1ns/1ps
module spi_clock_synchronizer(input fpga_clk, input SCK, input MOSI, input SSEL,
                              output SCK_risingedge, output SCK_fallingedge,
                              output SSEL_active, output SSEL_startmessage, output SSEL_endmessage,
                              output MOSI_data);
   // sync SCK to the FPGA clock using a 3-bits shift register
   reg [2:0] SCKr;  always @(posedge fpga_clk) SCKr <= {SCKr[1:0], SCK};
   assign SCK_risingedge = (SCKr[2:1]==2'b01);  // now we can detect SCK rising edges
   assign SCK_fallingedge = (SCKr[2:1]==2'b10);  // and falling edges

   // same thing for SSEL
   reg [2:0] SSELr;  always @(posedge fpga_clk) SSELr <= {SSELr[1:0], SSEL};
   assign SSEL_active = ~SSELr[1];  // SSEL is active low
   assign SSEL_startmessage = (SSELr[2:1]==2'b10);  // message starts at falling edge
   assign SSEL_endmessage = (SSELr[2:1]==2'b01);  // message stops at rising edge

   // and for MOSI
   reg [1:0] MOSIr;  always @(posedge fpga_clk) MOSIr <= {MOSIr[0], MOSI};
   assign MOSI_data = MOSIr[1];
endmodule
