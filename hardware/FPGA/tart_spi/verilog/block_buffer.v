`timescale 1ns/100ps
module block_buffer
  #(parameter WIDTH = 24,
    parameter MSB   = WIDTH-1,
    parameter ABITS = 9,
    parameter ASB   = ABITS-1,
    parameter DEPTH = 1 << ABITS,
    parameter DELAY = 3)
  (
   output [MSB:0] read_data,
   input [MSB:0]  write_data,
   input          clk,
   input [ASB:0]  read_address,
   input [ASB:0]  write_address
   );

// `define __USE_EXPLICT_BRAM
`ifndef __USE_EXPLICT_BRAM
   reg [MSB:0] block_buffer [0:DEPTH-1];
   reg [MSB:0] block_data = {WIDTH{1'b0}};

   assign read_data = block_data;

   //initialize all RAM cells to 0FF1CE at startup
   integer j;
   initial
     for (j=0; j <DEPTH; j = j+1) block_buffer[j] = 24'h0FF1CE;

   always @(posedge clk) begin
      block_buffer[write_address] <= #DELAY write_data;
      block_data <= #DELAY block_buffer[read_address];
   end

`else
   wire [13:0]      ADDRA = {read_address , {14-ADDR_WIDTH{1'b0}}};
   wire [13:0]      ADDRB = {write_address, {14-ADDR_WIDTH{1'b0}}};
   wire [31:0]      a_data = {{(32-DATA_WIDTH){1'b0}}, write_data};
   wire [31:0]      b_data;

   RAMB16BWER
     #(  // DATA_WIDTH_A/DATA_WIDTH_B: 0, 1, 2, 4, 9, 18, or 36
         .DATA_WIDTH_A(36),
         .DATA_WIDTH_B(36),
         // DOA_REG/DOB_REG: Optional output register (0 or 1)
         .DOA_REG(0),
         .DOB_REG(0),
         // EN_RSTRAM_A/EN_RSTRAM_B: Enable/disable RST
         .EN_RSTRAM_A("FALSE"),
         .EN_RSTRAM_B("FALSE"),
         .RSTTYPE("SYNC"),
         .RST_PRIORITY_A("CE"),
         .RST_PRIORITY_B("CE"),
         .SIM_COLLISION_CHECK("NONE"),
         .SIM_DEVICE("SPARTAN6"),
         .INIT_A(36'h000000000),
         .INIT_B(36'h000000000),
         .SRVAL_A(36'h000000000),
         .SRVAL_B(36'h000000000),
         // WRITE_MODE_A/WRITE_MODE_B: "WRITE_FIRST", "READ_FIRST", or "NO_CHANGE" 
         .WRITE_MODE_A("WRITE_FIRST"),
         .WRITE_MODE_B("WRITE_FIRST")
       ) BLOCK_BUFFER0
       ( .ADDRA(ADDRA),
         .CLKA(clk),
         .ENA(1'b1),
         .REGCEA(1'b0),         // A port register clock enable input
         .RSTA(1'b0),           // A port register set/reset input
         .WEA(4'b1111),         // Port A byte-wide write enable input
         // Port A Data:
         .DIA(32'h0),           // 32-bit input: A port data input
         .DIPA(4'h0),           // 4-bit input: A port parity input
         .DOA(a_data),          // 32-bit output: A port data output
         .DOPA(),               // 4-bit output: A port parity output
         
         // Port B Address/Control Signals: 14-bit (each) input: Port B address and control signals
         .ADDRB(ADDRB),         // B port address input
         .CLKB(clk),            // B port clock input
         .ENB(1'b1),            // B port enable input
         .REGCEB(1'b0),         // B port register clock enable input
         .RSTB(1'b0),           // B port register set/reset input
         .WEB(4'b0000),         // Port B byte-wide write enable input
         // Port B Data:
         .DIB(b_data),          // 32-bit input: B port data input
         .DIPB(4'h0),           // 4-bit input: B port parity input
         .DOB(),                // 32-bit output: B port data output
         .DOPB()                // 4-bit output: B port parity output
         );
`endif // !`ifdef __USE_EXPLICT_BRAM


endmodule // block_buffer
