`timescale 1ns/100ps
module block_buffer
  (
   output reg [DATA_MSB:0] read_data = {DATA_WIDTH{1'b0}},
   input [DATA_MSB:0]      write_data,
   input                   clk,
   input [ADDR_WIDTH-1:0]  read_address,
   input [ADDR_WIDTH-1:0]  write_address
   );

   parameter DATA_WIDTH = 24;
   parameter DATA_MSB   = DATA_WIDTH - 1;
   parameter ADDR_WIDTH = 9;
   parameter ADDR_MSB   = ADDR_WIDTH - 1;
   parameter MEM_DEPTH  = 1 << ADDR_WIDTH;
   parameter DELAY      = 3;

`ifdef __icarus
   reg [DATA_MSB:0] block_buffer [0:MEM_DEPTH-1];

   assign read_data = block_data;

   //initialize all RAM cells to 0FF1CE at startup
   integer j;
   initial
     for (j=0; j <MEM_DEPTH; j = j+1) block_buffer[j] = 24'h0FF1CE;

   always @(posedge clk) begin
      block_buffer[write_address] <= #DELAY write_data;
      block_data <= #DELAY block_buffer[read_address];
   end

`else
   wire [13:0]      ADDRA = {read_address , {14-ADDR_WIDTH{1'b0}}};
   wire [13:0]      ADDRB = {write_address, {14-ADDR_WIDTH{1'b0}}};

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
         .DIA(write_data),      // 32-bit input: A port data input
         .DIPA(DIPA),           // 4-bit input: A port parity input
         .DOA(),                // 32-bit output: A port data output
         .DOPA(),               // 4-bit output: A port parity output
         
         // Port B Address/Control Signals: 14-bit (each) input: Port B address and control signals
         .ADDRB(ADDRB),         // B port address input
         .CLKB(clk),            // B port clock input
         .ENB(1'b1),            // B port enable input
         .REGCEB(1'b0),         // B port register clock enable input
         .RSTB(1'b0),           // B port register set/reset input
         .WEB(4'b0000),         // Port B byte-wide write enable input
         // Port B Data:
         .DIB(DIB),             // 32-bit input: B port data input
         .DIPB(DIPB)            // 4-bit input: B port parity input
         .DOB(DOB),             // 32-bit output: B port data output
         .DOPB(DOPB)            // 4-bit output: B port parity output
         );
`endif // !`ifdef __icarus


endmodule // block_buffer
