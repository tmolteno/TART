`timescale 1ns/1ps
module block_buffer
  (
   output reg [DATA_MSB:0] read_data = {DATA_WIDTH{1'b0}},
   input [DATA_MSB:0]      write_data,
   input                   clk,
   input [ADDR_WIDTH-1:0]  read_address,
   input [ADDR_WIDTH-1:0]  write_address
   );

   parameter DATA_WIDTH = 24;
   parameter DATA_MSB = DATA_WIDTH - 1;
   parameter ADDR_WIDTH = 9;
   parameter ADDR_MSB = ADDR_WIDTH - 1;
   parameter MEM_DEPTH = 1 << ADDR_WIDTH;

   reg [DATA_MSB:0] block_buffer [0:MEM_DEPTH-1];

   //initialize all RAM cells to 0FF1CE at startup
   integer j;
   initial
     for (j=0; j <MEM_DEPTH; j = j+1) block_buffer[j] = 24'h0FF1CE;

   always @(posedge clk)
     begin
        block_buffer[write_address] <= write_data;
        read_data <= block_buffer[read_address];
     end

endmodule // block_buffer
