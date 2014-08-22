module block_buffer(
   output reg [23:0] read_data = 24'b0,
   input      [23:0] write_data,
   input clk,
   input [BLOCK_BUFFER_ADDR_WIDTH-1:0] read_address,
   input [BLOCK_BUFFER_ADDR_WIDTH-1:0] write_address
   );

   parameter BLOCK_BUFFER_DEPTH = 512;
   parameter BLOCK_BUFFER_ADDR_WIDTH = 10;

   reg [23:0] block_buffer [BLOCK_BUFFER_DEPTH-1:0];
   //initialize all RAM cells to 0FF1CE at startup
   integer init_j;
   initial for (init_j=0; init_j <BLOCK_BUFFER_DEPTH; init_j = init_j + 1) block_buffer[init_j] = 24'h0FF1CE; 


   always @(posedge clk)
      begin
         block_buffer[write_address] <= write_data;
         read_data <= block_buffer[read_address];
      end

endmodule
