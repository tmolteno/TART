module fifo_sdram_fifo_scheduler(
   input rst,

   input spi_start_aq,

   input write_clk,
   input [7:0] status_cnt,
   output reg aq_write_en = 0,
   output reg aq_read_en = 0,
  
   input bb_clk,

   output reg bb_filled=0,

   input cmd_ready,
   output reg cmd_enable = 0,     
   output reg cmd_wr = 0,
   output reg [SDRAM_ADDRESS_WIDTH-2:0] cmd_address  = 0,
   
   output reg tx_ready_for_first_read=0,
   input [4:0] tx_status_cnt,
   output wire [23:0] outtt, // remove me
   output reg [2:0] tart_state = AQ_WAITING
   );


   wire [23:0] aq_bb_rd_data;
   reg  [23:0] aq_bb_wr_data = 0;
   reg [8:0] aq_bb_rd_address = 0;
   reg [8:0] aq_bb_wr_address = 0;

   // Instantiate the module
   block_buffer aq_bb(
       .read_data(outtt), 
       .write_data(aq_bb_wr_data), 
       .clk(bb_clk), 
       .read_address(aq_bb_rd_address), 
       .write_address(aq_bb_wr_address)
       );
   //reg [23:0] outtt;
//
//
//   aq_bb_wr_address <= aq_bb_wr_address + 1'b1;
//   if (aq_bb_rd_address != aq_bb_wr_address)
//      begin
//         aq_bb_rd_address <= aq_bb_rd_address + 1'b1;
//         out <= aq_bb_rd_data;
//      end 

   parameter SDRAM_ADDRESS_WIDTH = 22;
   parameter BLOCKSIZE = 8'd32;
   
   // 1 bit bigger than needed. 
   parameter FILL_THRESHOLD = (22'h1FFFFF);
   // 1 bit bigger than needed. 
   reg [SDRAM_ADDRESS_WIDTH-1:0] sdram_wr_ptr = 0;
   reg [SDRAM_ADDRESS_WIDTH-1:0] sdram_rd_ptr = 0;

   reg [1:0] Sync_start = 2'b0;  
   // new signal synchronized to (=ready to be used in) clkB domain
   wire spi_start_aq_int; assign spi_start_aq_int = Sync_start[1]; 
   
   always @(posedge write_clk or posedge rst)
      begin
         if (rst)
            begin
              Sync_start <= 2'b00;
              aq_write_en <= 1'b0;
            end
         else
           begin
             aq_bb_wr_address <= aq_bb_wr_address+ 1'b1;
             aq_bb_wr_data <= 24'b1;
             if (aq_bb_rd_address != aq_bb_wr_address)
               begin
                  aq_bb_rd_address <= aq_bb_rd_address + 1'b1;
                  //outtt <= aq_bb_rd_data;
               end 
             Sync_start[0] <= spi_start_aq;
             Sync_start[1] <= Sync_start[0];
             if (spi_start_aq_int) aq_write_en <= 1'b1;
           end
      end
   
   parameter AQ_WAITING       = 3'd0;
   parameter AQ_READ_ONE      = 3'd5;
   parameter AQ_FIFO_TO_SDRAM = 3'd1;
   parameter TX_WRITING       = 3'd2;
   parameter TX_IDLE          = 3'd3;
   parameter FINISHED         = 3'd4;
   
   reg [5:0] rcnt = 6'b0;

   always @(posedge bb_clk or posedge rst)
      begin
         if (rst)
            begin
               sdram_wr_ptr <= 22'b0; // 1 bit bigger than needed. 
               sdram_rd_ptr <= 22'b0; // 1 bit bigger than needed. 
               aq_read_en   <= 1'b0;
               tart_state   <= AQ_WAITING;
               rcnt <= 6'b0;
               cmd_enable <= 1'b0;
               cmd_wr    <= 1'b0;
               bb_filled <= 1'b0;
            end
         else
            case (tart_state)
               AQ_WAITING:
                  begin
                     if (sdram_wr_ptr >= FILL_THRESHOLD)
                        begin
                           tart_state <= TX_WRITING;
                        end
                     else if (status_cnt >= BLOCKSIZE)
                        begin
                           tart_state <= AQ_READ_ONE;
                        end
                  end
               AQ_READ_ONE:
                  begin
                     if (rcnt == BLOCKSIZE-1)
                        begin
                           rcnt <= 6'b0;
                           tart_state <= AQ_WAITING;
                        end
                     else
                        begin
                           rcnt <= rcnt + 1'b1;
                           aq_read_en <= 1'b1;
                           tart_state <= AQ_FIFO_TO_SDRAM;
                        end
                  end
               AQ_FIFO_TO_SDRAM:
                  begin
                     aq_read_en <= 1'b0;
                     if (cmd_enable) cmd_enable <= 0;
                     else if (cmd_ready)
                        begin
                           cmd_wr       <= 1'b1;
                           cmd_enable   <= 1'b1;
                           cmd_address  <= sdram_wr_ptr;
                           sdram_wr_ptr <= sdram_wr_ptr + 1'b1;
                           tart_state <= AQ_READ_ONE;
                        end
                  end
               TX_WRITING:
                  begin
                     bb_filled <= 1'b1;
                     if (tx_status_cnt > 5'd15)
                        begin
                           tx_ready_for_first_read <= 1'b1;
                           tart_state <= TX_IDLE;
                        end
                     else
                        begin
                           if (cmd_enable) cmd_enable <= 1'b0;
                           else if (cmd_ready)
                             begin
                                cmd_wr       <= 1'b0;
                                cmd_enable   <= 1'b1;
                                cmd_address  <= sdram_rd_ptr;
                                sdram_rd_ptr <= sdram_rd_ptr + 1'b1;
                             end
                       end
                     end
               TX_IDLE:
                  begin
                     if (sdram_rd_ptr < FILL_THRESHOLD)
                        begin 
                           if (tx_status_cnt<5'd5) tart_state <= TX_WRITING;
                        end
                     else tart_state <= FINISHED;
                  end
               FINISHED:
                  begin
                     $display("Finished.");
                  end
            endcase
      end  
endmodule
