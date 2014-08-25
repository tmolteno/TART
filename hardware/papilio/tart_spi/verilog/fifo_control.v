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
   input [4:0] tx_status_cnt
   );

   parameter SDRAM_ADDRESS_WIDTH = 22;
   parameter BLOCKSIZE = 8'd32;
// parameter FILL_THRESHOLD = (1 << 20)-BLOCKSIZE;
// parameter FILL_THRESHOLD = (1 << 21)-BLOCKSIZE;
// parameter FILL_THRESHOLD = (21'b11111111 << 13); // didnt work
// parameter FILL_THRESHOLD = (21'b1111111 << 14);  //  didnt work
   parameter FILL_THRESHOLD = (21'b111111 << 15);   //  2064384 DID WORK
   

   reg [SDRAM_ADDRESS_WIDTH-2:0] sdram_wr_ptr = 0;
   reg [SDRAM_ADDRESS_WIDTH-2:0] sdram_rd_ptr = 0;

   reg [1:0] Sync_start = 2'b0;
   wire spi_start_aq_int;
   always @(posedge write_clk) Sync_start[0] <= spi_start_aq;   // notice that we use clkB
   always @(posedge write_clk) Sync_start[1] <= Sync_start[0];   // notice that we use clkB
   assign spi_start_aq_int = Sync_start[1];  // new signal synchronized to (=ready to be used in) clkB domain  
   
   always @(posedge write_clk or posedge rst)
      begin
         if (rst) aq_write_en <= 1'b0;
         else if (spi_start_aq_int) aq_write_en <= 1'b1;
      end

   reg [5:0] rcnt = 6'b0;

   parameter AQ_WAITING       = 3'd0;
   parameter AQ_READ_ONE      = 3'd5;
   parameter AQ_FIFO_TO_SDRAM = 3'd1;
   parameter TX_WRITING       = 3'd2;
   parameter TX_IDLE          = 3'd3;
   parameter FINISHED         = 3'd4;
   
   reg [2:0] tart_state = AQ_WAITING;
   
   always @(posedge bb_clk or posedge rst)
      begin
         if (rst)
            begin
               sdram_wr_ptr <= 0;
               sdram_rd_ptr <= 0;
               aq_read_en   <= 1'b0;
               tart_state   <= AQ_WAITING;
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
                     bb_filled <= 1;
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
