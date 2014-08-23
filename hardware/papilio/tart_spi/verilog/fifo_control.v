module fifo_sdram_fifo_scheduler(
   input rst,

   input spi_start_aq,

   input write_clk,
   input [7:0] status_cnt,
   output reg aq_write_en = 0,
   output reg aq_read_en = 0,
  
   input bb_clk,

   output reg [BLOCK_BUFFER_ADDR_WIDTH-1:0] block_buffer_write_ptr = 0,
   output reg bb_filled=0,
   output reg [BLOCK_BUFFER_ADDR_WIDTH-1:0] block_buffer_read_ptr = 0,
   
   output reg tx_write_en = 0,
   output reg tx_ready_for_first_read=0,
   input [4:0] tx_status_cnt

   );

   parameter BLOCKSIZE = 8'd32;
   parameter BLOCK_BUFFER_ADDR_WIDTH = 9;
   parameter FILL_THRESHOLD = (1 << BLOCK_BUFFER_ADDR_WIDTH)-BLOCKSIZE;


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
   reg read_delay = 1'b0;

   parameter AQ_WAITING       = 3'd0;
   parameter AQ_FIFO_TO_SDRAM = 3'd1;
   parameter TX_WRITING       = 3'd2;
   parameter TX_IDLE          = 3'd3;
   parameter FINISHED         = 3'd4;
   
   reg [2:0] tart_state = AQ_WAITING;
   
   always @(tart_state)
      begin
         case (tart_state)
            AQ_WAITING:       aq_read_en  <= 1'b0;
            AQ_FIFO_TO_SDRAM: aq_read_en  <= 1'b1;
            TX_WRITING:       tx_write_en <= 1'b1;
            TX_IDLE:          tx_write_en <= 1'b0;
            FINISHED:         tx_write_en <= 1'b0;
            default:
               begin
                  aq_read_en  <= 1'b0;
                  tx_write_en <= 1'b0;
               end
         endcase
      end

   always @(posedge bb_clk or posedge rst)
      begin
         if (rst)
            begin
               read_delay <= 1'b0;
               block_buffer_write_ptr <= 0;
               block_buffer_read_ptr <= 0;
               tart_state <= AQ_WAITING;
            end
         else
            case (tart_state)
               AQ_WAITING:
                  begin
                     if (block_buffer_write_ptr >= FILL_THRESHOLD) tart_state <= TX_WRITING;
                     else if (status_cnt >=  BLOCKSIZE)            tart_state <= AQ_FIFO_TO_SDRAM;
                  end
               AQ_FIFO_TO_SDRAM:
                  begin
                     if (read_delay == 1'b1)
                        begin
                           block_buffer_write_ptr <= block_buffer_write_ptr + 1'b1;
                           $display("READING FROM BUFFER  no.%d of %d : %d", rcnt, BLOCKSIZE, block_buffer_write_ptr);
                           if (rcnt == BLOCKSIZE-1)
                              begin
                                 rcnt <= 6'b0;
                                 tart_state <= AQ_WAITING;
                              end
                           else rcnt <= rcnt + 1'b1;
                        end
                     else read_delay <= 1'b1;
                  end
               TX_WRITING:
                     begin
                        bb_filled <= 1;
                        $display("FILLING TX BUFFER: bb_rd_ptr: %d", block_buffer_read_ptr);
                        if (tx_status_cnt > 5'd15)
                           begin 
                              $display("WRITING2IDLE: TX BUFFER: bb_rd_ptr: %d", block_buffer_read_ptr);
                              tart_state <= TX_IDLE;
                              tx_ready_for_first_read <= 1;
                           end
                        else block_buffer_read_ptr <= block_buffer_read_ptr + 1'b1;
                     end
               TX_IDLE:
                  begin
                     if (block_buffer_read_ptr < FILL_THRESHOLD)
                        begin 
                           if (tx_status_cnt<5'd5)
                              begin
                                 $display("IDLE2WRITING: TX BUFFER: bb_rd_ptr: %d", block_buffer_read_ptr);
                                 block_buffer_read_ptr <= block_buffer_read_ptr + 1'b1;
                                 tart_state <= TX_WRITING;
                              end
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
