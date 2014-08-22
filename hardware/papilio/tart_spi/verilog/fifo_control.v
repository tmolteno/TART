//
//     AQUISITION FIFO CONTROL
//

module aq_fifo_ctl(
   output reg aq_read_en = 0,
   output reg aq_write_en = 0,
   output reg [BLOCK_BUFFER_ADDR_WIDTH-1:0] block_buffer_write_ptr = 0,
   output wire bb_filled,
   input [7:0] status_cnt,
   input rst,
   input spi_start_aq,
   input write_clk,
   input read_clk,
   input fpga_clk
   );

   parameter BLOCKSIZE = 8'd32;
   parameter BLOCK_BUFFER_ADDR_WIDTH = 9;
   parameter FILL_THRESHOLD = (1 << BLOCK_BUFFER_ADDR_WIDTH)-BLOCKSIZE;//

   parameter  WAITING = 2'd0;
   parameter  READING = 2'd1;
   parameter  WRITING = 2'd2;
   parameter FINISHED = 2'd3;

   reg [1:0] w_state = WAITING;
   always @(w_state)
      begin
         case (w_state)
            WAITING: aq_write_en <= 0;
            WRITING: aq_write_en <= 1;
            FINISHED:aq_write_en <= 0;
            default: aq_write_en <= 0;
         endcase
      end
    
   reg [1:0] r_state = WAITING;
   always @(r_state)
      begin
         case (r_state)
            WAITING: aq_read_en <= 0;
            READING: aq_read_en <= 1;
            default: aq_read_en <= 0;
         endcase
      end


   reg bb_filled_int = 1'b0;
   wire spi_start_aq_int;

   reg [1:0] Sync_start = 2'b0;
   reg [1:0] Sync_stop = 2'b0;

   always @(posedge write_clk) Sync_start[0] <= spi_start_aq;   // notice that we use clkB
   always @(posedge write_clk) Sync_start[1] <= Sync_start[0];   // notice that we use clkB
   assign spi_start_aq_int = Sync_start[1];  // new signal synchronized to (=ready to be used in) clkB domain

   always @(posedge write_clk) Sync_stop[0] <= bb_filled_int;   // notice that we use clkB
   always @(posedge write_clk) Sync_stop[1] <= Sync_stop[0];   // notice that we use clkB
   assign bb_filled = Sync_stop[1];  // new signal synchronized to (=ready to be used in) clkB domain

  
   always @(posedge write_clk or posedge rst)
      begin
         if (rst)
            begin
               w_state <= WAITING;
            end
      else
         case (w_state)
            WAITING: if (spi_start_aq_int) w_state <= WRITING;
            WRITING: if (bb_filled) w_state <= FINISHED;
            FINISHED:
               begin
                  //$display("FINISHED AQUISITION");
               end
         endcase
   end
   reg [5:0] rcnt = 6'b0;
   reg read_delay = 1'b0;

   always @(posedge read_clk or posedge rst)
      begin
         if (rst)
            begin
               read_delay <= 1'b0;
               block_buffer_write_ptr <= 0;
               r_state <= WAITING;
            end
         else
            case (r_state)
               WAITING:
                 begin
                  if (bb_filled_int==0 && block_buffer_write_ptr >= FILL_THRESHOLD) bb_filled_int <= 1'b1;
                  if (status_cnt >=  BLOCKSIZE) r_state <= READING;
                 end
               READING:
                  begin
                     if (read_delay == 1'b1)
                        begin
                           block_buffer_write_ptr <= block_buffer_write_ptr + 1'b1;
                           $display("READING FROM BUFFER  no.%d of %d : %d", rcnt, BLOCKSIZE, block_buffer_write_ptr);
                           if (rcnt == BLOCKSIZE-1)
                              begin
                                 rcnt <= 6'b0;
                                 r_state <= WAITING;
                              end
                           else rcnt <= rcnt + 1'b1;
                        end
                     else read_delay <= 1'b1;
                  end
            endcase
      end
endmodule


//
//     TRANSMISSION FIFO CONTROL
//

module tx_fifo_ctl(
   output reg [BLOCK_BUFFER_ADDR_WIDTH-1:0] block_buffer_read_ptr = 0,
   output reg tx_ready_for_first_read=0,
   input      [BLOCK_BUFFER_ADDR_WIDTH-1:0] block_buffer_write_ptr,
   input tx_rst,
   input [4:0] tx_status_cnt,
   output reg tx_write_en = 0,
   input tx_write_clk
   );

   parameter BLOCK_BUFFER_ADDR_WIDTH = 10;
   parameter BLOCKSIZE = 8'd32;
   parameter BLOCK_BUFFER_DEPTH = 512;

   parameter WAITING = 2'd0, WRITING = 2'd1, IDLE = 2'd2;

  reg [1:0] tx_state = WAITING;
  
   
   always @(posedge tx_write_clk or posedge tx_rst)
      begin
         if (tx_rst) 
            begin
               tx_state <= WAITING;
            end
      else case (tx_state)
         WAITING:
            begin
               if (block_buffer_write_ptr >= BLOCK_BUFFER_DEPTH-BLOCKSIZE)
                  begin
                     block_buffer_read_ptr <= block_buffer_read_ptr + 1'b1;
                     tx_state <= WRITING;
                  end
            end
         IDLE:
            begin
               if (block_buffer_read_ptr < BLOCK_BUFFER_DEPTH-BLOCKSIZE && tx_status_cnt<5'd5)
                  begin 
                     $display("IDLE2WRITING: TX BUFFER: bb_rd_ptr: %d", block_buffer_read_ptr);
                     block_buffer_read_ptr <= block_buffer_read_ptr + 1'b1;
                     tx_state <= WRITING;
                  end
            end
        WRITING:
            begin
               $display("FILLING TX BUFFER: bb_rd_ptr: %d", block_buffer_read_ptr);
               if (tx_status_cnt > 5'd15)
                  begin 
                     $display("WRITING2IDLE: TX BUFFER: bb_rd_ptr: %d", block_buffer_read_ptr);
                     tx_state <= IDLE;
                     tx_ready_for_first_read <= 1;
                  end
               else block_buffer_read_ptr <= block_buffer_read_ptr + 1'b1;
            end
         endcase
      end

  /* Behavior triggered when we enter each state */
  always @(tx_state)
    begin
        case (tx_state)
            WAITING: tx_write_en <= 1'b0;
            IDLE:    tx_write_en <= 1'b0;
            WRITING: tx_write_en <= 1'b1;
            default: tx_write_en <= 1'b0;
        endcase
    end
    
endmodule
