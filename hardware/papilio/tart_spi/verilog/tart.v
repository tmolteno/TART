`timescale 1ns/1ps

//  _____      _      ____    _____
// |_   _|    / \    |  _ \  |_   _|
//   | |     / _ \   | |_) |   | |
//   | |    / ___ \  |  _ <    | |
//   |_|   /_/   \_\ |_| \_\   |_|

module tart(
            output wire led,
            
            output wire SDRAM_CLK,
            output wire SDRAM_CKE,
            output wire SDRAM_CS,
            output wire SDRAM_RAS,
            output wire SDRAM_CAS,
            output wire SDRAM_WE,
            output wire [1:0] SDRAM_DQM,
            output wire [12:0] SDRAM_ADDR,
            output wire [1:0] SDRAM_BA,
            inout  wire [15:0] SDRAM_DQ,

            output wire spi_miso,
            input spi_sck, input spi_mosi, input spi_ssel, /* SPI */
            input fpga_clk_32, input rst,
            input rx_clk_16, /* 16.368 MHz receiver master clock */
            input [23:0] antenna /* Radio Data Interface */
           );

   parameter IDLE = 2'd0, SENDING = 2'd1, RECEIVING = 2'd2, ERROR = 2'd3;
   parameter DEBUG=1'b1;

   parameter SDRAM_ADDRESS_WIDTH = 22;
	parameter SDRAM_COLUMN_BITS   = 8;
	parameter SDRAM_STARTUP_CYCLES= 10100;
	parameter CYCLES_PER_REFRESH  = (64000*100)/4196-1; 

   //
   //     GENERATE DIFFERENT CLOCK DOMAINS
   //

   fake_tart_clk clknetwork(.CLK_IN1(fpga_clk_32), .CLK_OUT1(fpga_clk), .CLK_OUT2(fake_rx_clk));

   //
   //     GENERATE FAKE DATA (24 BIT COUNTER) FOR DEBUGGING
   //
  
   wire [23:0] fake_antenna;
   fake_telescope fake_tart (.write_clk(fake_rx_clk), .write_data(fake_antenna));

   wire rx_clk;
   wire [23:0] antenna_data;

   assign       rx_clk = (DEBUG) ? fake_rx_clk: rx_clk_16;
   assign antenna_data = (DEBUG) ? fake_antenna : antenna;

   //
   //     AQUISITION BLOCK
   //

   wire [23:0] aq_write_data;
   wire [23:0] aq_read_data;
   wire [7:0] aq_status_cnt;  // This is a count set in sync with the RD clk. it will never overstate the fullness of the fifo.

   ipcorefifo
   acquisition_fifo(
                     .wr_clk(rx_clk),                    // input wr_clk
                     .rd_clk(fpga_clk),                  // input rd_clk
                     .rd_data_count(aq_status_cnt),      // output [7 : 0] rd_data_count
                     .rst(rst),                          // input rst
                     .wr_en(aq_write_en),                // input wr_en
                     .rd_en(aq_read_en),                 // input rd_en
                     .din(antenna_data),                 // input [23 : 0] din
                     .dout(aq_read_data),                // output [23 : 0] dout
                     .full(aq_full),                     // output full
                     .empty(aq_empty)                    // output empty
                  );

//
//      STORAGE BLOCK
//
                    
  wire [4:0] tx_status_cnt;
                 
  wire [SDRAM_ADDRESS_WIDTH-2:0] cmd_address;
  
  fifo_sdram_fifo_scheduler
  #(.SDRAM_ADDRESS_WIDTH(SDRAM_ADDRESS_WIDTH))
  scheduler(
            .rst(rst),

            .aq_write_en(aq_write_en),
            .status_cnt(aq_status_cnt),
            .write_clk(rx_clk),
            .aq_read_en(aq_read_en),

            .bb_clk(fpga_clk),

            .tx_status_cnt(tx_status_cnt), 
            .tx_ready_for_first_read(tx_ready_for_first_read),

            .bb_filled(bb_filled),
            
            .cmd_ready(cmd_ready),
            .cmd_enable(cmd_enable), 
            .cmd_wr(cmd_wr),
            
            .cmd_address(cmd_address),

            .spi_start_aq(spi_start_aq)
            );

   assign led = bb_filled;
   wire [31:0] data_out;
   wire [31:0] cmd_data_in;
   assign cmd_data_in = aq_read_data [23:0];

      SDRAM_Controller 
             #(
             .sdram_address_width(SDRAM_ADDRESS_WIDTH),
             .sdram_column_bits(SDRAM_COLUMN_BITS),
             .sdram_startup_cycles(SDRAM_STARTUP_CYCLES),
             .cycles_per_refresh(CYCLES_PER_REFRESH)
               )
           hamster_sdram (
          .clk(fpga_clk), 
          .reset(rst), 
          .cmd_ready(cmd_ready), 
          .cmd_enable(cmd_enable),
          .cmd_wr(cmd_wr),               
          .cmd_address(cmd_address),
          .cmd_byte_enable(4'b1111), 
          .cmd_data_in(cmd_data_in),
          
          .data_out(data_out), 
          .data_out_ready(data_out_ready),
          
          .SDRAM_CLK(SDRAM_CLK), 
          .SDRAM_CKE(SDRAM_CKE), 
          .SDRAM_CS(SDRAM_CS), 
          .SDRAM_RAS(SDRAM_RAS), 
          .SDRAM_CAS(SDRAM_CAS), 
          .SDRAM_WE(SDRAM_WE), 
          .SDRAM_DQM(SDRAM_DQM), 
          .SDRAM_ADDR(SDRAM_ADDR), 
          .SDRAM_BA(SDRAM_BA), 
          .SDRAM_DATA(SDRAM_DQ)
       );
   
//
//     TRANSMISSION BLOCK
//

   wire [23:0] tx_read_data;
   wire tx_empty, tx_full;

   wire [4:0] tx_debug_status_cnt;
   reg tx_read_en = 0;
   reg read_to_be_done = 0;
   reg startup = 1;

   // THIS IS A WORKAROUND FOR THE FIFO TO WORK...
   always @(posedge fpga_clk)
      begin
         if (tx_empty) tx_read_en <= ~tx_read_en;
         else if (spi_buffer_read_complete) read_to_be_done <= 1;
         else if ((startup && tx_ready_for_first_read) || (tx_empty==0 && read_to_be_done==1))
            begin
               startup <= 0;
               tx_read_en <= 1;
               read_to_be_done <= 0;
            end
         else tx_read_en <= 0;
      end
  
   tx_fifo_ipcore
   tx_fifo(
            .rst(rst),                             // input rst
            .wr_clk(fpga_clk),                     // input wr_clk
            .rd_clk(fpga_clk),                     // input rd_clk
            .din(data_out[23:0]),                  // input [23 : 0] din
            .wr_en(data_out_ready),                // input wr_en
            .rd_en(tx_read_en),                    // input rd_en
            .dout(tx_read_data),                   // output [23 : 0] dout
            .full(tx_full),                        // output full
            .empty(tx_empty),                      // output empty
            .rd_data_count(tx_debug_status_cnt),   // output [4 : 0] rd_data_count
            .wr_data_count(tx_status_cnt)          // output [4 : 0] wr_data_count
          );

//
//     SPI SLAVE
//

   SPI_slave dut (.fpga_clk(fpga_clk),
                  .SCK(spi_sck), .MOSI(spi_mosi), .MISO(spi_miso), .SSEL(spi_ssel),
                  .antenna_data(tx_read_data),
                  .spi_status(8'b11110000),   // for now let spi_status just be constant 11110000
                  .spi_buffer_read_complete(spi_buffer_read_complete),
                  .spi_reset(spi_reset),
                  .spi_start_aq(spi_start_aq)
                  );

endmodule


module test_tart_tb();
   reg rst=0, clk32=0, sck=0, mosi, ssel=1;
   always #15.625 clk32 = !clk32; // 32 MHz Papilio
   reg [23:0] telescope_data = 0;				
   reg  telescope_clk = 0; always #30 telescope_clk = !telescope_clk; // ~16 MHz tart clock
   wire led;
   
   tart tartwithspi(
                     .fpga_clk_32(clk32), .rst(rst), .led(led),                        // Papilio    32 MHz onboard clock
                     .rx_clk_16(telescope_clk), .antenna(telescope_data),              // TELESCOPE  16.368 MHz receiver master clock
                     .spi_sck(sck), .spi_mosi(mosi), .spi_miso(miso), .spi_ssel(ssel)  // SPI 
                     );

   task halfsclk;
      begin
         #32.051;  	// T/2 =   32.05 ns || 15.6 MHz
         //#64.10;	// T/2 =   64.10 ns ||  7.8 MHz
      end
   endtask

   task fullsclk; begin halfsclk(); halfsclk(); end endtask
   
   task ssel_enable; begin ssel = 0; #200; end endtask
   task ssel_disable; begin #200; ssel = 1; #200; end endtask
  
   reg [7:0] slaveout = 8'bx;
   task spi_write;
      input[7:0] x;
      integer j;
      begin
         //$display("Begin SPI write=%x %b", x, x);
         mosi = 0;
         sck = 0;
         slaveout = 8'bx;
         // send data MSB first on the RISING edge of sck
         for (j = 7 ;  j >= 0 ;  j = j - 1)
            begin
               mosi = x[j];
               halfsclk; sck = !sck; // rising edge
               slaveout[j] = miso;
               halfsclk; sck = !sck; // falling edge
            end
      //$display("MOSI %b %h", x, x);
      $display("MISO(RPI) %t %b %h", $time, slaveout, slaveout);
      end
   endtask
   
   integer f;

   initial begin
   rst = 1; #3000; rst = 0; #3000;

   ssel_enable; spi_write(8'b0000_0000); fullsclk; spi_write(8'b0000_0000); ssel_disable; // READ STATUS
   ssel_enable; spi_write(8'b1000_0001); fullsclk; spi_write(8'b0000_0001); ssel_disable; // START AQUSITION
   #1_200_000 // wait for a while 60ns *20k = 1200k 

// for (f=0; f < 20; f = f + 1)
//    begin
//         ssel_enable;
//         $display("3BYTE READ %t", $time);
//         spi_write(8'b0000_0010); fullsclk;
//         spi_write(8'b0000_0000); fullsclk; 		//MSB
//         spi_write(8'b0000_0000); fullsclk;
//         spi_write(8'b0000_0000); ssel_disable; //LSB
//         #200;
//    end

   for (f=0; f < 250; f = f + 1)
      begin
//         $display("MSB READ %t ps", $time);
//         ssel_enable; spi_write(8'b0_00000_10); fullsclk; spi_write(8'b0); ssel_disable; #200;//MSB
//         $display("  B READ %t", $time);
//         ssel_enable; spi_write(8'b0_00000_11); fullsclk; spi_write(8'b0); ssel_disable; #200;//  B
//         $display("LSB READ %t", $time);
         ssel_enable; spi_write(8'b0_00001_00); fullsclk; spi_write(8'b0); ssel_disable; #200;//LSB
      end

//   for (f=0; f < 1<<BLOCK_BUFFER_ADDR_WIDTH; f = f + 1)
//      begin
//        $display("block_buffer: %d %d %b", f,tartwithspi.tart_block_buffer.block_buffer[f],tartwithspi.tart_block_buffer.block_buffer[f]);
//      end
   #120 $finish();
   end
endmodule