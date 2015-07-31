`timescale 1ns/1ps

//  _____      _      ____    _____
// |_   _|    / \    |  _ \  |_   _|
//   | |     / _ \   | |_) |   | |
//   | |    / ___ \  |  _ <    | |
//   |_|   /_/   \_\ |_| \_\   |_|

module tart(
				// SHOW DELAYED CLOCK:
				output wire rx_clk_test_pin,
            // PAPILIO
            output wire led,
            // SDRAM
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
            // SPI
            output wire spi_miso,
            input spi_sck, input spi_mosi, input spi_ssel,
            // TELESCOPE
            input rx_clk_16,     // 16.368 MHz receiver master clock
            input [23:0] antenna // Radio Data Interface
           );

   parameter SDRAM_ADDRESS_WIDTH = 22;
   parameter SDRAM_COLUMN_BITS   = 8;
   parameter SDRAM_STARTUP_CYCLES= 10100;
   parameter CYCLES_PER_REFRESH  = 1524;

   //     HOOK UP SPI RESET INTO RESET

   wire reset;
   wire spi_reset;
   assign reset = spi_reset;// | rst;

   //     GENERATE DIFFERENT CLOCK DOMAINS

   //fake_tart_clk clknetwork(.CLK_IN1(fpga_clk_32), .CLK_OUT1(fpga_clk), .CLK_OUT2(fake_rx_clk), .CLK_OUT3(clk320));
   tart_clk_generator clknetwork(
      .CLKIN(rx_clk_16),               // 16.368 MHZ
      .CLKOUT0(rx_clk_16_buffered),    // 16.368 MHZ buffered
      .CLKOUT1(fpga_clk)               // 16.368x6 = 98.208 MHz
   );
   
   //     HOOK UP IO REGISTER TO INTERNAL LOGIC
   reg [23:0] real_antenna;
   always @(posedge fpga_clk) real_antenna <= antenna;
   
   //     GENERATE FAKE DATA (24 BIT COUNTER) FOR DEBUGGING

   wire [23:0] fake_antenna;
   //fake_telescope fake_tart (.write_clk(fake_rx_clk), .write_data(fake_antenna));
   fake_telescope fake_tart (.write_clk(rx_clk_16_buffered), .write_data(fake_antenna));

   //     TRI STATE FOR CHOOSING REAL DATA OR FAKE DATA
   wire spi_debug;
   wire sel_rx_clk;          assign       sel_rx_clk = rx_clk_16_buffered;
   //wire rx_clk;            assign       rx_clk = (spi_debug) ? fake_rx_clk  : rx_clk_16;
   wire [23:0] sel_antenna_data; assign sel_antenna_data = (spi_debug) ? fake_antenna : real_antenna;

   wire [23:0] antenna_data;
   wire rx_clk;
   //sync_antennas_to_clock sync_ant_int(
   // .fast_clk(fpga_clk),
   // .data_in(sel_antenna_data),
   // .slow_clk(rx_clk),
   // .data_out(antenna_data)  // data valid on the rising edge of the clock.
   // );
   wire [2:0] data_sample_delay;

	 delay_data_sampling_clk delay_rx_clk(
	   .fast_clk(fpga_clk),
		.data_sample_delay(data_sample_delay),
		.slow_clk(rx_clk)
	);
	assign rx_clk_test_pin = rx_clk;
	
   // assign rx_clk = sel_rx_clk;
   
	assign antenna_data = sel_antenna_data;

   //     AQUISITION BLOCK

   wire [23:0] aq_write_data;
   wire [23:0] aq_read_data;
   wire [8:0] aq_bb_rd_address;
   wire [8:0] aq_bb_wr_address;

   block_buffer
   aq_bb(
      .read_data(aq_read_data),
      .write_data(antenna_data),
      .clk(fpga_clk),
      .write_address(aq_bb_wr_address),
      .read_address(aq_bb_rd_address)
   );

   //      STORAGE BLOCK

   wire [SDRAM_ADDRESS_WIDTH-2:0] cmd_address;
   wire [2:0] tart_state;
   assign led = (tart_state>=2);

   wire [31:0] cmd_data_in;
   wire [31:0] data_out;

   wire spi_buffer_read_complete;
  
   fifo_sdram_fifo_scheduler
   #(.SDRAM_ADDRESS_WIDTH(SDRAM_ADDRESS_WIDTH))
   scheduler(
            .rst(reset),
            .spi_start_aq(spi_start_aq),
            .aq_bb_wr_address(aq_bb_wr_address),
            .aq_bb_rd_address(aq_bb_rd_address),
            .aq_read_data(aq_read_data),
            .cmd_data_in(cmd_data_in),
            .write_clk(rx_clk),
            .bb_clk(fpga_clk),
            .cmd_ready(cmd_ready),
            .cmd_enable(cmd_enable),
            .cmd_wr(cmd_wr),
            .cmd_address(cmd_address),
            .tart_state(tart_state),
            .spi_buffer_read_complete(spi_buffer_read_complete)
   );

   SDRAM_Controller_v
   #(
      .sdram_address_width(SDRAM_ADDRESS_WIDTH),
      .sdram_column_bits(SDRAM_COLUMN_BITS),
      .sdram_startup_cycles(SDRAM_STARTUP_CYCLES),
      .cycles_per_refresh(CYCLES_PER_REFRESH)
   )
   hamster_sdram(
      .clk(fpga_clk),
      .reset(reset),
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

   //     TRANSMISSION BLOCK

   reg [23:0] tx_read_data = 24'b0;
   always @(posedge fpga_clk) if (data_out_ready) tx_read_data <= data_out[23:0];

   //     SPI SLAVE

   SPI_slave dut (.fpga_clk(fpga_clk),
                  .SCK(spi_sck), .MOSI(spi_mosi), .MISO(spi_miso), .SSEL(spi_ssel),
                  .antenna_data(tx_read_data),
                  .spi_status({2'b10,spi_buffer_read_complete, spi_start_aq, spi_debug, tart_state[2:0]}),
                  .spi_buffer_read_complete(spi_buffer_read_complete),
						.data_sample_delay(data_sample_delay),
                  .spi_reset(spi_reset),
                  .spi_start_aq(spi_start_aq),
                  .spi_debug(spi_debug)
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
//         spi_write(8'b0000_0000); fullsclk; //MSB
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

   #120 $finish();
   end
endmodule