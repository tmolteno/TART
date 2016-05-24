`timescale 1ns/1ps
module old_spi_slave_tb;

//  SPI master states:
`define SPI_IDLE 4'b0000
`define SPI_BUSY 4'b0001
`define SPI_DONE 4'b1000

   reg [23:0] data;
   reg [7:0]  stat;
   reg        clk = 0;
   
   reg        sck = 1'b0;
   wire       mosi, miso, ssel;

   wire       spi_done, spi_reset, spi_start, spi_debug;
   wire [2:0] sdelay;

   reg        send = 0, done = 0;
   wire       done_w;
   reg [7:0]  data_tx, test_data, fpga_data;
   wire [7:0] data_rx;
   reg [3:0]  spi_state = `SPI_IDLE;
   wire       sending   = spi_state != `SPI_IDLE;
   wire       beginning = send && (spi_state == `SPI_IDLE || spi_state == `SPI_DONE);

   
   assign mosi    = data_tx[7];
   assign ssel    = spi_state == `SPI_IDLE;

   assign done_w  = spi_state == `SPI_DONE;
   assign data_rx = data_tx;


   initial begin : SPI_SLAVE_TB
      $dumpfile ("spi_tb.vcd");
      $dumpvars;

      data <= $random;
      stat <= $random;

      // Issue a reset command to the slave device:
      #30  send <= 1;
      test_data <= {1'b1, 3'b000, 4'b1111};
      #120 send <= 0;
      while (!done) #120 ;
      
      send      <= 1;
      test_data <= 8'b00000001;
      #120 send <= 0;
      while (!done) #120 ;
      
      // Set to data-aquisition mode:
      send      <= 1;
      test_data <= {1'b1, 3'b000, 4'b0001};
      #120 send <= 0;
      while (!done) #120 ;

      send      <= 1;
      test_data <= 8'b00000001;
      #120 send <= 0;
      while (!done) #120 ;
      
      // Now get some data:
      send      <= 1;
      test_data <= {1'b0, 3'b000, 4'b0010};
      #120 send <= 0;
      while (!done) #120 ;
      
      send      <= 1;
      test_data <= $random;
      #120 send <= 0;
      while (!done) #120 ;
      
      send      <= 1;
      test_data <= {1'b0, 3'b000, 4'b0100};
      #120 send <= 0;
      while (!done) #120 ;
      
      send      <= 1;
      test_data <= $random;
      #120 send <= 0;
      while (!done) #120 ;

      #120 $finish;
   end // SPI_SLAVE_TB


   //  Clocks:
   always #5  clk <= ~clk;
   always #60 sck <= ~sck;

   //  Monitor the incoming data:
   always @(posedge sck)
     if (done) begin
        $display ("%5t: SPI data = %08b", $time, data_rx);
        fpga_data <= data_rx;
     end


   //-------------------------------------------------------------------------
   //  
   //  Simple SPI master device.
   //  
   always @(posedge sck)
     case (spi_state)
       `SPI_IDLE, `SPI_DONE:
         spi_state <= send ? `SPI_BUSY : `SPI_IDLE ;
       default:
         spi_state <= spi_state + 1;
     endcase // case spi_state

   always @(posedge sck)
     if (beginning)
       data_tx <= test_data;
     else if (sending)
       data_tx <= {data_tx[6:0], miso};
     else
       data_tx <= data_tx;

   always @(posedge sck)
     done <= done_w;


   old_spi_slave SPI0
     ( .fpga_clk(clk),
       
       .SCK(sck),
       .MOSI(mosi),
       .MISO(miso),
       .SSEL(ssel),
       
       .antenna_data(data),
       .spi_status(stat),
       
       .spi_buffer_read_complete(spi_done),
       .data_sample_delay(sdelay),
       .spi_reset(spi_reset),
       .spi_start_aq(spi_start),
       .spi_debug(spi_debug)
       );
   

endmodule // old_spi_slave_tb
