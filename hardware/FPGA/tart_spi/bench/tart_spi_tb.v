`timescale 1ns/1ps
module tart_spi_tb;

   reg [23:0] data [0:15];
   reg [3:0]  data_index = 0;  
   reg [7:0]  stat;
   reg        clk = 0, rst = 0, ready = 0;
   
   reg        sck = 1'b0;
   wire       ssel, mosi, miso;

   wire       request, spi_reset, spi_start, spi_debug;
   wire [2:0] sdelay;

   reg [7:0]  data_tx;
   wire [7:0] data_rx;

   reg        cyc = 0, stb = 0;
   wire       rdy, ack;

   reg [7:0] aq_count = 0;
   reg       aq_start = 0, aq_done = 1;


   //  Clocks:
   always #5  clk <= ~clk;
//    always #6  sck <= ~sck;
   always #10 sck <= ~sck;


   integer   i;
   initial begin : SPI_SLAVE_TB
      $dumpfile ("tart_tb.vcd");
      $dumpvars;

      for (i = 0; i < 16; i = i+1)
        data[i] <= $random;
      stat <= $random;
      
      #20 rst <= 1;
      #30 rst <= 0;

      //----------------------------------------------------------------------
      $display("\n%10t: Issue a reset command to the slave device:", $time);
      #30  cyc  <= 1; stb <= 1;
      data_tx <= {1'b1, 3'b000, 4'b1111};
      while (!ack) #10 ;
      
      #10 data_tx <= 8'b00000001;
      while (!ack) #10 ;
      #10 stb <= 0;
      while (!rdy) #10 ;
      #10 while (!rdy) #10 ;
      cyc <= 0; #60 ;

      //----------------------------------------------------------------------
      $display("\n%10t: Set the aquisition mode:", $time);
      #30  cyc  <= 1; stb <= 1;
      data_tx <= {1'b1, 3'b000, 4'b0001};
      while (!ack) #10 ;
      
      #10 data_tx <= 8'b00000001;
      while (!ack) #10 ;
      #10 stb <= 0;
      while (!rdy) #10 ;
      #10 while (!rdy) #10 ;
      cyc <= 0; #60 ;

      //----------------------------------------------------------------------
      $display("\n%10t: Read back the aquisition mode:", $time);
      #30  cyc  <= 1; stb <= 1;
      data_tx <= {1'b0, 3'b000, 4'b0001};
      while (!ack) #10 ;
      
      #10 data_tx <= $random;
      while (!ack) #10 ;
      #10 data_tx <= $random;
      while (!ack) #10 ;
      #10 stb <= 0;
      while (!rdy) #10 ;
      #10 while (!rdy) #10 ;
      #10 while (!rdy) #10 ;
      cyc <= 0; #60 ;

      #20 ready <= 1;

      //----------------------------------------------------------------------
      //  Aquire some data, wait and then get some more.
      $display("\n%10t: Now get some data:", $time);
      #20 aq_count <= 6; aq_start <= 1;
      #10 while (!aq_done) #10 ;
      #200 ;

      //----------------------------------------------------------------------
      $display("\n%10t: Second data transfer:", $time);
      #20 aq_count <= 6; aq_start <= 1;
      #10 while (!aq_done) #10 ;
      
      //----------------------------------------------------------------------
      #200 $finish;
   end // SPI_SLAVE_TB

   initial begin : SPI_FAILED
      #12000 $display ("TIMEOUT!");
      $finish;
   end // SPI_FAILED

   //  Monitor the incoming data:
   always @(posedge clk)
     if (rdy) begin
        $display ("%10t: SPI data = %08b (0x%02x)", $time, data_rx, data_rx);
     end

   always @(posedge clk)
     if (spi_reset) begin
        data_index <= 0;
        ready <= 0;
     end
     else if (request && !ready) begin
        data_index <= data_index + 1;
        stat  <= $random;
        ready <= 1;
     end
     else
       ready <= 0;

   // Read back aquired data.
   always @(posedge clk)
     begin
        if (aq_start) begin
           cyc <= 1;
           stb <= 1;
           data_tx  <= {1'b0, 3'b000, 4'b0010};
           aq_start <= ~ack;
           if (ack)
             data_tx <= $random;
           aq_done  <= 0;
           if (aq_done) aq_count <= aq_count + 2;
        end
        else if (aq_count != 0 && cyc && stb && ack) begin
           stb <= 0;
           aq_count <= aq_count - 1;
        end
        else if (aq_count != 0 && cyc && rdy) begin
           stb <= 1;
           data_tx <= $random;
        end
        else if (aq_count == 0 && rdy && !aq_done) begin
           cyc <= 0;
           aq_done <= 1;
        end
     end


/*
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
 */
   
   spi_master SPI_MASTER0
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_i(cyc),
       .stb_i(stb),
       .ack_o(ack),
       .rdy_o(rdy),
       .dat_i(data_tx),
       .dat_o(data_rx),
       
       .SCK(sck),
       .SCK_enable(sck_en),
       .SSEL(ssel),
       .MOSI(mosi),
       .MISO(miso)
       );

   wire [23:0] data_w = data[data_index];
   wire        sck_w = sck & sck_en;
   
   tart_spi TART_SPI0
     ( .clk(clk),
       .rst(rst || spi_reset),
       
       .data_ready(ready),
       .data_request(request),
       .data_in(data_w),

       .debug_o(debug_o),
       
       .spi_reset(spi_reset),
       .spi_debug(spi_debug),
       .spi_status(stat),
       .spi_start_aq(spi_start_aq),
       .data_sample_delay(sdelay),
       
       .SCK (sck_w),
       .SSEL(ssel),
       .MOSI(mosi),
       .MISO(miso)
       );

endmodule // tart_spi_tb

