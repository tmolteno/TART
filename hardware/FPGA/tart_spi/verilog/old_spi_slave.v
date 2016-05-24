`timescale 1ns/1ps
/*************************** TART OPERATION REGISTERS ***********************
   by Tim Molteno. tim@elec.ac.nz
   & Max Scheel   max@max.ac.nz
   
   The tart operation is controlled by these 8-bit registers.

   Transfers over the SPI are (usually) 2x8-bit, using the following format

   SSEL_LOW {r/w, reg[6:0]}{ data[7:0]} SSEL_HIGH 
   The MSB indicates whether the register is being written or read.

ANTENNA_DATA
   Read-only register that contains a single byte of antenna data

   Register that contains the address of the current data in the antenna buffer
   gets filled with new data after register[ADDR_READ_DATA3] is dumped in the output buffer.

   TYPICAL OPERATION
   1. The SPI master writes to the acquire register (The initiates a capture)
   2. The FPGA starts acquiring data until its full.
   3. Fills the output FIFO and presents the first 24 bit of antenna data in the three 8bit registers.
   4. The SPI master reads the data from the antenna data register.
    4.a) register address gets auto-incremented with each byte read.
    4.b) on ADDR_READ_DATA3 the next address is set to ADDR_READ_DATA1-1 such that the next read will yield ADDR_READ_DATA1.

   EXAMPLE A) 2 byte operation

                   RPI         <-->         SLAVE

   START ACQUISTION
   1_000_0001 , 0000_0001      <-->       status_data[7:0] 0000_0000
   w ??? to register[0001] 0000_0001  --> TELESCOPE starts acquisition

   READ DATA REGISTER
   r ??? reg[...] 
   0_000_0010 , 0000_0000      <-->       status_data[7:0] , DATA[23:16]
   0_000_0011 , 0000_0000      <-->       status_data[7:0] , DATA[15:8]
   0_000_0100 , 0000_0000      <-->       status_data[7:0] , DATA[7:0]
   
   EXAMPLE B) 4 byte operation
   0_000_0010 , 0000_0000, 0000_0000, 0000_0000       <-->  status_data[7:0] , DATA[23:16], DATA[15:8], DATA[7:0]
   
   EXAMPLE C) 4(+) byte operation
   0_000_0010 , 0000_0000, 0000_0000, 0000_0000, ...  <-->  status_data[7:0] , DATA[23:16], DATA[15:8], DATA[7:0] , DATA[23:16], ...

*/

module spi_slave
  (
   input             fpga_clk,
   input             SCK,
   input             MOSI,
   output            MISO,
   input             SSEL,

   input [23:0]      antenna_data,
   input [7:0]       spi_status,

   output reg        spi_buffer_read_complete = 0,
	 output wire [2:0] data_sample_delay,
   output wire       spi_reset,
   output wire       spi_start_aq,
   output wire       spi_debug
   );


   //-------------------------------------------------------------------------
   //  Low-level SPI stuff.
   //-------------------------------------------------------------------------
   
   // we handle SPI in 8-bits format, so we need a 3 bits counter to count the bits as they come in
   reg [2:0] bitcnt;

   reg [7:0] data_rx_buffer   = 8'bx;
   reg       byte_received    = 1'b0;  // high when a byte has been received
   reg       trigger_data     = 1'b0; 
   reg       trigger_spi      = 1'b0;
   reg       spi_start_aq_int = 1'b0;
   reg [7:0] data_tx_buffer;
   reg [7:0] data_to_send     = 8'bx;

   assign MISO = data_tx_buffer[7];  // send MSB first
   
   // Handle receiving data
   always @(posedge fpga_clk)
     begin
        if (~SSEL_active) bitcnt <= 3'b000;
        else
          begin
             if (SCK_risingedge)
               begin
                  // implement a shift-left register (since we receive the data MSB first)
                  data_rx_buffer <= {data_rx_buffer[6:0], MOSI_data};
                  bitcnt <= bitcnt + 3'b001;
               end
          end
     end

   wire received_w = SSEL_active && SCK_risingedge && (bitcnt == 3'b111);
   always @(posedge fpga_clk)
     begin
        byte_received <= received_w;
        trigger_data  <= byte_received;
        trigger_spi   <= trigger_data;
     end

   always @(posedge fpga_clk)
     // spi_buffer_read_complete <= (trigger_new_data && trigger_spi) ? 1'b1: 1'b0;
     spi_buffer_read_complete <= trigger_new_data && trigger_spi;

   // Handle sending data
   always @(posedge fpga_clk)
     begin
        if (SSEL_active)
          begin
             if (SSEL_startmessage)
               begin
                  // first byte sent in a message is the status register
                  $display("MISO: %b , %h", spi_status, spi_status);
                  data_tx_buffer <= spi_status;
               end
             else
               begin 
                  if (trigger_spi)
                    begin
                       $display("MISO: %b , %h", data_to_send, data_to_send);
                       data_tx_buffer <= data_to_send;
                    end
                  else if (SCK_fallingedge && bitcnt > 3'b0)
                    data_tx_buffer <= {data_tx_buffer[6:0], 1'bx};
               end
          end
     end
   
   // Samples the incoming SPI signals synchronously.
   spi_clock_synchronizer sync
     ( .fpga_clk(fpga_clk),
       .SCK(SCK),
       .MOSI(MOSI),
       .SSEL(SSEL),
       .SCK_risingedge(SCK_risingedge),
       .SCK_fallingedge(SCK_fallingedge),
       .SSEL_active(SSEL_active),
       .SSEL_startmessage(SSEL_startmessage),
       .SSEL_endmessage(SSEL_endmessage),
       .MOSI_data(MOSI_data)
       );


   //-------------------------------------------------------------------------
   //  
   //  TART-specific control registers.
   //  NOTE: Uses (from above) the input signals:
   //      { byte_received, data_rx_buffer, trigger_data }
   //    and the output signals:
   //      { data_to_send }
   //  
   //-------------------------------------------------------------------------

   // SPECIFIY POSSIBLE REGISTER ADDRESSES
   //             Register                        Function
   parameter      ADDR_STATUS       = 4'b0000; // STATUS ADDR
   parameter      ADDR_STARTAQ      = 4'b0001; // START SAMPLING
   parameter      ADDR_READ_DATA1   = 4'b0010; // DATA MSB [23:16]
   parameter      ADDR_READ_DATA2   = 4'b0011; // DATA     [15:8]
   parameter      ADDR_READ_DATA3   = 4'b0100; // DATA LSB [7:0]
 	 parameter      ADDR_SAMPLE_DELAY = 4'b1100; //
   parameter      ADDR_DEBUG        = 4'b1000; // DEBUG MODE 
   parameter      ADDR_RESET        = 4'b1111; // RESET

   // TART SPI control registers.
   reg [7:0] register [15:0];

   reg [3:0] register_addr    = 4'b0;
   reg       write_flag       = 1'b0;
   reg       trigger_new_data = 1'b0;
   reg       start_byte       = 1'b1;

   assign data_sample_delay = register[ADDR_SAMPLE_DELAY][2:0];
   assign spi_reset         = register[ADDR_RESET]   == 8'b1;
   assign spi_start_aq      = register[ADDR_STARTAQ] == 8'b1;
   assign spi_debug         = register[ADDR_DEBUG]   == 8'b1;

   // Initialize all RAM cells to 0 at startup
   integer i;
   initial for (i=0; i <16; i = i + 1) register[i] = 8'b0;

   // Pat @12/05/2016: Flag whether the incoming byte is the command-byte?
   always @(posedge fpga_clk)
     if (byte_received) start_byte <= ~start_byte;

/*
   // Pat @12/05/2016: This version is bugged, and clears `start_byte` to
   // early? I couldn't activate `spi_reset` with this version?
   always @(posedge fpga_clk)
     begin
        if (SSEL_endmessage) start_byte <= 1;    // zero the counter
        else if (byte_received) start_byte <= 0; // count the bytes
     end
*/
   
   always @(posedge fpga_clk)
     if (spi_reset)
       begin
		      register[ADDR_SAMPLE_DELAY] <= 8'b0;
          register[ADDR_RESET]        <= 8'b0;
          register[ADDR_STARTAQ]      <= 8'b0;
          register[ADDR_DEBUG]        <= 8'b0;
       end
     else
       begin
          register[ADDR_READ_DATA1] <= antenna_data[23:16];
          register[ADDR_READ_DATA2] <= antenna_data[15:8];
          register[ADDR_READ_DATA3] <= antenna_data[7:0];
          register[ADDR_STATUS]     <= spi_status;
          //if (register[ADDR_STARTAQ] == 8'b1) spi_start_aq_int <= 1'b1;
          if (byte_received)
            begin
               $display("MOSI: %b", data_rx_buffer);
               if (start_byte) /* COMMAND WORD */
                 begin
                    write_flag    <= data_rx_buffer[7];
                    register_addr <= data_rx_buffer[3:0];
                    //if (data_rx_buffer[3:0] == ADDR_STARTAQ) spi_start_aq <= 1'b1;
                 end
               else if (start_byte == 0) /* DATA BYTE */
                 begin
                    if (write_flag == 1)
                      begin
                         register[register_addr] <= data_rx_buffer[7:0];
                      end
                    else /* write_flag == 0 */
                      begin
                         register_addr <= register_addr + 1'b1;
                      end
                 end
            end
          else if (trigger_data)
            begin
               data_to_send <= register[register_addr];
               if (register_addr == ADDR_READ_DATA3)
                 begin
                    trigger_new_data <= 1'b1;
                    register_addr <= ADDR_READ_DATA1-1'b1;
                 end
               else trigger_new_data <= 1'b0;
            end
       end

   
endmodule // spi_slave
