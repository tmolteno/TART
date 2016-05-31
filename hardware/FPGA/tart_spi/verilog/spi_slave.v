`timescale 1ns/1ps
//----------------------------------------------------------------------------
//  
//  Low-level SPI stuff.
//  
//  by Tim Molteno. tim@elec.ac.nz
//   & Max Scheel   max@max.ac.nz
//  
//----------------------------------------------------------------------------
module spi_slave
  (
   input            SCK,
   input            MOSI,
   output           MISO,
   input            SSEL,

   input [7:0]      spi_status,

   // Wishbone-like interface.
   // NOTE: Multiplexed address and data.
   input            clk_i,
   input            rst_i,      // TODO: this is currently ignored
   output           cyc_o,
   output           stb_o,
   input            ack_i,
   input [7:0]      dat_i,
   output reg [7:0] dat_o
   );


   // we handle SPI in 8-bits format, so we need a 3 bits counter to count the bits as they come in
   reg [2:0]        bitcnt;
   reg              byte_received    = 1'b0;  // high when a byte has been received
   reg [7:0]        data_tx_buffer;


   assign MISO  = data_tx_buffer[7]; // send MSB first
   assign cyc_o = SSEL_active;  // TODO: proper framing of a WB cycle
   assign stb_o = byte_received;

   
   // Handle receiving data
   always @(posedge clk_i)
     begin
        if (~SSEL_active) bitcnt <= 3'b000;
        else
          begin
             if (SCK_risingedge)
               begin
                  // implement a shift-left register (since we receive the data MSB first)
                  dat_o  <= {dat_o[6:0], MOSI_data};
                  bitcnt <= bitcnt + 3'b001;
               end
          end
     end

   always @(posedge clk_i)
     byte_received <= SSEL_active && SCK_risingedge && (bitcnt == 3'b111);

   // Handle sending data
   always @(posedge clk_i)
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
                  if (ack_i)
                    begin
                       $display("MISO: %b , %h", dat_i, dat_i);
                       data_tx_buffer <= dat_i;
                    end
                  else if (SCK_fallingedge && bitcnt > 3'b0)
                    data_tx_buffer <= {data_tx_buffer[6:0], 1'bx};
               end
          end
     end
   
   // Samples the incoming SPI signals synchronously.
   spi_clock_synchronizer sync
     ( .fpga_clk(clk_i),

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

   
endmodule // spi_slave
