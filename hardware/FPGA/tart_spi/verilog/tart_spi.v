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
   

 * TODO:
 *  + Pat @17/05/2016 -- need a board-level reset handler?
 * 
 */

// TART SPI-mapped IO states.
`define TART_IDLE  0
`define TART_START 1
`define TART_READ  2
`define TART_SEND  4
`define TART_WRITE 8

// Data prefetcher states.
`define PRE_EMPTY 0
`define PRE_WORD1 1
`define PRE_WORD2 2

module tart_spi
  (
   // System (bus) clock
   input            clk,
   input            rst,

   // Streaming data interface
   // NOTE: Doesn't need to initiate transfers, but data is valid whenever
   //   `data_ready` is asserted.
   input            data_ready,
   output reg       data_request = 0,
   input [23:0]     data_in,

   output reg       debug_o = 0,
//    output           debug_o,

   // SPI-transmitter status flags
   output reg       spi_reset = 0,
   output reg       spi_debug = 0,
   input [7:0]      spi_status,
   output reg       spi_start_aq = 0,
	 output reg [2:0] data_sample_delay = 0,

   // SPI pins
   input            SCK,
   input            SSEL,
   input            MOSI,
   output           MISO
   );

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

   // TART WB <-> SPI interface signals.
   wire [7:0]       data_from_spi;
   reg [7:0]        data_to_send;
   wire             cyc, stb, we;
   reg              ack = 0;
   wire             oflow, uflow, debug_w;

//    assign debug_o = oflow | uflow;
//    assign debug_o = debug_w;

   always @(posedge clk)
     if (rst)
       debug_o <= 0;
     else if (byte_arrival && register_addr == ADDR_RESET)
       debug_o <= 1;
     else
       debug_o <= debug_o;


   //-------------------------------------------------------------------------
   //  
   //  Bus interface for the Wishbone-like bus.
   //  
   //-------------------------------------------------------------------------
   wire             byte_request = cyc && stb && !we;
   wire             byte_arrival = cyc && stb &&  we;

   // TODO: Properly compute the number of bytes.
   always @(posedge clk)
     if (rst)
       ack <= 0;
     else
       ack <= !ack && (byte_arrival || byte_request);


   //-------------------------------------------------------------------------
   //
   //  TART SPI-mapped register-bank.
   //  
   //-------------------------------------------------------------------------
   wire             wrap_address = register_addr == ADDR_READ_DATA3;
   reg [3:0]        register_addr = 4'b0;
   wire             write_flag = data_from_spi[7];
   reg [3:0]        tart_state = `TART_IDLE;

   // TART register-mapping state machine.
   always @(posedge clk)
     if (rst)
       tart_state <= `TART_IDLE;
     else
       case (tart_state)
         `TART_IDLE:  tart_state <= cyc ? `TART_START : tart_state ;
         `TART_START:
           if (byte_arrival && ack)
             tart_state <= write_flag ? `TART_WRITE : `TART_READ ;
         `TART_READ:  tart_state <= !cyc ? `TART_IDLE : tart_state ;
//          `TART_READ:  tart_state <= byte_arrival && ack ? `TART_SEND : tart_state ;
//          `TART_SEND:  tart_state <= tart_state ;
         `TART_WRITE: tart_state <= !cyc ? `TART_IDLE : tart_state ;
       endcase // case (tart_state)

   // Address latching and wrapping logic, for register reads and streaming
   // captured data.
   always @(posedge clk)
     if (rst)
       register_addr <= 0;
     else if (byte_arrival && ack && tart_state == `TART_START)
       register_addr <= data_from_spi[3:0];
//      else if (byte_request && ack && tart_state == `TART_SEND)
     else if (byte_arrival && ack && tart_state == `TART_READ)
       begin
          if (wrap_address)
            register_addr <= ADDR_READ_DATA1;
          else
            register_addr <= register_addr + 1;
       end
     else
       register_addr <= register_addr;

   // TART reset logic.
   reg clr_reset = 0;
   always @(posedge clk)
     if (spi_reset && SSEL)
       clr_reset <= 1;
     else if (!spi_reset)
       clr_reset <= 0;

   always @(posedge clk) //  or posedge clr_reset)
     if (clr_reset)
       spi_reset <= 0;
     else if (byte_arrival && tart_state == `TART_WRITE && register_addr == ADDR_RESET)
       spi_reset <= data_from_spi[0];
   
   // TART register reads.
   always @(posedge clk)
     if (byte_request && !ack)
       begin
          if (tart_state == `TART_READ || tart_state == `TART_SEND)
            case (register_addr)
              ADDR_STATUS:       data_to_send <= spi_status;
              ADDR_STARTAQ:      data_to_send <= spi_start_aq;
              ADDR_READ_DATA1:   data_to_send <= antenna_data[23:16];
              ADDR_READ_DATA2:   data_to_send <= antenna_data[15:8];
              ADDR_READ_DATA3:   data_to_send <= antenna_data[7:0];
              ADDR_SAMPLE_DELAY: data_to_send <= data_sample_delay;
              ADDR_DEBUG:        data_to_send <= spi_debug;
              ADDR_RESET:        data_to_send <= spi_reset;
              default:           data_to_send <= 8'bx;
            endcase // case (register_addr)
          else
            data_to_send <= spi_status;
       end

   // TART register writes.
   always @(posedge clk)
     if (rst)
       begin
          spi_debug         <= 0;
          data_sample_delay <= 0;
          spi_start_aq      <= 0;
       end
     else if (byte_arrival && !ack && tart_state == `TART_WRITE)
       case (register_addr)
         ADDR_STARTAQ:      spi_start_aq      <= data_from_spi[0];
         ADDR_SAMPLE_DELAY: data_sample_delay <= data_from_spi[2:0];
         ADDR_DEBUG:        spi_debug         <= data_from_spi[0];
       endcase // case (register_addr)
   

   //-------------------------------------------------------------------------
   //  
   //  Data prefetch (from SDRAM) logic.
   //  
   //-------------------------------------------------------------------------
   reg [23:0] prefetch_data = 24'b0;
   reg [23:0] antenna_data  = 24'b0;
   reg [1:0]  pre_state = `PRE_EMPTY;
   reg        data_sent = 0;

   // Data-prefetch state machine.
   // NOTE: There should never be more than two words being stored or fetched.
   always @(posedge clk)
     if (rst)
       pre_state <= `PRE_EMPTY;
     else
       case (pre_state)
         `PRE_EMPTY: pre_state <= data_ready ? `PRE_WORD1 : pre_state ;
         `PRE_WORD1: pre_state <= data_ready && !data_sent ? `PRE_WORD2 :
                                  !data_ready && data_sent ? `PRE_EMPTY : pre_state ;
         `PRE_WORD2: pre_state <= data_sent  ? `PRE_WORD1 : pre_state ;
       endcase // case (pre_state)

   always @(posedge clk)
     if (data_ready) begin
       if (!data_sent && pre_state == `PRE_EMPTY)
         antenna_data <= data_in;
       else if (data_sent && pre_state == `PRE_WORD1)
         antenna_data <= data_in;
       else if (!data_sent && pre_state == `PRE_WORD1)
         antenna_data <= prefetch_data;
       else
         $error ("%5t: data arrived while in an incompatible state.", $time);
     end
     else if (data_sent && pre_state == `PRE_WORD2)
       antenna_data <= prefetch_data;

   always @(posedge clk)
     if (data_ready)
       prefetch_data <= data_in;

   always @(posedge clk)
     if (rst)
       data_sent <= 0;
     else
       data_sent <= tart_state == `TART_READ && wrap_address && byte_arrival && ack;

   always @(posedge clk)
     if (rst || data_request)
       data_request <= 0;
     else if (pre_state == `PRE_EMPTY && data_ready)
       data_request <= 1;
     else if (pre_state == `PRE_WORD2 && data_sent)
       data_request <= 1;
   

   //-------------------------------------------------------------------------
   //  
   //  SPI slave module, with a Wishbone-like interconnect.
   //  
   //-------------------------------------------------------------------------
   spi_target SPI_TARGET0
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_o(cyc),
       .stb_o(stb),
       .we_o (we),
       .ack_i(ack),
       .dat_i(data_to_send),
       .dat_o(data_from_spi),

       .overflow_o (oflow),
       .underflow_o(uflow),
       .debug_o    (debug_w),
       
       .SCK (SCK),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );

   
endmodule // tart_spi
