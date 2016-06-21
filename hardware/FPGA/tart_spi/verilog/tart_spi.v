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
`define TART_BUS   4

`define __WB_CLASSIC

module tart_spi
  #( parameter ADDRESS = 12,
     parameter ASB     = ADDRESS-1,
     parameter DELAY   = 3,

     // SPECIFIY POSSIBLE REGISTER ADDRESSES
     //             Register                        Function
     parameter ADDR_STATUS       = 4'b0000, // STATUS ADDR

     // Data aquisition control and read-back registers:
     parameter ADDR_STARTAQ      = 4'b0001, // START SAMPLING
     parameter ADDR_READ_DATA1   = 4'b0010, // DATA MSB [23:16]
     parameter ADDR_READ_DATA2   = 4'b0011, // DATA     [15:8]
     parameter ADDR_READ_DATA3   = 4'b0100, // DATA LSB [7:0]

     // Visibilities/correlators specific registers:
     parameter ADDR_VIS_STATUS   = 4'b1000,
     parameter ADDR_VIS_COUNT    = 4'b1001, // log2(#samples/window)
     parameter ADDR_VIS_DATA     = 4'b1010, // 1 byte of vis./read

     // Additional system registers:
 	   parameter ADDR_SAMPLE_DELAY = 4'b1100, //
     parameter ADDR_DEBUG        = 4'b1000, // DEBUG MODE 
     parameter ADDR_RESET        = 4'b1111) // RESET
   ( // System (bus) clock
     input              clk,
     input              rst,

     // Streaming data interface
     // NOTE: Doesn't need to initiate transfers, but data is valid whenever
     //   `data_ready` is asserted.
     input              data_ready,
     output             data_request,
     input [23:0]       data_in,

     // Indicate whether an overflow or underrun has occurred.
     output reg         debug_o = 0,

     // SPI-transmitter status flags
     output reg         spi_reset = 0,
     output reg         spi_debug = 0,
     input [7:0]        spi_status,
     output reg         spi_start_aq = 0,
	   output reg [2:0]   data_sample_delay = 0,

     // Wishbone-like (master) bus interface:
     output reg         cyc_o = 0,
     output reg         stb_o = 0,
     output reg         bst_o = 0,
     output reg         we_o = 0,
     input              ack_i,
     output reg [ASB:0] adr_o,
     input [7:0]        dat_i,
     output reg [7:0]   dat_o,

     // SPI pins:
     input              SCK,
     input              SSEL,
     input              MOSI,
     output             MISO
     );

   // TART WB <-> SPI interface signals.
   wire [7:0]           drx;
   reg [7:0]            dtx;
   wire                 cyc, stb, we;
   reg                  ack = 0;
   wire                 oflow, uflow, debug_w;


   //-------------------------------------------------------------------------
   //  
   //  Wishbone-like interconnect to the `spi_target`.
   //  
   //-------------------------------------------------------------------------
   wire                 byte_request = cyc && stb && !we;
   wire                 byte_arrival = cyc && stb &&  we;

   // TODO: Properly compute the number of bytes.
   always @(posedge clk)
     if (rst) ack <= #DELAY 0;
     else     ack <= #DELAY !ack && (byte_arrival || byte_request);

   // Watch for a SPI FIFO overflow, or underrun.
   always @(posedge clk)
     if (rst)                 debug_o <= #DELAY 0;
     else if (oflow || uflow) debug_o <= #DELAY 1;
     else                     debug_o <= #DELAY debug_o;


   //-------------------------------------------------------------------------
   //  
   //  Wishbone-like interconnect to the TART's internal bus.
   //  
   //-------------------------------------------------------------------------
   reg                  bus_cycle = 0;
   wire                 bus_range = tart_addr[3:0] > 7 && tart_addr[3:0] < 11;
   wire                 bus_begin = byte_arrival && ack && tart_state == `TART_START && bus_range;

   always @(posedge clk)
     if (rst)
       {cyc_o, stb_o, we_o} <= #DELAY 3'b000;
     else if (bus_begin)
       {cyc_o, stb_o, we_o} <= #DELAY {2'b11, write_flag};
     else
       {cyc_o, stb_o, we_o} <= #DELAY {cyc_o && ack_i, 1'b0, we_o && !ack_i};

   always @(posedge clk)
     if (bus_begin)
        case (tart_addr[3:0])
          ADDR_VIS_STATUS: adr_o <= #DELAY 12'he00;
          ADDR_VIS_COUNT:  adr_o <= #DELAY 12'he01;
          ADDR_VIS_DATA:   adr_o <= #DELAY 12'h000;
        endcase // case (tart_addr[3:0])


   //-------------------------------------------------------------------------
   //
   //  TART SPI-mapped register-bank.
   //  
   //-------------------------------------------------------------------------
   wire                wrap_addr = tart_addr == ADDR_READ_DATA3;
   wire [3:0]          next_addr = wrap_addr ? ADDR_READ_DATA1 : tart_addr+1;
   reg [3:0]           tart_addr = 4'b0;
   wire                write_flag = drx[7];
   reg [3:0]           tart_state = `TART_IDLE;

   // TART register-mapping state machine.
   always @(posedge clk)
     if (rst)
       tart_state <= #DELAY `TART_IDLE;
     else
       case (tart_state)
         `TART_IDLE:  tart_state <= #DELAY cyc ? `TART_START : tart_state ;
         `TART_START:
           if (byte_arrival) begin
              if (bus_range)
                tart_state <= #DELAY `TART_BUS;
              else if (write_flag)
                tart_state <= #DELAY `TART_WRITE;
              else
                tart_state <= #DELAY `TART_READ;
           end
         `TART_READ:  tart_state <= #DELAY !cyc ? `TART_IDLE : tart_state ;
         //          `TART_READ:  tart_state <= #DELAY byte_arrival && ack ? `TART_SEND : tart_state ;
         //          `TART_SEND:  tart_state <= #DELAY tart_state ;
         `TART_WRITE: tart_state <= #DELAY !cyc ? `TART_IDLE : tart_state ;
         `TART_BUS:   tart_state <= #DELAY cyc_o ? tart_state : `TART_IDLE ;
       endcase // case (tart_state)

   // Address latching and wrapping logic, for register reads and streaming
   // captured data.
   always @(posedge clk)
     if (rst)
       tart_addr <= #DELAY 0;
`ifdef __WB_CLASSIC
     else if (byte_arrival && !ack && tart_state == `TART_START)
       tart_addr <= #DELAY drx[3:0];
     else if (byte_arrival && !ack && tart_state == `TART_READ)
`else
     else if (byte_arrival && tart_state == `TART_START)
       tart_addr <= #DELAY drx[3:0];
     else if (byte_arrival && tart_state == `TART_READ)
`endif //  `ifdef __WB_CLASSIC
       tart_addr <= #DELAY next_addr;
     else
       tart_addr <= #DELAY tart_addr;

   // TART reset logic.
   reg clr_reset = 0;
   always @(posedge clk)
     if (spi_reset && SSEL)
       clr_reset <= #DELAY 1;
     else if (!spi_reset)
       clr_reset <= #DELAY 0;

   always @(posedge clk) //  or posedge clr_reset)
     if (clr_reset)
       spi_reset <= #DELAY 0;
     else if (byte_arrival && tart_state == `TART_WRITE && tart_addr == ADDR_RESET)
       spi_reset <= #DELAY drx[0];
   
   // TART register reads.
   always @(posedge clk)
     if (byte_request && !ack)
       begin
          if (tart_state == `TART_READ || tart_state == `TART_SEND)
            case (tart_addr)
              ADDR_STATUS:       dtx <= #DELAY spi_status;
              ADDR_STARTAQ:      dtx <= #DELAY spi_start_aq;
              ADDR_READ_DATA1:   dtx <= #DELAY antenna_data[23:16];
              ADDR_READ_DATA2:   dtx <= #DELAY antenna_data[15:8];
              ADDR_READ_DATA3:   dtx <= #DELAY antenna_data[7:0];
              ADDR_SAMPLE_DELAY: dtx <= #DELAY data_sample_delay;
              ADDR_DEBUG:        dtx <= #DELAY spi_debug;
              ADDR_RESET:        dtx <= #DELAY spi_reset;
              default:           dtx <= #DELAY 8'bx;
            endcase // case (tart_addr)
          else
            dtx <= #DELAY spi_status;
       end

   // TART register writes.
   always @(posedge clk)
     if (rst)
       begin
          spi_debug         <= #DELAY 0;
          data_sample_delay <= #DELAY 0;
          spi_start_aq      <= #DELAY 0;
       end
     else if (byte_arrival && !ack && tart_state == `TART_WRITE)
       case (tart_addr)
         ADDR_STARTAQ:      spi_start_aq      <= #DELAY drx[0];
         ADDR_SAMPLE_DELAY: data_sample_delay <= #DELAY drx[2:0];
         ADDR_DEBUG:        spi_debug         <= #DELAY drx[0];
       endcase // case (tart_addr)


   //-------------------------------------------------------------------------
   //  DRAM prefetch logic.
   //-------------------------------------------------------------------------
   wire [23:0]          antenna_data;
   reg                  data_sent = 0;
   wire                 sent_w = tart_state == `TART_READ && wrap_addr && byte_arrival && ack;

   always @(posedge clk)
     if (rst) data_sent <= #DELAY 0;
     else     data_sent <= #DELAY sent_w;

   dram_prefetch #( .WIDTH(24) ) DRAM_PREFETCH0
     ( .clk(clk),
       .rst(rst),
       .dram_ready(data_ready),
       .dram_request(data_request),
       .dram_data(data_in),
       .data_sent(data_sent),
       .fetched_data(antenna_data)
       );


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
       .dat_i(dtx),
       .dat_o(drx),

       .overflow_o(oflow),
       .underrun_o(uflow),
      
       .SCK_pin(SCK),
       .SSEL(SSEL),
       .MOSI(MOSI),
       .MISO(MISO)
       );


endmodule // tart_spi
