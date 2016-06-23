`timescale 1ns/100ps
/*
 * 
 * TART's data-aquisition control subcircuit, connected via a Wishbone-like
 * interconnect.
 * 
 * Has system registers for:
 *   000  --  antenna data[23:16];
 *   001  --  antenna data[15: 8];
 *   010  --  antenna data[ 7: 0];
 *   011  --  antenna data stream;
 *   101  --  aquisition sample delay;
 *   110  --  aquisition debug mode; and
 *   111  --  aquisition status and control.
 * 
 * NOTE:
 *  + supports both classic and pipelined transfers;
 * 
 * TODO:
 * 
 */

// Support CLASSIC Wishbone-like bus transactions?
`define __WB_CLASSIC
// `undef __WB_CLASSIC

module tart_aquire
  #(parameter WIDTH = 8,
    parameter MSB   = WIDTH-1,
    parameter DELAY = 3)
   (
    // Wishbone-like bus interface:
    input              clk_i,
    input              rst_i,
    input              cyc_i,
    input              stb_i,
    input              we_i,
    output reg         ack_o = 0,
    input [2:0]        adr_i,
    input [MSB:0]      dat_i,
    output reg [MSB:0] dat_o = 0,

    // Streaming data interface
    // NOTE: Doesn't need to initiate transfers, but data is valid whenever
    //   `data_ready` is asserted.
    input              data_ready,
    output             data_request,
    input [23:0]       data_in,

    input              spi_busy,
    output reg         aq_debug_mode = 0,
    output reg         aq_enabled = 0,
	  output reg [2:0]   aq_sample_delay = 0
    );

   wire [MSB:0]        aq_delay  = {{(MSB-2){1'b0}}, aq_sample_delay};
   wire [MSB:0]        aq_debug  = {{MSB{1'b0}}, aq_debug_mode};
   wire [MSB:0]        aq_status = {{MSB{1'b0}}, aq_enabled};


   //-------------------------------------------------------------------------
   //  Wishbone-like (slave) bus interface logic.
   //-------------------------------------------------------------------------
   //  Generate acknowledges for incoming requests.
   always @(posedge clk_i)
     if (rst_i) ack_o <= #DELAY 0;
     else       ack_o <= #DELAY cyc_i && stb_i && !ack_o;

   //  Read back the appropriate register.
   always @(posedge clk_i)
`ifdef __WB_CLASSIC
     if (cyc_i && stb_i && !we_i && !ack_o)
`else
     if (cyc_i && stb_i && !we_i)
`endif
       case (adr_i)
         0: dat_o <= #DELAY stream;
         1: dat_o <= #DELAY antenna_data[23:16];
         2: dat_o <= #DELAY antenna_data[15: 8];
         3: dat_o <= #DELAY antenna_data[ 7: 0];
         5: dat_o <= #DELAY aq_delay;
         6: dat_o <= #DELAY aq_debug;
         7: dat_o <= #DELAY aq_status;
         default:
           dat_o  <= #DELAY 'bx;
       endcase // case (adr_i)

   //  Write into the appropriate register.
   always @(posedge clk_i)
     if (rst_i)
       {aq_enabled, aq_debug_mode, aq_sample_delay} <= #DELAY 0;
     else if (cyc_i && stb_i && we_i)
       case (adr_i)
         5: aq_sample_delay <= #DELAY dat_i[2:0];
         6: aq_debug_mode   <= #DELAY dat_i[0];
         7: aq_enabled      <= #DELAY dat_i[0];
       endcase // case (adr_i)


   //-------------------------------------------------------------------------
   //  DRAM prefetcher-control logic.
   //-------------------------------------------------------------------------
   wire [23:0]          antenna_data;
   reg                  data_sent = 0;
   wire                 send = cyc_i && stb_i && !we_i && adr_i == 0 && !ack_o;
   wire [7:0]           data [0:2];
   wire [7:0]           stream = data[index];
   reg [1:0]            index = 0;
   wire                 wrap_index = index == 2;
   wire [1:0]           next_index = wrap_index ? 0 : index + 1;

   assign data[0] = antenna_data[23:16];
   assign data[1] = antenna_data[15: 8];
   assign data[2] = antenna_data[ 7: 0];


   //-------------------------------------------------------------------------
   //  Increment the current antenna-data index, and prefetch more data as
   //  needed.
   always @(posedge clk_i)
     if (rst_i) data_sent <= #DELAY 0;
     else       data_sent <= #DELAY wrap_index && send;

   always @(posedge clk_i)
     if (!spi_busy) index <= #DELAY 0;
     else if (send) index <= #DELAY next_index;


   //-------------------------------------------------------------------------
   //  DRAM prefetch logic core.
   //-------------------------------------------------------------------------
   dram_prefetch #( .WIDTH(24) ) DRAM_PREFETCH0
     ( .clk(clk_i),
       .rst(rst_i),
       .dram_ready(data_ready),
       .dram_request(data_request),
       .dram_data(data_in),
       .data_sent(data_sent),
       .fetched_data(antenna_data)
       );


endmodule // tart_aquire
