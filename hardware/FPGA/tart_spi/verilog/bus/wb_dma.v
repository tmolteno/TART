`timescale 1ns/100ps
/*
 * 
 * The DMA transfers memory from the source to the destination, and with the
 * supported transfer size limited by the buffer size.
 * 
 * NOTE:
 *  + currently synchronous, and both bus interfaces share the same clock;
 * 
 * TODO:
 * 
 */

// DMA states.
`define DMA_IDLE  0
`define DMA_WAIT  1
`define DMA_READ  2
`define DMA_WRITE 4

module wb_dma
  #(parameter WIDTH = 32,       // Data bits
    parameter MSB   = WIDTH-1,

    parameter ABITS = 32,       // Address bits
    parameter ASB   = ABITS-1,

    parameter BBITS = 10,       // Buffer bits
    parameter BSIZE = (1 << BBITS) - 1,
    parameter BSB   = BBITS-1,

    parameter DELAY = 3)
   (
    input              clk_i,
    input              rst_i,

    // Wishbone-like (slave) interface for controlling the DMA:
    input              s_cyc_i,
    input              s_stb_i,
    input              s_we_i,
    input              s_bst_i, // Bulk Sequential Transfer?
    output reg         s_ack_o = 0,
    output reg         s_wat_i = 0,
    output reg         s_rty_o = 0,
    input [1:0]        s_adr_i,
    input [MSB:0]      s_dat_i,
    output reg [MSB:0] s_dat_o,
    
    // Wishbone-like (master) interface for performing the DMA:
    output reg         m_cyc_o = 0,
    output reg         m_stb_o = 0,
    output reg         m_we_o = 0,
    output reg         m_bst_o = 0, // Bulk Sequential Transfer?
    input              m_ack_i,
    input              m_wat_i,
    input              m_rty_i,
    output reg [ASB:0] m_adr_o = 0,
    input [MSB:0]      m_dat_i,
    output reg [MSB:0] m_dat_o
    );


   //-------------------------------------------------------------------------
   //  DMA configuration bus.
   //-------------------------------------------------------------------------
   reg [ASB:0]         src_adr, dst_adr;
   reg [BSB:0]         dma_len;
   reg                 dma_start = 0;
   reg [3:0]           dma_state = `DMA_IDLE;                 
   wire                dma_idle = dma_state == `DMA_IDLE;
   wire                s_ack = s_bst_i || !s_ack_o;

   //  Write to DMA registers, if the DMA unit is idle.
   always @(posedge clk_i)
     if (rst_i)
       dma_start <= #DELAY 0;
     else if (s_cyc_i && s_stb_i && s_we_i && dma_idle)
       case (s_adr_i)
         0: src_adr   <= #DELAY s_dat_i;
         1: dst_adr   <= #DELAY s_dat_i;
         2: dma_len   <= #DELAY s_dat_i;
         3: dma_start <= #DELAY s_dat_i;
       endcase // case (s_adr_i)
     else if (!dma_idle)
       dma_start <= #DELAY 0;

   always @(posedge clk_i)
     if (s_cyc_i && s_stb_i && !s_we_i)
       case (s_adr_i)
         0: s_dat_o <= #DELAY {'b0, src_adr};
         1: s_dat_o <= #DELAY {'b0, dst_adr};
         2: s_dat_o <= #DELAY {'b0, dma_len};
         3: s_dat_o <= #DELAY {'b0, !dma_idle};
       endcase // case (s_adr_i)

   /*
   //  Generate a "retry" response if DMA transfer is in progress.
   always @(posedge clk_i)
     if (rst_i)
        <= #DELAY 0;
     else if (s_cyc_i && s_stb_i && s_we_i && !dma_idle)
       s_rty_o <= #DELAY 1;
     else
       s_rty_o <= #DELAY 0;
    */

   //  Generate a "retry" response if DMA transfer is in progress, else
   //  "acknowledge" the bus-cycle.
   always @(posedge clk_i)
     if (rst_i)
       {s_rty_o, s_ack_o} <= #DELAY 0;
     else if (s_cyc_i && s_stb_i && (s_bst_i || !s_ack_o))
       {s_rty_o, s_ack_o} <= #DELAY {s_we_i && !dma_idle, !s_we_i || dma_idle};
     else
       {s_rty_o, s_ack_o} <= #DELAY 0;


   //-------------------------------------------------------------------------
   //  DMA control logic.
   //-------------------------------------------------------------------------
   always @(posedge clk_i)
     if (rst_i) dma_state <= #DELAY `DMA_IDLE;
     else
       case (dma_state)
         `DMA_IDLE:
   //-------------------------------------------------------------------------
   //  Generate WB-like transactions.
   //-------------------------------------------------------------------------
   assign bst_w = num > 2 && cyc;
//    assign stb_w = !bst

   always @(posedge clk)
     if (rst) bst <= #DELAY 0;
     else     bst <= #DELAY bst_w || (set || get) && num > 1;

   always @(posedge clk)
     if (rst) begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 0;
     end
     else if (set) begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 7;
     end
     else if (get) begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY 6;
     end
     else if (cyc && stb && ack && bst && !bst_w) begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY {cyc, stb && we, we};
     end
     else if (cyc && ack && (stb && !bst || !stb && !we)) begin
        {fin, get, set} <= #DELAY 4;
        {cyc, stb, we } <= #DELAY 0;
     end
     else begin
        {fin, get, set} <= #DELAY 0;
        {cyc, stb, we } <= #DELAY {cyc, stb, we };
     end

   wire [ASB:0] next_adr = (we && bst || !we && bst_w) ? adr + 1 : adr;

   always @(posedge clk)
     if (rst)             adr <= #DELAY 0;
     else if (set || get) adr <= #DELAY ptr;
     else if (cyc && stb) adr <= #DELAY next_adr;

   always @(posedge clk)
     if (cyc && stb) num <= #DELAY num - 1;


   //-------------------------------------------------------------------------
   //  Wishbone-connected SRAM.
   //-------------------------------------------------------------------------
   wb_sram #( .WIDTH(WIDTH), .SBITS(BBITS) ) SRAM0
     ( .clk_i(clk),
       .rst_i(rst),
       .cyc_i(cyc),
       .stb_i(stb),
       .we_i (we),
       .bst_i(bst),
       .ack_o(ack),
       .adr_i(adr),
       .dat_i(val),
       .dat_o(dat)
       );


endmodule // wb_dma
