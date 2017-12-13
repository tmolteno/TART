
`timescale 1 ns / 1 ps

module tart_v1_0 #
(		
    //  Antenna/signal parameters:
    parameter integer ANTENNAE = 24,
    parameter integer NSB      = 23,
    parameter integer ALIGN    = 1,
    
    parameter integer ACCUM_BITS = 24,
    parameter integer VISB_LOG2  = 20,
    
    parameter integer READ_BANKS  = 16,
    parameter integer BANK_BITS  = 4,
    
    parameter integer WBADR_BITS = 12,
    parameter integer WBBUS_BITS =  8,
    
    parameter integer TMUX_RATE  = 12,
    parameter integer TMUX_BITS  =  4,
    
    parameter integer READ_COUNT = 576,
    parameter integer READ_BITS  =  10,
    
    //  Fake antenna-data options:
    parameter integer DEBUG    = 0,
    parameter integer MULTI    = 1,
    parameter integer RNG      = 1,
    parameter integer CONST    = 0,
    parameter integer CDATA    = 24'h0,
    
    //  Wishbone mode settings/parameters:
    parameter integer ASYNC    = 0,     // asynchronous WB transactions (0/1)?
    parameter integer PIPED    = 1,     // pipelined BURST transactions (0/1)?
    parameter integer RESET    = 0,     // fast-reset enable (0/1)?
    parameter integer CHECK    = 1,     // bus-signal sanity-checking (0/1)?
    //     parameter CHECK    = 0,     // bus-signal sanity-checking (0/1)?
    parameter integer VIZWR    = 0,     // enable writes to viz-buffer (0/1)?
    
    //  Simulation-only parameters:
    parameter integer DELAY    = 3,
    

    // Parameters of Axi Slave Bus Interface S00_AXI
    parameter integer C_S00_AXI_DATA_WIDTH	= 32,
    parameter integer C_S00_AXI_ADDR_WIDTH	= 10
)
(
    // TELESCOPE
    input [NSB:0]      antenna, // Radio Data Interface

	    // Shouldn't start until clk_locked is asserted.
    input clk_locked,// clocks are ready to be used.
    input clk_i,     // 16.368 MHZ buffered
    input clk6x_i,   // 16.368x6 = 98.208 MHz
    input clk6n_i,   // 16.368x6 = 98.208 MHz
    input clk12x_i,  // 16.368x12 = 196.416 MHz
   
    // Ports of Axi Slave Bus Interface S00_AXI
    input wire  s00_axi_aclk,
    input wire  s00_axi_aresetn,
    input wire [C_S00_AXI_ADDR_WIDTH-1 : 0] s00_axi_awaddr,
    input wire [2 : 0] s00_axi_awprot,
    input wire  s00_axi_awvalid,
    output wire  s00_axi_awready,
    input wire [C_S00_AXI_DATA_WIDTH-1 : 0] s00_axi_wdata,
    input wire [(C_S00_AXI_DATA_WIDTH/8)-1 : 0] s00_axi_wstrb,
    input wire  s00_axi_wvalid,
    output wire  s00_axi_wready,
    output wire [1 : 0] s00_axi_bresp,
    output wire  s00_axi_bvalid,
    input wire  s00_axi_bready,
    input wire [C_S00_AXI_ADDR_WIDTH-1 : 0] s00_axi_araddr,
    input wire [2 : 0] s00_axi_arprot,
    input wire  s00_axi_arvalid,
    output wire  s00_axi_arready,
    output wire [C_S00_AXI_DATA_WIDTH-1 : 0] s00_axi_rdata,
    output wire [1 : 0] s00_axi_rresp,
    output wire  s00_axi_rvalid,
    input wire  s00_axi_rready
);
    //-------------------------------------------------------------------------
    //
    //     ADDITIONAL TART SETTINGS
    //
    //-------------------------------------------------------------------------
    //  Visibilities/correlator settings.
    parameter ACCUM = ACCUM_BITS; // Bit-width of the accumulators
    parameter BLOCK = ACCUM;       // Maximum #bits of the block-size
    parameter MSB   = BLOCK-1;     // Data transfer MSB
    parameter TRATE = TMUX_RATE;  // Time-multiplexing rate
    parameter TBITS = TMUX_BITS;   // TMUX bits
    parameter COUNT = VISB_LOG2;  // (1 << 3) - 1;
    parameter NREAD = READ_COUNT; // Number of visibilities to read back
    parameter RBITS = READ_BITS;  // = ceiling{log2(NREAD)};
    parameter RSB   = RBITS-1;     // MSB of read-back address
    
    //  Wishbone settings.
    parameter BBITS = WBBUS_BITS; // Bit-width of the SoC Wishbone bus
    parameter BSB   = BBITS-1;     // Bus data MSB
    parameter WSB   = BBITS-2;     // axi -> WB bus address-width
    parameter ABITS = WBADR_BITS; // Correlator bus address bit-width
    parameter ASB   = ABITS-1;     // Address MSB

    //  Settings for the visibilities data banks:
    parameter XBITS = BANK_BITS;  // Bit-width of the block-counter
    parameter XSB   = XBITS-1;     // MSB of the block-counter
    
    //  Internal correlator data-bus settings:
    parameter CBITS = XBITS+RBITS; // Correlator-bus address bit-width
    parameter CSB   = CBITS-1;
    
    //-------------------------------------------------------------------------
    //
    //  TART SIGNALS
    //
    //-------------------------------------------------------------------------
    wire fpga_clk;
    wire clk_x;
    wire reset;
    
    wire                clock_n, reset_n;
    wire                clock_b, reset_b; // WB system signals
    
    wire                rx_clk_16_buf;
    
    
    //-------------------------------------------------------------------------
    //  Wishbone bus signals to/from the off-chip, axi I/O interface.
    wire         axi_cyc, axi_stb, axi_we;
    wire         axi_ack, axi_wat, axi_rty, axi_err;
    wire [WSB:0] axi_adr;
    wire [BSB:0] axi_dtx, axi_drx;
    
    //-------------------------------------------------------------------------
    //  Wishbone signals for the raw-data capture & recovery module .
    wire         cap_cyc, cap_stb, cap_we;
    wire         cap_ack, cap_wat, cap_rty, cap_err;
    wire [1:0]   cap_adr;
    wire [BSB:0] cap_drx, cap_dtx;
    //  Wishbone signals for the data-acquisition device.
    wire         acq_cyc, acq_stb, acq_we;
    wire         acq_ack, acq_wat, acq_rty, acq_err;
    wire [1:0]   acq_adr;
    wire [BSB:0] acq_drx, acq_dtx;
    
    //  Wishbone signals for the DSP (visibilities) core.
    wire         dsp_cyc, dsp_stb, dsp_we;
    wire         dsp_ack, dsp_wat, dsp_rty, dsp_err;
    wire [1:0]   dsp_adr;
    wire [BSB:0] dsp_drx, dsp_dtx;
    
    //  Wishbone signals for the reset/control unit.
    wire         sys_cyc, sys_stb, sys_we;
    wire         sys_ack, sys_wat, sys_rty, sys_err;
    wire [1:0]   sys_adr;
    wire [BSB:0] sys_drx, sys_dtx;
    
    //-------------------------------------------------------------------------
    //  Visibilities-unit signals:
    wire                vx_enabled, vx_pending, vx_overflow, vx_overwrite;
    wire                vx_newblock, vx_streamed, vx_switching, vx_accessed;
    wire [MSB:0]        vx_checksum;
    wire                vx_stuck, vx_limp;      // additional debug signals
    wire [XSB:0]        vx_bank;
    
    //-------------------------------------------------------------------------
    //  Capture-unit info signals:
    wire                cx_enabled, cx_debug;
    wire                cx_strobe, cx_middle, cx_locked;
    wire [MSB:0]        cx_signal;
    
    //-------------------------------------------------------------------------
    //  Acquisition (of antenna raw-data) signals:   
    wire                aq_enabled;
    wire [2:0]          aq_state;
    
    //-------------------------------------------------------------------------
    //  axi and system-status signals.
    wire                axi_busy, request_from_axi;
    wire                axi_uflow, axi_oflow;
    wire [7:0]          sys_status, axi_status;
    
    //  Miscellaneous assignments.
    //-------------------------------------------------------------------------
    assign clock_b = fpga_clk;
    assign reset_b = reset;
   
    //-------------------------------------------------------------------------
    //  Compose the status-signal from the most important status-signals of
    //  TART's subsystems.
    assign sys_status =  {vx_enabled, vx_pending, // visibilities status
                          cx_enabled,   cx_debug, // capture status
                          aq_enabled,  aq_state}; // acquisition status
    assign axi_status = {axi_oflow, axi_uflow, 5'h0, axi_busy};


    /* Rename clock signals. */
    
    assign fpga_clk = clk6x_i;
    assign clock_n = clk6n_i;
    assign rx_clk_16_buf = clk_i;
    assign clk_x = clk12x_i;

    assign reset_n = !clk_locked;
 
    //-------------------------------------------------------------------------
    //
    //  TART SYSTEM-WIDE, WISHBONE (SPEC B4) INTERCONNECT AND PERIPHERALS.
    //
    //-------------------------------------------------------------------------
    //  Local synchronous reset for the Wishbone controller.
    (* NOMERGE *) reg reset_tartwb = 1'b1;
    
    always @(posedge clock_b)
        reset_tartwb <= #DELAY reset_b;
    
    //-------------------------------------------------------------------------
    //  The TART Wishbone bus connects the lone Wishbone master (the axi slave
    //  core) to the:
    //   + raw-data capture unit;
    //   + raw-data acquisition and buffer unit;
    //   + DSP core (that computes the visibilities); and
    //   + TART system-controller.
    //  
    //  TODO:
    //   + parameterise moar stuffs;
    //  
    tart_wishbone
    #(  
        .XBITS(2),
        .ASYNC(1),
        .RESET(RESET),
        .PIPED(PIPED),
        .CHECK(CHECK),
        // supported peripherals:
        .CAPEN(1),
        .ACQEN(1),
        .DSPEN(1),
        .SYSEN(1),
        // simulation-only options:
        .DELAY(DELAY)
    ) ARB
    (
        .bus_clk_i(clock_b),
        .bus_rst_i(reset_tartwb),
        
        //-------------------------------------------------------------------
        //  axi Wishbone master.
        .master_cyc_i(axi_cyc),
        .master_stb_i(axi_stb),
        .master_we_i (axi_we),
        .master_ack_o(axi_ack),
        .master_wat_o(axi_wat),
        .master_rty_o(axi_rty),
        .master_err_o(axi_err),
        .master_adr_i(axi_adr),
        .master_dat_i(axi_drx),
        .master_dat_o(axi_dtx),
        
        //-------------------------------------------------------------------
        //  Capture-unit Wishbone slave.
        .cap_cyc_o(cap_cyc),
        .cap_stb_o(cap_stb),
        .cap_we_o (cap_we),
        .cap_ack_i(cap_ack),
        .cap_wat_i(cap_wat),
        .cap_rty_i(cap_rty),
        .cap_err_i(cap_err),
        .cap_adr_o(cap_adr),
        .cap_dat_i(cap_drx),
        .cap_dat_o(cap_dtx),
        
        //  Acquisition-unit Wishbone slave.
        .acq_cyc_o(acq_cyc),
        .acq_stb_o(acq_stb),
        .acq_we_o (acq_we),
        .acq_ack_i(acq_ack),
        .acq_wat_i(acq_wat),
        .acq_rty_i(acq_rty),
        .acq_err_i(acq_err),
        .acq_adr_o(acq_adr),
        .acq_dat_i(acq_drx),
        .acq_dat_o(acq_dtx),
        
        //  DSP-unit Wishbone slave.
        .dsp_cyc_o(dsp_cyc),
        .dsp_stb_o(dsp_stb),
        .dsp_we_o (dsp_we),
        .dsp_ack_i(dsp_ack),
        .dsp_wat_i(dsp_wat),
        .dsp_rty_i(dsp_rty),
        .dsp_err_i(dsp_err),
        .dsp_adr_o(dsp_adr),
        .dsp_dat_i(dsp_drx),
        .dsp_dat_o(dsp_dtx),
        
        //  System-control-unit Wishbone slave.
        .sys_cyc_o(sys_cyc),
        .sys_stb_o(sys_stb),
        .sys_we_o (sys_we),
        .sys_ack_i(sys_ack),
        .sys_wat_i(sys_wat),
        .sys_rty_i(sys_rty),
        .sys_err_i(sys_err),
        .sys_adr_o(sys_adr),
        .sys_dat_i(sys_drx),
        .sys_dat_o(sys_dtx)
        //-------------------------------------------------------------------
    );
    
    //-------------------------------------------------------------------------
    //
    //  axi SLAVE CORE.
    //
    //-------------------------------------------------------------------------
    //  Local synchronous reset for the axi interface.
    (* NOMERGE *) reg reset_axi = 1'b1;
    
    always @(posedge clock_b)
        reset_axi <= #DELAY reset_b;
    
    //-------------------------------------------------------------------------
    //  NOTE:
    //   + this module is the lone, Wishbone (SPEC B4), master device;
 
    // Instantiation of Axi Bus Interface S00_AXI
    tart_v1_0_S00_AXI # (
        .WIDTH(BBITS),
        .ASYNC(1),
        .PIPED(PIPED),
        .CHECK(CHECK),
        .C_S_AXI_DATA_WIDTH(C_S00_AXI_DATA_WIDTH),
        .C_S_AXI_ADDR_WIDTH(C_S00_AXI_ADDR_WIDTH)
    ) tart_v1_0_S00_AXI_inst (
        .clk_i     (clock_b),
        .rst_i     (reset_axi),
        
        //  Wishbone master interface.
        .cyc_o     (axi_cyc),
        .stb_o     (axi_stb),
        .we_o      (axi_we),
        .ack_i     (axi_ack),
        .wat_i     (axi_wat),
        .rty_i     (axi_rty),
        .err_i     (axi_err),
        .adr_o     (axi_adr),
        .dat_i     (axi_dtx),
        .dat_o     (axi_drx),
        
        .S_AXI_ACLK(s00_axi_aclk),
        .S_AXI_ARESETN(s00_axi_aresetn),
        .S_AXI_AWADDR(s00_axi_awaddr),
        .S_AXI_AWPROT(s00_axi_awprot),
        .S_AXI_AWVALID(s00_axi_awvalid),
        .S_AXI_AWREADY(s00_axi_awready),
        .S_AXI_WDATA(s00_axi_wdata),
        .S_AXI_WSTRB(s00_axi_wstrb),
        .S_AXI_WVALID(s00_axi_wvalid),
        .S_AXI_WREADY(s00_axi_wready),
        .S_AXI_BRESP(s00_axi_bresp),
        .S_AXI_BVALID(s00_axi_bvalid),
        .S_AXI_BREADY(s00_axi_bready),
        .S_AXI_ARADDR(s00_axi_araddr),
        .S_AXI_ARPROT(s00_axi_arprot),
        .S_AXI_ARVALID(s00_axi_arvalid),
        .S_AXI_ARREADY(s00_axi_arready),
        .S_AXI_RDATA(s00_axi_rdata),
        .S_AXI_RRESP(s00_axi_rresp),
        .S_AXI_RVALID(s00_axi_rvalid),
        .S_AXI_RREADY(s00_axi_rready)
    );
    
    //-------------------------------------------------------------------------
    //
    //  RAW-DATA CAPTURE & CLOCK-RECOVERY.
    //
    //-------------------------------------------------------------------------
    //  Local synchronous reset for the data-capture module.
    (* NOMERGE *) reg reset_capture = 1'b1;
    
    always @(posedge clock_b)
       reset_capture <= #DELAY reset_b;
    
    //-------------------------------------------------------------------------
    //  Raw-data generally requires clock-recovery, as each antenna's clock
    //  has an unknown phase-delay associated with it. Clock-recovery for each
    //  antenna/channel removes these delays from each channel.
    //  
    //  NOTE:
    //   + capture registers are mapped to '0b00nnnnn'.
    //   + for testing purposes, registers can be set so that fake data is
    //     generated;
    //  
    tart_capture
    #( 
        .AXNUM(ANTENNAE),
        // use additional data-capture and alignment circuitry?
        .ALIGN(ALIGN),
        .RATIO(TRATE),
        .RBITS(TBITS),
        .TICKS(4),              // to match pipeline-register delays
        // Wishbone mode settings
        .RESET(RESET),
        .PIPED(PIPED),
        .CHECK(CHECK),
        // fake-data options:
        .MULTI(MULTI),
        .RNG  (RNG),
        .CONST(CONST),
        .CDATA(CDATA),
        // simulation-only settings:
        .DELAY(DELAY)
    ) CAPTURE
    (
        //--------------------------------------------------------------------
        //  Global clocks & resets:
        .clock_e   (rx_clk_16_buf),
        .clock_n   (clock_n),   // negated system-clock
        .clock_i   (clock_b),
        .reset_i   (reset_capture),
        
        //--------------------------------------------------------------------
        //  Wishbone (SPEC B4) interconnect:
        .cyc_i     (cap_cyc),
        .stb_i     (cap_stb),
        .we_i      (cap_we ),
        .ack_o     (cap_ack),
        .wat_o     (cap_wat),
        .rty_o     (cap_rty),
        .err_o     (cap_err),
        .adr_i     (cap_adr),
        .dat_i     (cap_dtx),
        .dat_o     (cap_drx),
        
        //--------------------------------------------------------------------
        //  External antenna data:
        .signal_e_i(antenna),
        
        //-------------------------------------------------------------------------
        //  Source signals, and debug/info:
        .enabled_o(cx_enabled),
        .strobe_o (cx_strobe),
        .middle_o (cx_middle),
        .signal_o (cx_signal),
        .centred_o(cx_locked),
        .debug_o  (cx_debug)
    );
    
    
    
    //-------------------------------------------------------------------------
    //
    //  RAW-DATA ACQUISITION-CONTROL & READ-BACK.
    //
    //-------------------------------------------------------------------------
    //  Local synchronous reset for the raw-data acquisition unit.
    (* NOMERGE *) reg reset_acquire = 1'b1;
    
    always @(posedge clock_b)
      reset_acquire <= #DELAY reset_b;
    
    //-------------------------------------------------------------------------
    //  This module controls the raw-data acquisition unit.
    //  
    //  NOTE:
    //   + acquisition registers are mapped to '0b01nnnnn'.
    //  
    tart_acquire
    #( 
         .AXNUM    (ANTENNAE),
      //  .ABITS    (SDRAM_ADDRESS_WIDTH),
        .BBITS    (BBITS),
        .PIPED    (PIPED),     // Wishbone mode settings
        .RESET    (0),
        .CHECK    (CHECK),
        .DELAY    (DELAY)
    ) ACQUIRE
    ( 
        .clock_i  (clock_b),
        .reset_i  (reset_acquire),
        
        //  Raw-data inputs.
        .locked_i (cx_enabled),
        .strobe_i (cx_strobe),
        .middle_i (cx_middle),
        .signal_i (cx_signal),
        
        //  Wishbone (SPEC B4) bus for raw-data and visibilities.
        .cyc_i    (acq_cyc),
        .stb_i    (acq_stb),
        .we_i     (acq_we),
        .ack_o    (acq_ack),
        .wat_o    (acq_wat),
        .rty_o    (acq_rty),
        .err_o    (acq_err),
        .adr_i    (acq_adr),
        .dat_i    (acq_dtx),
        .dat_o    (acq_drx),
        
        .io_busy_i(axi_busy),
        
        //  Memory controller signals (bus-domain).
        .mcb_ce_o (cmd_enable),
        .mcb_wr_o (cmd_write),
        .mcb_rdy_i(cmd_ready),
        .mcb_ack_i(data_out_ready),
        .mcb_adr_o(cmd_address),
        .mcb_dat_i(data_out),
        .mcb_dat_o(cmd_data_in),
        
        //  Debug signals.
        .enabled_o(aq_enabled),
        .state_o  (aq_state)
    );

    
`ifdef __USE_CORRELATORS
    //-------------------------------------------------------------------------
    //     
    //  CORRELATOR / VISIBILITIES BLOCK.
    //     
    //-------------------------------------------------------------------------
    //  Local synchronous reset for the DSP functional unit.
    (* NOMERGE *) reg reset_dsp = 1'b1;
    
    always @(posedge clock_b)
     reset_dsp <= #DELAY reset_b;
    
    //-------------------------------------------------------------------------
    //  NOTE:
    //   + system-control registers are mapped to '0b10nnnnn'.
    //  
    tart_dsp
     #( 
        .AXNUM(ANTENNAE),       // number of attached antennae
        .ACCUM(ACCUM),          // accumulator bit-width
        .TRATE(TRATE),          // time multiplexing (TMUX) rate
        .TBITS(TBITS),          // TMUX counter bit-width
        .NREAD(NREAD),          // visibilities read-count
        .RBITS(RBITS),
        .XBITS(XBITS),          // visibilities-bank address bit-width
        .CBITS(CBITS),          // correlator address bit-width
        //  Wishbone settings:
        .PIPED(PIPED),          // Wishbone pipelined mode?
        .CHECK(CHECK),          // bus sanity-checking?
        .VIZWR(VIZWR),          // bidirectional streaming access?
        //  Simulation-only options:
        .DELAY(DELAY)           // simulation-only settings
        ) DSP
       (
       //--------------------------------------------------------------------
        .clk_x(clk_x),          // correlator clock
        .clk_i(clock_b),        // Wishbone/system clock
        .rst_i(reset_dsp),      // bus/system reset
    
        //--------------------------------------------------------------------
        //  Captured, oversampled antenna control & data signals:
        .vld_i(cx_enabled),
        .new_i(cx_strobe),      // strobes before each new sample
        .sig_i(cx_signal),      // recovered signal
    
        //--------------------------------------------------------------------
        //  Wishbone (SPEC B4) bus between DSP and acquisition unit:
        .cyc_i(dsp_cyc),
        .stb_i(dsp_stb),
        .we_i (dsp_we),
        .ack_o(dsp_ack),
        .wat_o(dsp_wat),
        .rty_o(dsp_rty),
        .err_o(dsp_err),
        .adr_i(dsp_adr),
        .dat_i(dsp_dtx),
        .dat_o(dsp_drx),
    
        //--------------------------------------------------------------------
        //  Stream Core-Enable:
        .sce_i(axi_busy),
    
        //  Correlator control & status signals:
        .enabled_o  (vx_enabled ),
        .pending_o  (vx_pending ),
        .newblock_o (vx_newblock),
        .checksum_o (vx_checksum),
        .streamed_o (vx_streamed),
    
        //--------------------------------------------------------------------
        //  Miscellaneous debugging/status signals:
        .bank_o     (vx_bank),
        .overflow_o (vx_overflow),
        .stuck_o    (vx_stuck),
        .limp_o     (vx_limp)
    );
    
    
`else // !`ifdef __USE_CORRELATORS
    //-------------------------------------------------------------------------
    //  DSP not used, so clamp some signals.
    //-------------------------------------------------------------------------
    assign vx_streamed = 1'b0;
    assign vx_newblock = 1'b0;
    assign vx_pending  = 1'b0;
    assign vx_checksum = 24'h0ff1ce;
    
`endif //  !`ifdef __USE_CORRELATORS
    
    
    //-------------------------------------------------------------------------
    //
    //  RESET HANDLER.
    //
    //-------------------------------------------------------------------------
    //  NOTE:
    //   + system-control registers are mapped to '0b11nnnnn'.
    //  
    tart_control
    #( 
        .WIDTH(BBITS),
        .RTIME(4),
        .CHECK(0),
        .PIPED(1),
        .DELAY(DELAY)
    ) CONTROL
    (
        .clk_i(clock_b),
        .rst_i(reset_b),
        
        .cyc_i(sys_cyc),
        .stb_i(sys_stb),
        .we_i (sys_we),
        .ack_o(sys_ack),
        .wat_o(sys_wat),
        .rty_o(sys_rty),
        .err_o(sys_err),
        .adr_i(sys_adr),
        .dat_i(sys_dtx),
        .dat_o(sys_drx),
        
        .status_i(sys_status),
        .extra_i (axi_status),
        .reset_ni(reset_n),
        .reset_o (reset)
    );


    endmodule
