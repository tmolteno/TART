
`timescale 1 ns / 1 ps

module tart_v1_0_S00_AXI #
(
    // Bus bit-widths:
    parameter WIDTH = 8,       // TODO: currently must be `8`!
    parameter MSB   = WIDTH-1,
    parameter ASB   = WIDTH-2,
    //      parameter BYTES = WIDTH>>3, // TODO: byte selects
    //      parameter SSB   = BYTES-1,
    
    // Wishbone modes/parameters:
    parameter ASYNC = 1,
    parameter PIPED = 1,
    parameter CHECK = 1,
    
    parameter HEADER_BYTE  = 8'hA7, // Pattern to send as the first byte
    parameter DELAY = 3,
    
    // Width of S_AXI data bus
    parameter integer C_S_AXI_DATA_WIDTH	= 32,
    // Width of S_AXI address bus
    parameter integer C_S_AXI_ADDR_WIDTH	= 10
)
(
    // Wishbone (SPEC B4) interface:
    input          clk_i,
    input          rst_i,
    output         cyc_o,
    output         stb_o,
    output         we_o,
    input          ack_i,
    input          wat_i,
    input          rty_i,
    input          err_i,
    output [ASB:0] adr_o,
    //      input [SSB:0]  sel_i, // TODO: support larger word-sizes?
    input [MSB:0]  dat_i,
    output [MSB:0] dat_o,
    
    // Global Clock Signal
    input wire  S_AXI_ACLK,
    // Global Reset Signal. This Signal is Active LOW
    input wire  S_AXI_ARESETN,
    // Write address (issued by master, acceped by Slave)
    input wire [C_S_AXI_ADDR_WIDTH-1 : 0] S_AXI_AWADDR,
    // Write channel Protection type. This signal indicates the
    // privilege and security level of the transaction, and whether
    // the transaction is a data access or an instruction access.
    input wire [2 : 0] S_AXI_AWPROT,
    // Write address valid. This signal indicates that the master signaling
    // valid write address and control information.
    input wire  S_AXI_AWVALID,
    // Write address ready. This signal indicates that the slave is ready
    // to accept an address and associated control signals.
    output wire  S_AXI_AWREADY,
    // Write data (issued by master, acceped by Slave) 
    input wire [C_S_AXI_DATA_WIDTH-1 : 0] S_AXI_WDATA,
    // Write strobes. This signal indicates which byte lanes hold
    // valid data. There is one write strobe bit for each eight
    // bits of the write data bus.    
    input wire [(C_S_AXI_DATA_WIDTH/8)-1 : 0] S_AXI_WSTRB,
    // Write valid. This signal indicates that valid write
    // data and strobes are available.
    input wire  S_AXI_WVALID,
    // Write ready. This signal indicates that the slave
    // can accept the write data.
    output wire  S_AXI_WREADY,
    // Write response. This signal indicates the status
    // of the write transaction.
    output wire [1 : 0] S_AXI_BRESP,
    // Write response valid. This signal indicates that the channel
    // is signaling a valid write response.
    output wire  S_AXI_BVALID,
    // Response ready. This signal indicates that the master
    // can accept a write response.
    input wire  S_AXI_BREADY,
    // Read address (issued by master, acceped by Slave)
    input wire [C_S_AXI_ADDR_WIDTH-1 : 0] S_AXI_ARADDR,
    // Protection type. This signal indicates the privilege
    // and security level of the transaction, and whether the
    // transaction is a data access or an instruction access.
    input wire [2 : 0] S_AXI_ARPROT,
    // Read address valid. This signal indicates that the channel
    // is signaling valid read address and control information.
    input wire  S_AXI_ARVALID,
    // Read address ready. This signal indicates that the slave is
    // ready to accept an address and associated control signals.
    output wire  S_AXI_ARREADY,
    // Read data (issued by slave)
    output wire [C_S_AXI_DATA_WIDTH-1 : 0] S_AXI_RDATA,
    // Read response. This signal indicates the status of the
    // read transfer.
    output wire [1 : 0] S_AXI_RRESP,
    // Read valid. This signal indicates that the channel is
    // signaling the required read data.
    output wire  S_AXI_RVALID,
    // Read ready. This signal indicates that the master can
    // accept the read data and response information.
    input wire  S_AXI_RREADY
);

// AXI4LITE signals
reg [C_S_AXI_ADDR_WIDTH-1 : 0] 	axi_awaddr;
reg  	axi_awready;
reg  	axi_wready;
reg [1 : 0] 	axi_bresp;
reg  	axi_bvalid;
reg [C_S_AXI_ADDR_WIDTH-1 : 0] 	axi_araddr;
reg  	axi_arready;
reg [C_S_AXI_DATA_WIDTH-1 : 0] 	axi_rdata;
reg [1 : 0] 	axi_rresp;
reg  	axi_rvalid;

// Example-specific design signals
// local parameter for addressing 32 bit / 64 bit C_S_AXI_DATA_WIDTH
// ADDR_LSB is used for addressing 32/64 bit registers/memories
// ADDR_LSB = 2 for 32 bits (n downto 2)
// ADDR_LSB = 3 for 64 bits (n downto 3)
localparam integer ADDR_LSB = (C_S_AXI_DATA_WIDTH/32) + 1;
localparam integer OPT_MEM_ADDR_BITS = 1;

reg [C_S_AXI_DATA_WIDTH-1:0]	 reg_data_out;
reg	 aw_en;

// wishbone registers. 

reg [ASB:0] adr;
reg [MSB:0] dat;
reg we;
reg read, write;

wire start;

assign we_o = we;
assign adr_o = adr;
assign dat_o = dat;

reg [2:0] c_rd_start, c_wr_start, c_done;
reg c_rd_start_t, c_wr_start_t, c_done_t;
reg r_busy, w_busy;


// I/O Connections assignments

assign S_AXI_AWREADY	= axi_awready;
assign S_AXI_WREADY	= axi_wready;
assign S_AXI_BRESP	= axi_bresp;
assign S_AXI_BVALID	= axi_bvalid;
assign S_AXI_ARREADY	= axi_arready;
assign S_AXI_RDATA	= axi_rdata;
assign S_AXI_RRESP	= axi_rresp;
assign S_AXI_RVALID	= axi_rvalid;


always @( posedge S_AXI_ACLK )
begin
  if ( S_AXI_ARESETN == 1'b0 )
    begin
      axi_awready <= 1'b0;
      axi_wready <= 1'b0;
      aw_en <= 1'b1;
      axi_awaddr <= 0;
    end 
  else
    begin    
      if (~axi_awready && S_AXI_AWVALID && S_AXI_WVALID && aw_en)
        begin
          axi_awready <= 1'b1;
          axi_wready <= 1'b1;
          aw_en <= 1'b0;
          axi_awaddr <= S_AXI_AWADDR;
        end
        else if (S_AXI_BREADY && axi_bvalid)
            begin
              aw_en <= 1'b1;
              axi_awready <= 1'b0;
            end
      else
        begin
          axi_awready <= 1'b0;
          axi_wready <= 1'b0;
        end
    end 
end       

always @( posedge S_AXI_ACLK )
begin
	if ( S_AXI_ARESETN == 1'b0 )
	begin
		axi_bvalid  <= 0;
		axi_bresp   <= 2'b0;
		w_busy <= 0;
	end 
	else
	begin    
	if (!w_busy && axi_awready && S_AXI_AWVALID && axi_wready && S_AXI_WVALID)
	begin
		c_wr_start_t <= !c_wr_start_t;
		w_busy <= 1;
	end
	else if (w_busy && (c_done[2] ^ c_done[1]))
	begin
		w_busy <= 0;
		// indicates a valid write response is available
		axi_bvalid <= 1'b1;
		axi_bresp  <= 2'b0; // 'OKAY' response 
	end                   // work error responses in future
	else if (S_AXI_BREADY && axi_bvalid) 
	begin
		axi_bvalid <= 1'b0; 
	end
end
end

always @( posedge S_AXI_ACLK )
begin
	if ( S_AXI_ARESETN == 1'b0 )
	begin
		axi_arready <= 1'b0;
		axi_araddr  <= 32'b0;
	end 
	else
	begin    
	if (~axi_arready && S_AXI_ARVALID)
	begin
		axi_arready <= 1'b1;
		axi_araddr  <= S_AXI_ARADDR;
	end
	else
	begin
		axi_arready <= 1'b0;
	end
end 
end


always @( posedge S_AXI_ACLK )
begin
	if ( S_AXI_ARESETN == 1'b0 )
	begin
		axi_rvalid <= 0;
		axi_rresp  <= 0;
		axi_rdata <= 0;
		c_rd_start_t <= 0;
		r_busy <= 0;
	end 
	else
	begin
		if (!r_busy && axi_arready && S_AXI_ARVALID && ~axi_rvalid)
		begin
			c_rd_start_t <= !c_rd_start_t;
			r_busy <= 1;
		end
		else if (r_busy && (c_done[2] ^ c_done[1]))
		begin
			r_busy <= 0;
			axi_rvalid <= 1'b1;
			axi_rresp  <= 2'b0; // 'OKAY' response
			axi_rdata  <= reg_data_out;
		end
		else if (axi_rvalid && S_AXI_RREADY)
		begin
			axi_rvalid <= 1'b0;
		end
	end
end

assign start = !busy && (read || write);

always @(posedge clk_i)
begin
	if (rst_i)
	begin
		reg_data_out <= 32'h0;
		c_done_t <= 0;
		read <= 0;
		write <= 0;
	end
	else
	begin
		if (c_rd_start[2] ^ c_rd_start[1])
		begin
			we <= 0;
			read <= 1;
			adr <= axi_araddr[(ASB+2):2];
		end
		else if (c_wr_start[2] ^ c_wr_start[1])
		begin
			we <= 1;
			write <= 1;
			adr <= axi_awaddr[(ASB+2):2];
			dat <= S_AXI_WDATA[MSB:0];
		end
		else if (read && (done || fail))
		begin
			read <= 0;
			c_done_t <= !c_done_t;
			reg_data_out <= {busy, 21'b0, dat_i, done, fail};
		end
		else if (write && (done || fail))
		begin
			write <= 0;
			c_done_t <= !c_done_t;
		end
	end
end

always @( posedge S_AXI_ACLK )
begin
	if ( S_AXI_ARESETN == 1'b0 )
	begin
		c_done <= 0;
	end
	else
	begin
		c_done <= {c_done[1:0], c_done_t};
	end
end	

always @( posedge clk_i )
begin
	if (rst_i)
	begin
		c_rd_start <= 0;
		c_wr_start <= 0;
	end
	else
	begin
		c_rd_start <= {c_rd_start[1:0], c_rd_start_t};
		c_wr_start <= {c_wr_start[1:0], c_wr_start_t};
	end
end


wb_cycle
#( .ASYNC(ASYNC),
	.PIPED(PIPED),
	.CHECK(CHECK),
	.DELAY(DELAY)
) RWCYC
(
	.clk_i(clk_i),
	.rst_i(rst_i),
	.cyc_o (cyc_o),
	.stb_o(stb_o),
	.ack_i(ack_i),
	.wat_i(wat_i),
	.rty_i(rty_i),
	.err_i(err_i),
	.start_i(start),
	.busy_o(busy),
	.done_o(done),
	.fail_o(fail)
);

endmodule

