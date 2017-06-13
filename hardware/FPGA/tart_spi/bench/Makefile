IVC=iverilog
INC=../include/
OPT=-D__icarus -Wall -I$(INC)

OUT=../build

# Source directories:
RTL =../verilog
FIFO=$(RTL)/fifo
MISC=$(RTL)/misc
CORD=$(RTL)/correlator
BUS =../../wishbone/rtl
SPI =$(RTL)/spi
ACQ =$(RTL)/acquire

# Some Wishbone-like cores:
WBL =$(BUS)/wb_reset.v $(BUS)/wb_sram_port.v $(BUS)/wb_fetch.v $(BUS)/wb_cycle.v $(BUS)/wb_transfer.v $(BUS)/wb_sram_stream.v $(BUS)/wb_sram_prefetch.v

# Common library cores:
LIB =$(FIFO)/afifo16.v $(FIFO)/fifo_dc_gray.v $(FIFO)/afifo_gray.v $(MISC)/shift_reg.v $(RTL)/tart_control.v $(WBL) $(RTL)/xilinx/RAM32X6_SDP.v $(RTL)/xilinx/RAMB8X32_SDP.v $(RTL)/xilinx/RAMB8X36_SDP.v $(RTL)/xilinx/MUX8.v $(RTL)/mfsr/mfsr32.v $(RTL)/xilinx/RAMB16X16X4_TDP.v

# Simulation-only files:
XIL =xilinx
SIM =$(XIL)/DSP48A1.v $(XIL)/LUT6.v $(XIL)/MUXF7.v ${XIL}/IDDR2.v

DEP =$(LIB) $(SPI)/spi_layer.v $(SPI)/spi_slave_wb.v $(SPI)/spi_slave.v $(SPI)/spi_master.v $(ACQ)/dram_prefetch.v

##  Correlator files:
COR =$(CORD)/tart_visibilities.v $(CORD)/bank_switch.v $(CORD)/correlator_SDP.v $(CORD)/correlator_DSP.v $(CORD)/rmw_address_unit.v $(CORD)/correlate_cos_sin.v $(CORD)/correlate_cos_sin_DSP.v $(CORD)/fake_hilbert.v $(CORD)/control.v $(CORD)/correlator_block.v $(CORD)/block_DSP.v $(CORD)/block_SDP.v $(CORD)/tart_correlator.v
# $(RTL)/tart_correlator.v

##  DSP files:
DSP =$(COR) $(CORD)/tart_dsp.v
FAK =$(COR) $(CORD)/tart_fake_dsp.v

# TODO:
TOP =$(DEP) $(COR) $(ACQ)/align_captures.v $(ACQ)/signals_capture.v $(ACQ)/capture.v $(ACQ)/raw_capture.v $(ACQ)/tart_capture.v $(RTL)/tart.v

all: dsp

cor: ${DEP} ${COR} correlator/tart_correlator_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) $(COR) $(SIM) correlator/tart_correlator_tb.v

dsp: ${DEP} ${DSP} tart_dsp_tb.v
	$(IVC) $(OPT) -s tart_dsp_tb -o $(OUT)/tart.out $(DEP) $(DSP) $(SIM) tart_dsp_tb.v

fak: ${DEP} ${FAK} tart_dsp_tb.v
	$(IVC) $(OPT) -D__USE_FAKE_DSP -o $(OUT)/tart.out $(DEP) $(FAK) $(SIM) tart_dsp_tb.v

BLK =$(CORD)/bank_switch.v $(RTL)/correlator_block_SDP.v $(RTL)/correlator_block_DSP.v $(RTL)/correlator_SDP.v $(RTL)/correlator_DSP.v $(RTL)/rmw_address_unit.v $(RTL)/correlate_cos_sin.v $(RTL)/correlate_cos_sin_DSP.v $(RTL)/fake_hilbert.v $(RTL)/xilinx/RAM32X6_SDP.v $(RTL)/xilinx/RAMB8X32_SDP.v $(RTL)/xilinx/MUX8.v $(BUS)/wb_get_block.v $(RTL)/mfsr/mfsr32.v

blk: ${BLK} tart_block_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(BLK) $(SIM) tart_block_tb.v

ctl: ${COR} correlator/control_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(COR) $(SIM) control_tb.v

#
#  Simulate Wishbone (B4 spec) pipelined BURST READS & WRITES.
#
FCH =$(BUS)/wb_fetch.v $(BUS)/wb_store.v $(BUS)/wb_sram_port.v $(RTL)/xilinx/RAMB16X32X8_TDP.v

fch: ${FCH} bus/wb_fetch_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(FCH) bus/wb_fetch_tb.v

#  Simulate the TART Wishbone arbitration and control logic.
ARB =${RTL}/tart_wishbone.v

arb: ${ARB} tart_wishbone_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(ARB) tart_wishbone_tb.v


.PHONY: run
run: all
	@$(OUT)/tart.out

dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ $(OUT)/tart.out
