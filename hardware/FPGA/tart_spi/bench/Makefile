IVC=iverilog
INC=../include/
OPT=-D__icarus -Wall -I$(INC)

OUT=../build

# Source directories:
RTL =../verilog
FIFO=$(RTL)/fifo
BUS =$(RTL)/bus
SPI =$(RTL)/spi

# Some Wishbone-like cores:
WBL =$(BUS)/wb_reset.v $(BUS)/wb_sram.v $(BUS)/wb_sram_dual_port.v $(BUS)/wb_prefetch.v $(BUS)/wb_stream.v

# Common library cores:
LIB =$(FIFO)/afifo16.v $(FIFO)/fifo_dc_gray.v $(FIFO)/afifo_gray.v $(RTL)/tart_aquire.v $(RTL)/tart_control.v $(WBL)

DEP =$(LIB) $(SPI)/spi_target.v $(SPI)/spi_layer.v $(SPI)/spi_slave.v $(SPI)/spi_master.v $(RTL)/dram_prefetch.v $(RTL)/tart_spi.v
COR =$(RTL)/tart_visibilities.v $(RTL)/tart_correlator.v $(RTL)/correlator_block.v $(RTL)/correlator.v $(RTL)/correlator_sdp.v $(RTL)/correlate_cos_sin.v $(RTL)/fake_hilbert.v $(RTL)/ones_count.v

all: ${DEP} tart_soc_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) tart_soc_tb.v

spi: ${DEP} tart_spi_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) tart_spi_tb.v

cor: ${DEP} ${COR} tart_correlator_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) $(COR) tart_correlator_tb.v

.PHONY: run
run: all
	@$(OUT)/tart.out

dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ $(OUT)/tart.out
