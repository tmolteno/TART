IVC=iverilog
OPT=-D__icarus

OUT=../build

RTL =../verilog
FIFO=$(RTL)/fifo
BUS =$(RTL)/bus
SPI =$(RTL)/spi
LIB =$(FIFO)/afifo16.v $(FIFO)/fifo_dc_gray.v $(FIFO)/afifo_gray.v $(BUS)/wb_reset.v $(RTL)/tart_aquire.v $(RTL)/tart_control.v
DEP =$(LIB) $(SPI)/spi_target.v $(SPI)/spi_layer.v $(SPI)/spi_slave.v $(SPI)/spi_master.v $(BUS)/wb_stream.v $(BUS)/wb_sram.v $(RTL)/dram_prefetch.v $(RTL)/tart_spi.v

all: ${DEP} tart_soc_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) tart_soc_tb.v

spi: ${DEP} tart_spi_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) tart_spi_tb.v

.PHONY: run
run: all
	@$(OUT)/tart.out

dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ tart.out
