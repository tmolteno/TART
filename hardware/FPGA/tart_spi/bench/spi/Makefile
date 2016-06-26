IVC=iverilog
OPT=-D__icarus

OUT=../../build

RTL =../../verilog
FIFO=$(RTL)/fifo
BUS =$(RTL)/bus
SPI =$(RTL)/spi
LIB =$(FIFO)/afifo16.v $(FIFO)/fifo_dc_gray.v $(FIFO)/afifo_gray.v
DEP =$(LIB) $(SPI)/spi_layer.v $(SPI)/spi_slave.v $(SPI)/spi_master.v $(BUS)/wb_sram.v
OLD =$(LIB) $(SPI)/spi_target.v $(SPI)/old_slave.v $(SPI)/spi_master.v $(BUS)/wb_sram.v

all: ${DEP} spi_slave_tb.v
	$(IVC) $(OPT) -o $(OUT)/spi.out $(DEP) spi_slave_tb.v

old: ${OLD} spi_slave_tb.v
	$(IVC) $(OPT) -o $(OUT)/spi.out $(DEP) spi_slave_tb.v
	@$(OUT)/spi.out

.PHONY: run
run: all
	@$(OUT)/spi.out

dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ spi.out
