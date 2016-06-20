IVC=iverilog
OPT=-D__icarus

OUT=../../build

RTL =../../verilog
FIFO=$(RTL)/fifo
BUS =$(RTL)/bus
SPI =$(RTL)/spi
LIB =$(FIFO)/afifo16.v $(FIFO)/fifo_dc_gray.v
DEP =$(LIB) $(SPI)/spi_target.v $(SPI)/spi_slave.v $(SPI)/spi_master.v $(BUS)/wb_sram.v

all: ${DEP} spi_slave_tb.v
	$(IVC) $(OPT) -o $(OUT)/spi.out $(DEP) spi_slave_tb.v

.PHONY: run
run: all
	@$(OUT)/spi.out

dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ spi.out
