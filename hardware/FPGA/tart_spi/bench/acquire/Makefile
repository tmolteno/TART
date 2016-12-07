#
# TODO:
#  + most of this has been moved to the capture folder, so this Makefile needs
#    rebuilding;
#
IVC=iverilog
INC=../../include/
OPT=-D__icarus -Wall -I${INC}

OUT=../../build

# Source directories:
RTL =../../verilog
ACQ =${RTL}/acquire
CAP =${RTL}/capture
FAK =../capture
MISC=${RTL}/misc
BUS =../../../wishbone/rtl

# Library sources:
LIB =${RTL}/fifo/afifo_gray.v ${RTL}/mfsr/mfsr32.v ${MISC}/shift_reg.v ${BUS}/wb_transfer.v

# Simulation-only files:
XIL =../xilinx
SIM =mcb_dummy.v ${XIL}/IOBUF.v ${XIL}/IDDR2.v ${XIL}/ODDR2.v
DEP =${LIB} ${RTL}/SDRAM_Controller_v.v ${ACQ}/raw_acquire.v ${ACQ}/block_buffer.v ${ACQ}/fifo_control.v ${ACQ}/dram_prefetch.v ${FAK}/fake_signal.v ${FAK}/signal_stagger.v
TOP =${ACQ}/tart_acquire.v


all: acq

acq: ${TOP} ${DEP} ${SIM} tart_acquire_tb.v
	${IVC} ${OPT} -o ${OUT}/tart.out ${TOP} ${DEP} ${SIM} tart_acquire_tb.v


.PHONY: run
run: all
	@${OUT}/tart.out

.PHONY: dir
dir:
	@mkdir ${OUT}

.PHONY: clean
clean:
	rm -f *~ ${OUT}/tart.out
