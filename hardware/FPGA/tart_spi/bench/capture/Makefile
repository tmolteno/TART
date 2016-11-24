IVC=iverilog
INC=../../include/
OPT=-D__icarus -Wall -I$(INC)

OUT=../../build

# Source directories:
RTL =../../verilog
CAP =$(RTL)/capture
MISC=$(RTL)/misc
BUS =../../../wishbone/rtl

# Library sources:
LIB =$(RTL)/fifo/afifo_gray.v $(RTL)/mfsr/mfsr32.v $(MISC)/shift_reg.v $(BUS)/wb_transfer.v

# Simulation-only files:
XIL =xilinx
SIM =signal_stagger.v
DEP =${CAP}/signal_capture.v ${CAP}/align_captures.v
TOP =${LIB} ${CAP}/tart_capture.v ${CAP}/capture_control.v ${CAP}/signal_centre.v ${CAP}/raw_capture.v ${CAP}/fake_telescope.v ${CAP}/fifo_control.v


all: acq

sig: ${DEP} ${SIM} signal_capture_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) $(SIM) signal_capture_tb.v

old: ${DEP} ${SIM} align_captures_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) $(SIM) align_captures_tb.v

cap: ${TOP} ${DEP} ${SIM} tart_capture_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(TOP) $(DEP) $(SIM) tart_capture_tb.v


.PHONY: run
run: all
	@$(OUT)/tart.out

.PHONY: dir
dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ $(OUT)/tart.out
