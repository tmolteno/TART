IVC=iverilog
INC=../../include/
OPT=-D__icarus -Wall -I$(INC)

OUT=../../build/tart.out

# Source directories:
RTL=../../verilog
CAP=$(RTL)/capture
MSC=$(RTL)/misc
BUS=../../../wishbone/rtl

# Library sources:
LIB=$(RTL)/fifo/afifo_gray.v $(RTL)/mfsr/mfsr32.v $(MSC)/shift_reg.v $(BUS)/wb_transfer.v

# Simulation-only files:
XIL=../xilinx
SIM=fake_signal.v signal_stagger.v ${XIL}/IDDR2.v
DEP=${CAP}/signal_capture.v ${CAP}/signal_phase_DDR.v ${CAP}/signal_phase.v ${CAP}/signal_source.v ${CAP}/align_captures.v
TOP=${LIB} ${CAP}/tart_capture.v ${CAP}/capture_control.v ${CAP}/signal_centre.v ${CAP}/fake_telescope.v


all: sig

sig: ${DEP} ${SIM} signal_capture_tb.v
	$(IVC) $(OPT) -o $(OUT) $(DEP) $(SIM) signal_capture_tb.v

old: ${DEP} ${SIM} align_captures_tb.v
	$(IVC) $(OPT) -o $(OUT) $(DEP) $(SIM) align_captures_tb.v

cap: ${TOP} ${DEP} ${SIM} tart_capture_tb.v
	$(IVC) $(OPT) -o $(OUT) $(TOP) $(DEP) $(SIM) tart_capture_tb.v


.PHONY: run
run: all
	@$(OUT)

.PHONY: dir
dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ $(OUT)
