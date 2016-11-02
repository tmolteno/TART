IVC=iverilog
INC=../../include/
OPT=-D__icarus -Wall -I$(INC)

OUT=../../build

# Source directories:
RTL =../../verilog
ACQ =$(RTL)/acquire

# Simulation-only files:
XIL =xilinx
SIM =signal_stagger.v
DEP =${ACQ}/signal_capture.v ${ACQ}/align_captures.v


all: cap

sig: ${DEP} signal_capture_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) $(SIM) signal_capture_tb.v

cap: ${DEP} align_captures_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) $(SIM) align_captures_tb.v


.PHONY: run
run: all
	@$(OUT)/tart.out

.PHONY: dir
dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ $(OUT)/tart.out
