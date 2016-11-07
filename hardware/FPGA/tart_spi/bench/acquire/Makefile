IVC=iverilog
INC=../../include/
OPT=-D__icarus -Wall -I$(INC)

OUT=../../build

# Source directories:
RTL =../../verilog
ACQ =$(RTL)/acquire
LIB =$(RTL)/fifo/afifo_gray.v $(RTL)/mfsr/mfsr32.v

# Simulation-only files:
XIL =xilinx
SIM =signal_stagger.v
DEP =${ACQ}/signal_capture.v ${ACQ}/align_captures.v
TOP =${LIB} ${ACQ}/tart_capture.v ${ACQ}/capture_control.v ${ACQ}/capture.v ${ACQ}/fake_telescope.v ${ACQ}/delay_data_sampling_clk.v ${ACQ}/block_buffer.v ${ACQ}/fifo_control.v


all: acq

sig: ${DEP} ${SIM} signal_capture_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) $(SIM) signal_capture_tb.v

cap: ${DEP} ${SIM} align_captures_tb.v
	$(IVC) $(OPT) -o $(OUT)/tart.out $(DEP) $(SIM) align_captures_tb.v

acq: ${TOP} ${DEP} ${SIM} tart_capture_tb.v
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
