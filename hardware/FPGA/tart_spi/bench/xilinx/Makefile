IVC=iverilog
INC=../../include/
OPT=-D__icarus -I$(INC)

OUT=../../build

# Source directories:
RTL=../verilog

DEP=RAMB16_S9_S36.v

all: ${DEP} RAMB16_S9_S36_tb.v
	$(IVC) $(OPT) -o $(OUT)/ramb.out $(DEP) RAMB16_S9_S36_tb.v

.PHONY: run
run: all
	@$(OUT)/ramb.out

dir:
	@mkdir $(OUT)

.PHONY: clean
clean:
	rm -f *~ $(OUT)/ramb.out
