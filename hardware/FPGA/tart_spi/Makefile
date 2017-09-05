##--------------------------------------------------------------------------##
##
##  Makefile for command line synthesis, using Xilinx ISE tools.
##
##  Copyright:
##    Roy Ward, 2006
##    Tim Molteno, 2006-2017
##    Patrick Suggate, 2007-2017
##    Max Scheel, 2017
##
##  NOTE:
##   + currently, the command-line options, for the various ISE tools, are
##     for ISE 14.7 ;
##   + `make clean` can be a little dangerous;
##
##  TODO:
##   + needs a different floorplan when correlators are not used;
##   + improve the `upload`, `images`, and `clean` Makefile targets;
##
##--------------------------------------------------------------------------##

SHELL=/bin/bash

standard: build upload

# TODO: use `papilio-prog`
upload: program

# Build the various prebuilt images
# TODO: build for several configurations
images: build

all: pairs permute floor build upload

.PHONY: pairs
pairs:
	runhaskell script/pairs.hs --antennae=24 --blocksize=6 --multiplex=12 --outfile include/tart_pairs.v

.PHONY: permute
permute:
	runhaskell script/pairs.hs --antennae=24 --blocksize=6 --multiplex=12 --permute --outfile data/permute.txt

.PHONY: hex
hex:
	runhaskell script/pairs.hs --antennae=24 --blocksize=6 --multiplex=12 --hex --verbose --outfile data/permute.hex

.PHONY: floor
floor:
	runhaskell script/floor.hs --numDSP=4 --prefix="DSP/COR/CXB" --outfile papilio/floor.ucf



##--------------------------------------------------------------------------##
##
##  XILINX SYNTHESIS STUFF.
##
##--------------------------------------------------------------------------##

# Provide some defaults for the user settings
TOP_MODULE=$(shell echo ${XFILES} | cut -d\. -f1)
PINOUT_STEM=${TOP_MODULE}
PINOUT=${PINOUT_STEM}-$(shell echo ${TARGET} | cut -d- -f1,2).ucf
#########################################################
# User settings start here
#########################################################

# Target as part_num-package-speed_grade. USE LOWER CASE
#  Papilio
TARGET=xc6slx9-tqg144-2

# TARGET=xcr3128xl-vq100-10
# TARGET=xc2s30-tq144-5
# TARGET=xc3s1000-ft256-4
# TARGET=xc3s200-pq208-4
# TARGET=xc5vlx30-ff324-3
# TARGET=xc2v3000-ff1152-6

# export XILINX=/usr/local/Xilinx
export XILINX=/mnt/bucket/Xilinx/14.7/ISE_DS/ISE/bin/lin64


##--------------------------------------------------------------------------##
##  Synthesis files.
##--------------------------------------------------------------------------##
DIR=$(shell pwd)
RTL=${DIR}/verilog
UCF=${DIR}/papilio
INC=${DIR}/include

LIB_FILES=${RTL}/fifo/afifo_gray.v ${RTL}/mfsr/mfsr32.v ${RTL}/misc/shift_reg.v

WB4=${DIR}/../wishbone/rtl
WB4_FILES=${WB4}/wb_transfer.v ${WB4}/wb_cycle.v ${WB4}/wb_sram_port.v ${WB4}/wb_sram_prefetch.v ${WB4}/wb_fetch.v ${WB4}/wb_sram_stream.v ${WB4}/wb_reset.v

XIL=${RTL}/xilinx
XIL_FILES=${XIL}/MUX8.v ${XIL}/RAM32X6_SDP.v ${XIL}/RAMB8X32_SDP.v ${XIL}/RAMB16X16X4_TDP.v

SPI=${RTL}/spi
SPI_FILES=${SPI}/spi_slave_wb.v ${SPI}/spi_layer.v

CAP=${RTL}/capture
CAP_FILES=${CAP}/tart_capture.v ${CAP}/signal_phase_DDR.v ${CAP}/signal_source.v ${CAP}/fake_telescope.v

ACQ=${RTL}/acquire
ACQ_FILES=${ACQ}/tart_acquire.v ${ACQ}/raw_acquire.v ${ACQ}/dram_prefetch.v ${ACQ}/fifo_control.v ${ACQ}/block_buffer.v

DSP=${RTL}/correlator
DSP_FILES=${DSP}/tart_dsp.v ${DSP}/tart_correlator.v ${DSP}/correlator_block.v ${DSP}/tart_visibilities.v ${DSP}/control.v ${DSP}/block_DSP.v ${DSP}/block_SDP.v ${DSP}/correlate_cos_sin.v ${DSP}/correlate_cos_sin_DSP.v ${DSP}/correlator_DSP.v ${DSP}/correlator_SDP.v ${DSP}/fake_hilbert.v ${DSP}/rmw_address_unit.v ${DSP}/bank_switch.v


##--------------------------------------------------------------------------##
##  Simulation files.
##--------------------------------------------------------------------------##
SIM=${DIR}/bench


##--------------------------------------------------------------------------##
##  File & top-level groups.
##--------------------------------------------------------------------------##
COMMON_FILES=${RTL}/tart.v ${RTL}/tart_dual_dcm.v ${RTL}/tart_wishbone.v ${RTL}/tart_control.v ${RTL}/SDRAM_Controller_v.v ${LIB_FILES} ${XIL_FILES} ${WB4_FILES} ${SPI_FILES} ${CAP_FILES} ${ACQ_FILES} ${DSP_FILES}

XFILES=${COMMON_FILES}
IFILES=${COMMON_FILES} ${SIM}/correlator/tart_dsp_tb.v

# (optional) Name of top module. Defaults to name of first file with suffix removed
TOP_MODULE=tart
ICARUS_TOP_MODULE=tart_dsp_tb


# (optional) List of modules for a show resources report (FPGA only)
RSRC_REPORT=

# (optional) Name of Contraints file. Defaults to PINOUT_STEM-PART-PACKAGE.ucf
PINOUT=${UCF}/constraints.ucf
FLOOR =${UCF}/floor.ucf

# Place to stick all Xilinx-generated files
BUILD =${DIR}/build

# (optional) Name of PINOUT_STEM. Defaults to TOP_MODULE
#PINOUT_STEM=testing

# whether to require matching of all the paramaters in the .ucf file true/false
# WARNING: if this is false, be careful of spelling errors ... they won't appear as warnings
#STRICT_UCF=false

# opt level is 1,2,3 or 4. 4 will take some time (maybe hours). 5 is just insane. Default is 1
OPT_LEVEL=4

# opt mode is one of speed or area
# OPT_MODE=area
OPT_MODE=speed

# quiet mode - true or false.
QUIET_MODE=false

#########################################################
# Advanced user settings
#########################################################

# Both FPGA and CPLD
XST_EXTRAS=-vlgincdir ${INC}
NGDBUILD_EXTRAS=-uc ${FLOOR}

# FPGA Only
# MAP_EXTRAS= -r -timing -cm ${OPT_MODE}
MAP_EXTRAS= -w -r 4 -timing -mt 2 -logic_opt on -global_opt ${OPT_MODE}
PAR_EXTRAS=
BITGEN_EXTRAS=

# CPLD only
CPLDFIT_XTRAS=
HPREP6_EXTRAS=

ICARUS_OPTIONS=

#########################################################
# User settings end here
#########################################################

# Internal settings. Probably no need to tamper with these
X_SETTINGS = ${XILINX}/../../../settings64.sh
X_PREFIX   = _xilinx_int
X_LABEL    = ${BUILD}/${X_PREFIX}

PART=$(shell echo ${TARGET} | cut -d- -f1)
PACKAGE=$(shell echo ${TARGET} | cut -d- -f2)
SPEED_GRADE=$(shell echo ${TARGET} | cut -d- -f3)
FFILES=${X_LABEL}_target.v ${XFILES}

ifneq '$(filter xc9%,${PART})' ''
PART_TYPE=cpld
else
ifneq '$(filter xcr3%,${PART})' ''
PART_TYPE=cpld
else
ifneq '$(filter xc2c%,${PART})' ''
PART_TYPE=cpld
else
ifneq '$(filter xc2s%,${PART})' ''
PART_TYPE=fpga
else
ifneq '$(filter xc3s%,${PART})' ''
PART_TYPE=fpga
else
ifneq '$(filter xc2v%,${PART})' ''
PART_TYPE=fpga
else
ifneq '$(filter xc6s%,${PART})' ''
PART_TYPE=fpga
else
PART_TYPE=unknown
endif
endif
endif
endif
endif
endif
endif

ifeq '$(OPT_LEVEL)' '2'
XST_OPT_LEVEL=2
CPLD_EXHAUST=
PAR_OPT=-ol med
else
ifeq '$(OPT_LEVEL)' '3'
XST_OPT_LEVEL=2
CPLD_EXHAUST=
PAR_OPT=-ol high
else
ifeq '$(OPT_LEVEL)' '4'
XST_OPT_LEVEL=2
CPLD_EXHAUST=-exhaust
PAR_OPT=-ol high -xe n
else
ifeq '$(OPT_LEVEL)' '5'
XST_OPT_LEVEL=2
CPLD_EXHAUST=-exhaust
PAR_OPT=-ol high -xe c
else
#assume optlevel 1 (lowest)
XST_OPT_LEVEL=1
CPLD_EXHAUST=
PAR_OPT=-ol std
endif
endif
endif
endif

ifeq '${STRICT_UCF}' 'TRUE'
AUL=
else
ifeq '${STRICT_UCF}' 'true'
AUL=
else
ifeq '${STRICT_UCF}' '1'
AUL=
else
AUL=-aul
endif
endif
endif

ifeq '${QUIET_MODE}' 'true'
SILENT_INT=-intstyle silent
MAP_QUIET=-quiet
else
SILENT_INT=
MAP_QUIET=
endif

ifeq '${PART_TYPE}' 'cpld'
PROG_EXT=jed
else
PROG_EXT=bit
endif

prom:
	${XILINX}/promgen -w -u 0 ${X_PREFIX}.bit -p mcs


# .PHONY: clean
clean:
	@rm -rf demo.s
	@rm -rf ${X_LABEL}_dir ${X_LABEL}.* ${X_PREFIX}.* ${BUILD}/_* *.srp *.lso tmperr.err icarus.out
	@md5sum Makefile > make.md5

clean_test:
ifneq '$(shell cat make.md5)' '$(shell md5sum Makefile)'
	@echo 'Makefile changed: making clean'
	make clean
else
endif

test:
	echo ${TEST}

#########################################################

icarus: clean_test ${XFILES}
	iverilog -D__icarus -Wall -o icarus.out -s ${ICARUS_TOP_MODULE} ${ICARUS_OPTIONS} ${IFILES}

#########################################################
# Used both for CPLD and FPGA

${X_LABEL}.prj : 
	@echo '`define __${PART}_${PACKAGE} 1'  > ${X_LABEL}_target.v
	@echo '`define __xilinx 1'  >>  ${X_LABEL}_target.v
	@rm -f ${X_LABEL}.prj
	@touch ${X_LABEL}.prj
	@for x in $(filter %.v,${FFILES}); do echo "verilog work $$x" >> ${X_LABEL}.prj; done
	@for x in $(filter %.vhd,${FFILES}); do echo "vhdl work $$x" >> ${X_LABEL}.prj; done
	@mkdir -p ${X_LABEL}_dir/xst

xst: clean_test ${FFILES} ${X_LABEL}.ngc

${X_LABEL}.ngc : ${X_LABEL}.prj ${FFILES} ${PINOUT}
	@echo 'set -tmpdir ${X_LABEL}_dir/xst -xsthdpdir ${X_LABEL}_dir/xst'> ${X_LABEL}.scr
	@echo 'run -ifn ${X_LABEL}.prj -ifmt mixed -top $(TOP_MODULE) -ofn ${X_LABEL}.ngc -p ${PART}-${PACKAGE}-${SPEED_GRADE}' >> ${X_LABEL}.scr
	@echo '-opt_mode ${OPT_MODE} -opt_level ${XST_OPT_LEVEL}' ${XST_EXTRAS} >> ${X_LABEL}.scr
	. ${X_SETTINGS} ; xst -ifn ${X_LABEL}.scr

${X_LABEL}_%.srp: ${X_LABEL}.prj ${FFILES} ${PINOUT}
	@echo 'set -tmpdir ${X_LABEL}_dir/xst -xsthdpdir ${X_LABEL}_dir/xst'> ${X_LABEL}_$(*F).scr
	@echo 'run -ifn ${X_LABEL}.prj -ifmt mixed -top $(*F) -ofn ${X_LABEL}_$(*F).ngc -p ${PART}-${PACKAGE}-${SPEED_GRADE}' >> ${X_LABEL}_$(*F).scr
	@echo '-opt_mode ${OPT_MODE} -opt_level ${XST_OPT_LEVEL}' ${XST_EXTRAS} >> ${X_LABEL}_$(*F).scr
	. ${X_SETTINGS} ; xst -ifn ${X_LABEL}_$(*F).scr

resources: $(patsubst %,${X_LABEL}_%.srp,${RSRC_REPORT})
	@for x in ${RSRC_REPORT}; do echo MODULE: $$x ; head -$$[15+`grep -n "Device utilization summary" $(patsubst %,${X_LABEL}_%.srp,$$x) | grep -v ")" | cut -d: -f1`] $(patsubst %,${X_LABEL}_%.srp,$$x) | tail -15 | grep "Number of" | grep -v IOBs | grep -v GCLKs ; echo "" ; done

${X_LABEL}.ngd : ${X_LABEL}.ngc
	@mkdir -p ${X_LABEL}_dir/xst
	. ${X_SETTINGS} ; ngdbuild -dd ${X_LABEL}_dir ${AUL} ${NGDBUILD_EXTRAS} ${SILENT_INT} -uc ${PINOUT} ${X_LABEL}.ngc
	@mv ${X_PREFIX}.ngd ${X_LABEL}.ngd

build: clean_test ${X_LABEL}.${PROG_EXT}

program: clean_test clean_test build
	. ${X_SETTINGS} ; impact -batch impact_${PART_TYPE}.cmd


#########################################################
# CPLD only

ifeq '${OPT_MODE}' 'area'
CPLD_OPT_MODE=density
else
CPLD_OPT_MODE=speed
endif

${X_PREFIX}.vm6 : ${X_PREFIX}.ngd
	. ${X_SETTINGS} ; cpldfit -p ${PART}-${SPEED_GRADE}-${PACKAGE} -optimize ${CPLDFIT_XTRAS} ${CPLD_OPT_MODE} ${CPLD_EXHAUST} ${X_PREFIX}.ngd

${X_PREFIX}.jed : ${X_PREFIX}.vm6
	. ${X_SETTINGS} ; hprep6 -i ${X_PREFIX}.vm6 ${SILENT_INT} ${HPREP6_EXTRAS}


#########################################################
# FPGA only

map: ${X_LABEL}0.ncd

${X_LABEL}0.ncd : ${X_LABEL}.ngd
	@cp ${X_LABEL}.ngd ${X_LABEL}0.ngd
	. ${X_SETTINGS} ; map ${MAP_QUIET} ${MAP_EXTRAS} -ol high ${X_LABEL}0.ngd ${X_LABEL}0.pcf

par: ${X_LABEL}.ncd

${X_LABEL}.ncd :${X_LABEL}0.ncd
	. ${X_SETTINGS} ; par -w ${PAR_OPT} ${PAR_EXTRAS} ${X_LABEL}0.ncd ${X_LABEL}.ncd
#	. ${X_SETTINGS} ; par -n 5 -pl high -rl std  ${X_LABEL}0.ncd ${X_LABEL}.dir
	. ${X_SETTINGS} ; trce -a ${SILENT_INT} ${X_LABEL}.ncd ${X_LABEL}0.pcf
	@mv ${X_PREFIX}.twr ${X_LABEL}.twr
	tail -18 ${X_LABEL}.twr

timing: ${X_LABEL}.ncd
	tail -18 ${X_LABEL}.twr

bitgen: ${X_LABEL}.bit

${X_LABEL}.bit : ${X_LABEL}.ncd
	. ${X_SETTINGS} ; bitgen -w ${SILENT_INT} ${BITGEN_EXTRAS} ${X_LABEL}.ncd
