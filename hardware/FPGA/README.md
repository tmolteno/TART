# TART FPGA Overview

The FPGA synchronizes all the incoming radio signals, buffers them and exposes an SPI interface to the host computer to retrieve raw data,
visibilities and also to configure the system.

## Real-Time correlator

The FPGA also acts as a real-time correlator, calculating visibilities on-the-fly.


# Top-Level of the TART Modules and Cores
This directory is just the top-level directory for various TART modules & libraries.

To build the TART FPGA image, the project files, documentation, and sources are within  the `tart_spi` subdirectory.


## TART SPI
The `tart_spi` project contains the design, testbenches, and a FPGA implementation of TART's data-acquisition and visibilities-calculation hardware.

The `tart_spi/bench` directory contains testbenches for many of the TART cores, to be simulated using *Icarus Verilog*, and output can be visualised using *GtkWave*, for example.


## Wishbone
These logic cores, within `wishbone/rtl` use Wishbone SPEC B4, and are simple implementations for basic functionality like interfacing to SRAM's, or transferring blocks of data between different components.

The `wishbone/bench` directory contains testbenches for the Wishbone cores, to be simulated using *Icarus Verilog*, and output can be visualised using *GtkWave*, for example.


## Unused Cores
The remaining directories, `fifo`, `ddr_controller`, and `ddrmem` are not currently used (and perhaps should be removed).
