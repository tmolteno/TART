# Description

A SPI-connected data-aquisition and DSP hardware design.

# License

Copyright (C) 2016 Tim Molteno, Max Scheel, and Patrick Suggate.

**TODO license**

# TART Source

## Naming Conventions

### Wishbone(-like) Interfaces and Signals
A name like *c_dtx* means that:
 * the *c* indicates that the naming is from the point-of-view of the "c" module, in this case the correlator-block; and
 * the *dtx* indicates that the signal carries data to the correlator, to be transmitted.

## Directories

### Configuration
* script/       -- configuration generators;
* include/      -- configuration files to be included;
* ucf/          -- Xilinx implementation constraint files;

### TART Hardware Descriptions
* verilog/      -- most TART-specific code;
* verilog/bus/  -- standard modules that communicate via a Wishbone-like interconnect;
* verilog/spi/  -- SPI cores;

### TART Simulation
* bench/
* bench/bus/
* bench/spi/
