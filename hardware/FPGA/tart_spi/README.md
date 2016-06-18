# Description

A SPI-connected data-aquisition and DSP hardware design.

# License

Copyright (C) 2016 Tim Molteno, Max Scheel, and Patrick Suggate.

**TODO license**

# Directories

## Configuration
* script/       -- configuration generators;
* include/      -- configuration files to be included;
* ucf/          -- Xilinx implementation constraint files;

## TART Hardware Descriptions
* verilog/      -- most TART-specific code;
* verilog/bus/  -- standard modules that communicate via a Wishbone-like interconnect;
* verilog/spi/  -- SPI cores;

## TART Simulation
* bench/
* bench/bus/
* bench/spi/
