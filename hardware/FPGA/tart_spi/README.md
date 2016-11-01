# Description

A SPI-connected data-aquisition and DSP hardware design.

# License

Copyright (C) 2016 Tim Molteno, Max Scheel, and Patrick Suggate.

**TODO license**

# TART Source

## Directories and Files

**TODO rewrite some of the scripts as Python scripts**

### Configuration
* script/       -- configuration generators;
* include/      -- configuration files to be included;
* papilio/      -- Xilinx implementation constraint files;
* data/         -- data-files for the current configuration;

### TART Hardware Descriptions
* verilog/      -- most TART-specific code;
* verilog/spi/  -- SPI cores;

### TART Simulation
* bench/
* bench/spi/

### Additional Files
There is an additional library of simple Wishbone cores in the `wishbone` directory (outside of this project). This contains the subdirectories:
* rtl/    -- standard modules that communicate via a Wishbone-like interconnect;
* bench/  -- testbenches for the Wishbone modules;


## Naming Conventions

### Wishbone(-like) Interfaces and Signals
A name like *c_dtx* means that:
 * the *c* indicates that the naming is from the point-of-view of the "c" module, in this case the correlator-block; and
 * the *dtx* indicates that the signal carries data to the correlator, to be transmitted.

Instead of calling the Wishbone *STALL_I* signal by its proper name, it's instead called *WAT_I*; i.e., WAiT-state.

Many Wishbone modules have parameters:
* *CHECK* which enables additional (standard) checking; e.g., for ignoring spurious signals like an *ACK_I* when *CYC_O* isn't asserted;
* *PIPED* for enabling (Wishbone SPEC B4) pipelined burst-mode transfers;
* *ASYNC* to reduce/eliminate synchronous delays for circuits that are fast enough (and/or the bus clock is slow enough) to not need them;
