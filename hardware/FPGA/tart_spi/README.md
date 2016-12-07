# Description

The TART hardware uses a Xilinx Spartan 6 FPGA, from a Papilio Pro board, for a SPI-connected device that captures and processes antenna signals. It supports both raw-data aquisition, and real-time visibilities calculation.

# License

Copyright (C) 2016 Tim Molteno, Max Scheel, and Patrick Suggate.

**TODO license**

# TART Source

## Directories and Files

**TODO rewrite some of the scripts as Python scripts**

### Configuration
* script/              -- configuration generators;
* include/             -- configuration files to be included;
* papilio/             -- Xilinx implementation constraint files;
* data/                -- data-files for the current configuration;

### TART Hardware Descriptions
* verilog/             -- most TART-specific code;
* verilog/capture/     -- cores for the raw-signal clock-recovery, and data-capture;
* verilog/acquire/     -- raw-data buffering and read-back logic cores;
* verilog/correlator/  -- cores for correlating, data-management, and (fake) Hilbert transforming;
* verilog/spi/         -- SPI cores;

### TART Simulation
* bench/               -- top-level testbenches;
* bench/capture/       -- clock-recovery and data-capture testing;
* bench/acquire/       -- testbenches for the raw-data acquisition functionality;
* bench/correlator/    -- some visibilities/DSP-related testbenches;
* bench/spi/           -- SPI-related testbenches;

### Additional Files
There is an additional library of simple Wishbone cores in the `wishbone` directory (outside of this project). This contains the subdirectories:
* rtl/    -- standard modules that communicate via a Wishbone-like interconnect;
* bench/  -- testbenches for the Wishbone modules;


## Naming Conventions

### Wishbone(-like) Interfaces and Signals
Instead of calling the Wishbone *STALL_I* signal by its proper name, it's instead called *WAT_I*; i.e., WAiT-state.

Many Wishbone modules have parameters:
* *CHECK* which enables additional (standard) checking; e.g., for ignoring spurious signals like an *ACK_I* when *CYC_O* isn't asserted;
* *PIPED* for enabling (Wishbone SPEC B4) pipelined burst-mode transfers;
* *ASYNC* to reduce/eliminate synchronous delays for circuits that are fast enough (and/or the bus clock is slow enough) to not need them;
