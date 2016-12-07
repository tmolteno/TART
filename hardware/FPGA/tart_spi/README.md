# Description

The TART hardware uses a Xilinx Spartan 6 FPGA, from a Papilio Pro board, for a SPI-connected device that captures and processes antenna signals. It supports both raw-data aquisition, and real-time visibilities calculation.

When configured to use 24 antennae, the real-time correlators perform about 20 billion arithmetic operations (MAC's) per second.

# License

Copyright (C) 2016 Tim Molteno, Max Scheel, and Patrick Suggate.

**TODO license**

# TART Python Scripts

These are located within the folder `hardware/rpi/`, and contain the scripts:
* `tartdsp.py`        -- sets and queries the states for a DSP build of the TART hardware;
* `calibrate.py`      -- utilities for measuring the phase-delays of each of the antennae;
* `tart_testbench.py` -- tests the raw-data acquisition and read-back modes of the hardware;

**TODO scripts**

# TART Source

## Directories and Files

**TODO rewrite some of the scripts as Python scripts**

### Configuration
* script/              -- configuration generators;
* include/             -- configuration files to be included;
* papilio/             -- Xilinx implementation constraint files;
* data/                -- data-files for the current configuration;

The configuration (include) file for the TART synthesis options, `tartcfg.v`, contains a lot of settings, but only a few would typically be useful to change. Most of them are to ease the task of porting to another architecture, though this hasn't been done.

Settings:
* __512Mb_SDRAM     -- when the standard 64Mb SDRAM has been replaced with the larger one;
* __USE_ACQUISITION -- sometimes the hardware may be used for just raw-data acquisition;
* __USE_CORRELATORS -- or just for the calculation of visibilities, in real-time;
* __RELEASE_BUILD   -- possibly useful if some of the debugging & fake-data stuff isn't needed;

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
Some signals are have *_x* tags and/or suffixes, to indicate that they belong to the ~200 MHz (12x the XTAL clock) correlator clock-domain, and others have *_e* for the external crystal (16.368 MHz) clock-domain signals.  Most of the rest of the signals belong to the bus clock-domain, which is ~100 MHz (6x the XTAL clock).

### Wishbone(-like) Interfaces and Signals
Instead of calling the Wishbone *STALL_I* signal by its proper name, it's instead called *WAT_I*; i.e., WAiT-state.

Many Wishbone modules have parameters:
* *CHECK* which enables additional (standard) checking; e.g., for ignoring spurious signals like an *ACK_I* when *CYC_O* isn't asserted;
* *PIPED* for enabling (Wishbone SPEC B4) pipelined burst-mode transfers;
* *ASYNC* to reduce/eliminate synchronous delays for circuits that are fast enough (and/or the bus clock is slow enough) to not need them;

## Synthesis

All development and testing uses Xilinx ISE 14.7, and this is what we recommend that you use as well. Many Xilinx-specific primitives and constraints have been used to meet speed & area targets, and the synthesis software needs to support these.

The design uses some procedurally generated placement constraints to meet timing. An example timing report is the following:
![TART place-and-route timing report][synth]

The 24 hardware correlators are arranged in six blocks of four, and 16 of the correlators use the Spartan 6's DSP48A1 primitives, the rest use the carry-chain, and floor-planned to give the following circuit layout:
![TART place-and-route floorplan][floor]


[synth]: https://github.com/tmolteno/TART/blob/master/hardware/FPGA/tart_spi/doc/img/TART-timing.png "TART place-and-route timing report"
[floor]: https://github.com/tmolteno/TART/blob/master/hardware/FPGA/tart_spi/doc/img/TART.jpg "TART place-and-route floorplan"
