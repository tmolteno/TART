# Description

The TART hardware uses a Xilinx Spartan 6 FPGA, from a Papilio Pro board, for a SPI-connected device that captures and performs real-time processing of signals from an antenna array. Additionally, it supports raw-data aquisition and buffering (using the on-board SDRAM), for offline visibilities calculations.

When configured to use 24 antennae, the real-time correlators perform nearly 20 billion arithmetic operations per second.


# License

Copyright (C) 2016 Tim Molteno, Max Scheel, and Patrick Suggate.

**TODO license**
GPL3, LGPL3 ??


# Design, Sources, and using the TART Hardware #
NOTE: Installation instructions can be found within _INSTALL.md_ .


## Using TART Hardware ##
**TODO update the list of scripts**
Python scripts are used to configure, and exchange data with the TART hardware. These are located within the subdirectory `hardware/rpi/tart_cli` (of the *TART* project root), and contain the following scripts:
* `low_level_dsp.py`        -- sets and queries the states for a DSP build of the TART hardware;
* `calibrate.py`      -- utilities for measuring the phase-delays of each of the antennae;
* `tart_testbench.py` -- tests the raw-data acquisition and read-back modes of the hardware;

**TODO scripts**

To query the current state of the (connected) TART hardware, first connect to the Raspberry Pi via SSH:

> ssh pi@tart-name

and then the FPGA can be interrogated via:

> cd TART/hardware/rpi/tart_cli
> sudo python low_level_dsp.py --status --verbos

or, for further options:

> sudo python low_level_dsp.py --help


## Simulation & Testbenches ##

Additionally, there are numerous testbenches for TART's FPGA-based subsystems, and typically reside with `bench` subdirectories. The testbench's associated `Makefile` expects that the Icarus Verilog simulator is installed.

GtkWave can be used to visualise the simulation output, and some `.gtkw` files are included, as these were used during development.


# Architecture #

The TART correlators consist of 24 correlators, arranged in six blocks. Each correlator performs two 1-bit correlations, and each of these is then accumulated using a 24-bit accumulator. The correlators operate at 12 times the signal-acquisition clock frequency, which is about 196 MHz, by default.

ASCII art schematics can be found in some of the higher-level modules (within the `verilog` subdirectory):
* `tart.v`
* `capture/tart_capture.v`
* `correlator/tart_dsp.v`
and these schematics give the high-level structure of the design.


## Wishbone Interconnects ##

On-chip communication between cores typically uses Wishbone interconnects (and usually SPEC B4 compliant). There are two Wishbone domains:
* 98.208 MHz for the system-wide bus; and
* 196.416 MHz for transferring the data from the correlators to a Wishbone bridge.

Wishbone (SPEC B4) was used to standardise communication between cores, to improve modularity and extensibility, and without sacrificing performance. Key to this was support for pipelined burst-mode transfers, and that Wishbone (in general) is a fairly simple protocol with only very little additional logic overhead (versus typical custom/ad-hoc interconnects).

The full Wishbone SPEC B4 specification can be obtained from:

https://opencores.org/cdn/downloads/wbspec_b4.pdf


## SPI Interface ##

Communication between the *Raspberry Pi* and the *Papilio Pro* uses the *SPI* interconnect, and with the RPi as the master. The slave SPI logic core (within the Papilio) uses asynchronous FIFO's for clock-domain crossing, and a source-synchronous configuration. This allows the SPI core to operate at around 100 MHz (with Spartan 6 IOB's), though it is limited to 32 MHz for the current version of TART (as any faster would require additional "padding" bytes).

The slave uses signalling levels that are compatible with a MODE1 (POL = 0, PHA = 1) SPI master, and the data bit-width is set to 8-bits.


### Future Work ###

The Tx & Rx FIFO's should be changed to a smaller, simple design. This should also (slightly) reduce latency, but this has yet to be implemented & tested.


## System Modules ##

### TART Clocks ###
The TART antenna use an external 16.368 MHz clock, along with additional jitter-cleaner circuitry.

There are three internal clocks that are used


## Capture Frontend ##
Features clock-recovery, and 12x oversampling (using DDR latches, and a 6x clock).

### Registers ###
The DSP unit has the following SPI-mapped registers:
**TODO: dsp regs**


## Raw-Data Acquisition Unit ##

### Registers ###
The acquisition unit has the following SPI-mapped registers:
**TODO: acq regs**


## Real-Time Visibilities Calculator ##

### Registers ###
The DSP unit has the following SPI-mapped registers:
**TODO: dsp regs**


### Future Changes ###
Currently the design of the correlators uses excessive accumulator resources. By using narrower bit-width accumulators within the first "layer" of correlators, the area of these can be reduced. To give adequate window-size, a second "layer" of accumulators, and of greater bit-width, can be used to accumulate larger visibilities totals. This would reduce logic area because fewer of these layer-two accumulators would be needed.


## Directories and Files

**TODO rewrite some of the Haskell scripts as Python scripts**


### Configuration
* script/              -- configuration generators;
* include/             -- configuration files to be included;
* papilio/             -- Xilinx implementation constraint files;
* data/                -- data-files for the current configuration;

The configuration (include) file for the TART synthesis options, `tartcfg.v`, contains a lot of settings, but only a few would typically be useful to change. Most of them are to ease the task of porting to another architecture, though this hasn't been done.

Settings:
* `__512Mb_SDRAM`     -- when using a 512Mb (vs. the standard 64Mb) SDRAM;
* `__USE_ACQUISITION` -- the hardware may be used for just raw-data acquisition;
* `__USE_CORRELATORS` -- or just for the calculation of visibilities, in real-time;
* `__RELEASE_BUILD`   -- possibly useful if some of the debugging & fake-data stuff isn't needed;

#### Implementation Constraints ####
The default implementation constraints file is `papilio/papilio.ucf`, which contains the pinout and timing constraints for the *Papilio Pro* (Sparan 6-based) FPGA development board.

#### Scripts ####
Currently these are Haskell scripts, and can be evaluated once *GHC* (version 8.0.1 has been tested to work), and a few additional libraries (`turtle` and `bits`) have been installed.


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
NOTE: Installation instructions can be found within _INSTALL.md_ .

All development and testing uses Xilinx ISE 14.7, and this is what we recommend that you use as well. Many Xilinx-specific primitives and constraints have been used to meet speed & area targets, and the synthesis software needs to support these.

The design uses some procedurally generated placement constraints to meet timing (see the *scripts* section above). An example timing report is the following:
![TART place-and-route timing report][synth]

The 24 hardware correlators are arranged in six blocks of four, and 16 of the correlators use the Spartan 6's DSP48A1 primitives, the rest use the carry-chain, and floor-planned to give the following circuit layout:
![TART place-and-route floorplan][floor]


[synth]: https://github.com/tmolteno/TART/blob/master/hardware/FPGA/tart_spi/doc/img/TART-timing.png "TART place-and-route timing report"
[floor]: https://github.com/tmolteno/TART/blob/master/hardware/FPGA/tart_spi/doc/img/TART.png "TART place-and-route floorplan"
