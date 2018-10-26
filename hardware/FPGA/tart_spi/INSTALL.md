# Using a Prebuilt Image
*TODO: Prebuilt images for various configurations*
Prebuilt images can be uploaded using the *Papilio Prog* Java program, which requires a Java runtime to be installed.

Assuming that `papilio-prog` is installed (see the relevant section below, for more information), and that the Papilio board is connected, then just:

> make upload

(when executed from the `tart_spi` directory) will upload the prebuilt image.


## Papilio Programmer Installation ##
On Debian-like systems:

> apt install java

### Installation from Source ###
The sources can be obtained from Github, [Papilio-Loader](https://github.com/GadgetFactory/Papilio-Loader), and cloned via:

> git clone https://github.com/GadgetFactory/Papilio-Loader

> cd pappy
> ./configure && make

### Windows Installation ###

Or a Windows version can be downloaded from [papilio-prog](https://www.pappy.org) , and install via:

> unzip pappy

### Programming the FPGA ###

Then the prebuilt TART image, `bin/tart.bit`, can be uploaded:

> papilio-prog -b bin/tart.bit -f SPARTAN6


# Full Installation Instructions #


## Prerequisites for Building ##


### Xilinx 14.7 ###
Obtain from Xilinx, and install such that the required paths have been setup:

> export XILINX_PATH=<path-to-Xilinx-ISE>


### Make ###
Any recent version should work correctly, for example *GNU Make 4.1* works.


### Simulation & Testbenches ###
Icarus Verilog is used for simulation from the command line, and this can be installed (on Debian-based systems) via:

> apt install iverilog gtkwave

The testbenches should probably also work using the Xilinx simulator, though this hasn't been tested.

To perform the various simulations, and referencing from the root directory of `TART/hardware/FPGA`, the makefiles and testbench files are typically within `bench` subdirectories.

For example, within the `tart_spi/bench` directory, to simulate a single correlator:

> make cor

for the entire DSP:

> make dsp

or just the SPI modules, from the `tart_spi/bench/spi` directory:

> make spi

And there are other testbenches for other subsystems, and the `wishbone` directory contains testbenches for many of the Wishbone compliant cores.

Some simulations generate `.vcd` files, and GtkWave was used during development to visualise simulation output. Some GtkWave project files, which end with `.gtkw`, are part of the TART repository, as they were used during testing & functional verification.


### Papilio-Prog ###
See the instruction within the *Using a Prebuilt Image* section, at the beginning of this document.


### GHC 8.0.1 (Optional) ###
If you really want to regenerate the floorplan and correlator-wiring files, then:

> cabal install turtle
> make scripts


### Python 2.7 ###
Communication with the TART hardware uses Python, and TART has been developed and tested using Python 2.7 ; e.g., *Python 2.7.13* is known to work correctly. Similar, and later 2.x versions of Python should also work, though Python 3 may or may-not work correctly.


## Building ##
Once the above prerequisites have been met, then:

> make build
> make upload

OR, alternatively, launch the Xilinx ISE application, and then navigate to and open the TART project file, `spartan6_spi.xise`. Once loaded, and assuming that a Spartan 6-based Papilio Pro is the synthesis target, then begin synthesis, and produce a *.bit* file.

Once this has finished (which should take around 5-10 minutes), then use `papilio-prog` to upload `tart.bit` to the Papilio device.


## Initial Setup & Testing ##
When the TART hardware has been setup, installed, and connected correctly, then:

> ssh pi@my-tart.xxx
> cd TART/hardware/rpi/tart_cli
> sudo python low_level_dsp.py --status --verbose

where `my-tart.xxx` is the domain-name/IO-address of your TART-connected Raspberry Pi. The output of these commands should give the current status of the TART FPGA hardware, and the initial state has the capture frontend disabled, raw-data acquisition disabled, and the correlators disabled.


## Web Server Setup ##
**TODO: Get Maximoose to help with this**
