# Using a Prebuilt Image
*TODO: Prebuilt images for various configurations*
Prebuilt images can be uploaded using the *Papilio Prog* Java program, which requires a Java runtime to be installed.

Assuming that `papilio-prog` is installed (see the relevant section below, for more information), and that the Papilio board is connected, then just:

> make upload

(when executed from the `tart_spi` directory) will upload the prebuilt image.


## Papilio Programmer Installation ##
On Debian-like systems:

> apt install java
> git clone https://github.com/GadgetFactory/Papilio-Loader
> cd pappy
> ./configure && make


### Programming the FPGA ###

Then the prebuilt TART image, `bin/tart.bit`, can be uploaded:

> papilio-prog -b bin/tart.bit -f SPARTAN6


### Papilio-Prog ###
See the instruction within the *Using a Prebuilt Image* section, at the beginning of this document.


## Initial Setup & Testing ##
When the TART hardware has been setup, installed, and connected correctly, then:

> ssh pi@my-tart.xxx
> cd TART/hardware/rpi/tart_cli
> sudo python low_level_dsp.py --status --verbose

where `my-tart.xxx` is the domain-name/IO-address of your TART-connected Raspberry Pi. The output of these commands should give the current status of the TART FPGA hardware, and the initial state has the capture frontend disabled, raw-data acquisition disabled, and the correlators disabled.
