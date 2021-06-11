# TART Install Guide

A TART telescope consists of some telescope hardware connected to a Raspberry Pi. Installing your own TART telescope requires several steps:

* Building and the telescope hardware
* Installing the TART software on the Raspberry Pi
* Connecting to the TART via web interface
* Radio Testing
* Set up radio receiver array. This is typically done in a reasonably radio-quiet place.
* Calibrating the telescope.

## Building and testing the radio hardware

If you are interested in building the telescope contact tim@elec.ac.nz, as we have printed circuit boards, and other components that we are willing to give to anyone interested in deploying their own telescope.

Hardware documentation can be found in [hardware/README.md](hardware/README.md).


## Installing Software

The telescope software runs on three different computers. 

* A Raspberry Pi (Model 3 or 4) which is plugged into the TART basestation board. This runs the telescope web API server.
* A calibration server which is a fast desktop that runs a calibration routine at regular intervals (every few hours)
* An optional object position server. This is a server that provides a catalog of known objects and their elevation/azimuth for any point on earth. A public one is available so you'll only need to provide your own server if you're running a process that requires low-latency access to this information.

Information on installation of software can be found in [software/README.md](software/README.md).

## Radio Testing

Radio testing should be performed after the software has been installed on the raspberry pi, and verification is usually done through the web interface.


## Set up telescope

Once the telescope can be accessed via its web interface, the telescope hardware should be put out in a suitable location. Then the calibration procedure should be followed. This requires two steps that are outlined in the [calibration documentation](doc/calibration/README.md).

## Other Useful Things

* Using the web_api from remote computers to gather data.
