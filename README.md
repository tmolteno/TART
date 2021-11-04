# Transient Array Radio Telescope

## 'Next' branch

Extensive rework of the TART code repository.
 - Reimplementation of Flask telescope API in FastAPI
 - Retention policy
 - WASM in-browser imaging for TART vuer




[![DOI](https://zenodo.org/badge/20430511.svg)](https://zenodo.org/badge/latestdoi/20430511)


The [Transient Array Radio Telescope](https://en.wikipedia.org/wiki/Transient_Array_Radio_Telescope) (TART) is a low-cost 24-element aperture synthesis array
developed by the [Elec Research Group](http://elec.ac.nz) at the University of Otago

 ![All-sky image from a TART telescope][tart_image] 

The TART software and hardware designs are released as open-source designs and are
licensed under the GPL v3.

The authors of the first version of the TART are Tim Molteno, Charles Shaw, Max 
Scheel and Phil Brown. Enquries about the TART should be addressed to Tim Molteno (tim@elec.ac.nz). 

[Connect to our development radio telescope here](https://tart.elec.ac.nz "Online Telescopes")

## Hardware

The TART hardware is designed using a combination of KiCAD (newest boards) and
CadSoft's Eagle package. These designs are located in the [hardware](hardware/README.md) folder. 

## Firmware

The TART uses an FPGA to synchronize data from each receiver. The code for this in the 
Verilog language is in the [hardware/FPGA](hardware/FPGA/README.md) directory


## Installation

See the  [Installation Instructions](INSTALL.md) file for more detail on installation.

[tart_image]: https://github.com/tmolteno/TART/blob/master/doc/img/tart_image.jpg "TART All-Sky Image"


## Telescope API documentation

The [Telescope API documentation](https://tart.elec.ac.nz/doc/) describes how the TART is controlled via the network for configration and data retrieval. 
