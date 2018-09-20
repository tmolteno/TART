# Transient Array Radio Telescope

The Transient Array Radio Telescope (TART) is a low-cost 24-element aperture synthesis array
developed by the [Elec Research Group](http://elec.ac.nz) at the University of Otago

 ![All-sky image from a TART telescope][tart_image] 

The TART software and hardware designs are released as open-source designs and are
licensed under the GPL v3.

The authors of the first version of the TART are Tim Molteno, Charles Shaw, Max 
Scheel and Phil Brown. Enquries about the TART should be addressed to Tim Molteno (tim@elec.ac.nz). 

[Connect to our development radio telescope here](https://tart.elec.ac.nz "Online Telescopes")

## Hardware

The TART hardware is designed using a combination of KiCAD (newest boards) and
CadSoft's Eagle package. These designs are located in the hardware folder.

## Firmware

The TART uses an FPGA to synchronize data from each receiver. The code for this in the 
Verilog language is in the hardware/FPGA directory

## Software 

Software for operation is predominantly written in python and contained in the software
directory. Software containers are used to simplify installation.

The telescope operation is controlled by a RESTful API. There is a web-based front end that can be used to
monitor telescope operation.

[tart_image]: https://github.com/tmolteno/TART/blob/master/doc/img/tart_image.jpg "TART All-Sky Image"
