# Transient Array Radio Telescope

The Transient Array Radio Telescope (TART) is a low-cost aperture synthesis array
developed by the elec research group at the University of Otago

[http://elec.otago.ac.nz/w/index.php/TART]

The tart software and hardware designs are released as open-source designs and are
licensed under the GPL v3.

The authors of the first version of the TART are Tim Molteno, Charles Shaw and Max 
Scheel. Enquries about the TART should be addressed to Tim Molteno (tim@elec.ac.nz). 

## Hardware

The TART hardware is designed using a combination of KiCAD (newest boards) and
CadSoft's Eagle package. These designs are located in the hardware folder.

## Firmware

The TART uses an FPGA to synchronize data from each receiver. The code for this in the 
Verilog language is in the hardware/papilio/tart_spi/verilog directory

## Software 


Software for operation is predominantly written in python and contained in the software/python
directory. This installs the tart python package that contains all the necessary code.

