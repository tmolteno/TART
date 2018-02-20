# TART Zynq IP and Kernel Module

## Installation

To get started you need to recreate the project. Open Vivado, then 
open a tcl console with Window->TCL Console. `cd` to this directory
then: 

```
cd path/to/here
source project.tcl
```

Once the project opens you need to generate a bitstream and create
a device tree blob. Follow the 
[Linux Installation](docs/install_linux.md) on creating a device
tree blob.

The Makefile in this directory will create a device tree blob from 
the device tree files the SDK generates for you when following the
instruction above.

Compiled versions of u-boot.img, boot.bin and linux's are in `boot`. 

### Kernel

In the `kernel` directory there is a kernel module. You will need 
to edit the Makefile so KERNEL_DIR points to a copy of the linux 
kernel source code. Then run `make` and copy tart.ko to the zynq
board. 

