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
the device tree files the SDK generates for you.

Compiled versions of u-boot.img, boot.bin and linux's are in `boot`. 


