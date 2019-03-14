# TART: Radio-telescope command line tools
    
This module provides command line tools for operating Transient Array Radio Telescope (TART). These tools are

* tart_calibrate
* tart_calibration_data
* tart_download_antenna_positions
* tart_download_data
* tart_download_gains
* tart_image
* tart_upload_gains


To generate an image from a telescope, try the following command which should display the current view from a telescope
on top of Signal-Hill near Dunedin New Zealand.

    tart_image --api https://tart.elec.ac.nz/signal --display

For more information see the [TART Github repository](https://github.com/tmolteno/TART)

## Install Instructions

tart_tools is available from standard python package repositories. Try:

    pip3 install tart_tools


## Authors

* Tim Molteno (tim@elec.ac.nz)
* Max Scheel (max@max.ac.nz)

## Development work
    
If you are developing this package, this should be installed using
```
	make develop
```
in which case changes to the source-code will be immediately available to projects using it.

    
## NEWS

* Version 0.1.5. Python3 compatability changes
* Version 0.2.0. New tart_download_data function.
