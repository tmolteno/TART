# TART: Radio-telescope command line tools
    
This module provides command line tools for operating Transient Array Radio Telescope (TART). These tools are

* tart_calibrate
* tart_calibration_data
* tart_download_antenna_positions
* tart_upload_antenna_positions
* tart_download_data
* tart_download_gains
* tart_image
* tart_upload_gains
* tart_set_mode
* tart_vis2json


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

* Version 0.2.0b13. Clean up code to use other API endpoints.
                    Add CLI to --ignore some antennas
                    Add --n option to tart_download_data to stop after n downloads (used to grab the latest raw file)
                    Add CLI to use the influxdb
* Version 0.2.0b12. Fix bug in get-gains option.
* Version 0.2.0b11. Add a tart_set_mode binary.
* Version 0.2.0b9. Add a timeout to all HTTP requests.
* Version 0.2.0b8. Add a put method to the authorized api handler.
* Version 0.2.0b7. Fix typo in tart_calibrate that stopped calibration working.
* Version 0.2.0. New tart_download_data function.
* Version 0.1.5. Python3 compatability changes
