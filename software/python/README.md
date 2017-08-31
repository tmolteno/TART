# Python packages for the TART radio telescope

There are three python packages that are used for various parts of the TART telescope. The low-level hardware interface is done via an SPI bus, and the tart_hardware_interface package is used to provide a python library to access this hardware.

The tart_web_api provides an HTTP server that implements an high-level API for managing the telescope. This API can change configuration, retrieve data and facilitate calibration.

Finally there is a general-use package in the tart-package directory. This contains imaging routines and many useful utilities for handling coordinate transformations e.t.c.

For everyday use, these can simply be installed using PyPi

     sudo pip install tart tart_web_api tart_hardware_interface


## Authors

* Tim Molteno (tim@elec.ac.nz)
* Max Scheel (max@max.ac.nz)
* Tim Miller (milletf1@gmail.com)
* Pat Suggate (ihavenolimbs@gmail.com)


