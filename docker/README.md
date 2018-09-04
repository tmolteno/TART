## TART Docker Files

This directory contains Dockerfiles that automate the execution of the parts of the telescope. These are


### Telescope Web API

This allows remote control of the telescope via a Restful interface. The web front end uses this API to configure and get data from the telescope.

### Calibration Server

A server that runs at regular intervals and uses known objects in the sky to calibrate the telescope.

### Object Position Server

A restful API server that returns the positions in the sky of known L1 sources for a telescope at any position on earth.
