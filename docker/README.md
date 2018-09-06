## TART Docker Files

This directory contains Dockerfiles that automate the execution of the parts of the telescope. These are

Author: Tim Molteno (tim@elec.ac.nz)

### Web Application (web_app)

This is a javascript application that runs in a browser (anywhere on earth) and communicates with the web API to view the telescope. 
This docker image should be installed on the raspberry pi with the telescope hardware.

### Telescope Web API

This allows remote control of the telescope via a Restful interface. The web front end uses this API to configure and get data from the telescope.

This docker image should be installed on the raspberry pi with the telescope hardware.

### Calibration Server

A server that runs at regular intervals and uses known objects in the sky to calibrate the telescope.

This docker image should be installed on a reasonably powerful server with access to the telescope web api server.


### Object Position Server

A restful API server that returns the positions in the sky of known L1 sources for a telescope at any position on earth.
