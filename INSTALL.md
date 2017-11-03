# TART Install Guide

A TART telescope consists of some telescope hardware connected to a Raspberry Pi. Installing your own TART telescope requires several steps:

* Building and testing the radio hardware
* Installing the TART software on the Raspberry Pi
* Connecting to the TART via web interface
* Set up radio receiver array. This is typically done in a reasonably radio-quiet place.
* Calibrating the telescope.

## Building and testing the radio hardware

### Radio Testing

## Installing software on the raspberry Pi

On your telescope host, execute the following:

    sudo pip install tart-web-api 

Then create a shell script with the following contents.

    #!/bin/bash
    export LOGIN_PW=password
    export FLASK_APP=tart_web_api
    flask run

## Set up radio receiver array

### Antenna Location Calibration

## Calibrating the Telescope

## Other Useful Things

* Using the web_api from remote computers to gather data.
