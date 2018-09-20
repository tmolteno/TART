# TART Install Guide

A TART telescope consists of some telescope hardware connected to a Raspberry Pi. Installing your own TART telescope requires several steps:

* Building and testing the radio hardware
* Installing the TART software on the Raspberry Pi
* Connecting to the TART via web interface
* Set up radio receiver array. This is typically done in a reasonably radio-quiet place.
* Calibrating the telescope.

## Building and testing the radio hardware

If you are interested in building the telescope contact tim@elec.ac.nz, as we have PCBs here that we are willing to give to anyone interested.

### Radio Testing

TODO 

## Installing software on the raspberry Pi

Execute the following commands to copy the necessary software to the target Pi

    TARGET=pi@tart2-dev

    rsync -rv web-app ${TARGET}:.
    rsync -rv docker ${TARGET}:.
    rsync -rv hardware ${TARGET}:.

Modify the file docker-compose.yml to change the password and the name of your telescope.

On your telescope host, execute the following:

Then install docker and docker-compose on the target raspberry pi

    curl -fsSL get.docker.com -o get-docker.sh && sh get-docker.sh
    sudo gpasswd -a $USER docker
    sudo pip install docker-compose

Now log into the pi and run:
    cd docker
    docker-compose up
 
Point your browser to the raspberry pi.

## Set up radio hardware

### Antenna Location Calibration

## Calibrating the Telescope

## Other Useful Things

* Using the web_api from remote computers to gather data.
