# TART Install Guide

A TART telescope consists of some telescope hardware connected to a Raspberry Pi. Installing your own TART telescope requires several steps:

* Building and testing the radio hardware
* Installing the TART software on the Raspberry Pi
* Connecting to the TART via web interface
* Set up radio receiver array. This is typically done in a reasonably radio-quiet place.
* Calibrating the telescope.

## Building and testing the radio hardware

If you are interested in building the telescope contact tim@elec.ac.nz, as we have printed circuit boards, and other components that we are willing to give to anyone interested in deploying their own telescope.

Hardware documentation can be found in [hardware/README.md](hardware/README.md).


### Radio Testing

TODO 

## Installing software on the Raspberry Pi

Execute the following commands to copy the necessary software to the target Pi (assumed to be called 'tart2-dev' below:

    TARGET=pi@tart2-dev.local

    rsync -rv --exclude=node_modules software ${TARGET}:.

Modify the file docker-compose.yml to change the password and the name of your telescope.

On your telescope host, execute the following:

Then install docker and docker-compose on the target raspberry pi

    curl -fsSL get.docker.com -o get-docker.sh && sh get-docker.sh
    sudo gpasswd -a $USER docker
    newgrp docker
    sudo pip install docker-compose

Now log into the pi and run:
    cd software
    docker-compose up
 
Point your browser to the raspberry pi (http://tart2-dev.local). You should see the telescope web interface. 

Further software installation information cab be found in [software/README.md](software/README.md).

## Set up radio hardware

Once the telescope can be accessed via its web interface, the telescope hardware should be put out in a suitable location. Then the calibration procedure should be followed. This requires two steps that are outlined in the [calibration documentation](doc/calibration/README.md).

## Other Useful Things

* Using the web_api from remote computers to gather data.
