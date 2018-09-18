## TART Software

The telescope software is built using docker software containers. This directory is the top-level of all the software containers. Some of the containers run on the telescope itself (the API and web_app), while others are used for calibration and providing a catalog of known objects.


## Installation on a target raspberry pi

The following procedure will install all the necessary TART software on a Raspberry Pi (Model 3) attached to the TART hardware.

### Step 1. Prepare the Pi

Install docker on the raspberry pi. This is done by following.

### Step 1. Copy code to the Pi

    TARGET=pi@tart2-dev

    rsync -rv software ${TARGET}:.
    rsync -rv hardware ${TARGET}:.

### Step 2. Build on the Pi

SSH into the raspberry pi after completing step 1.

    cd software
    docker-compose build

This will build all the necessary sofware on the Pi. To run all the software an services. Type

    docker-compose up -d

### Testing

Point your browser at the target Pi http://<target_pi>/. You should see the TART web interface. Remember to login and change the mode to 'vis'


## Calibration

The calibration software is 
## Object Position Server
