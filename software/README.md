## TART Software

The telescope software is built using docker software containers. This directory is the top-level of all the software containers. Some of the containers run on the telescope itself (the API and web_app), while others are used for calibration and providing a catalog of known objects.

The folder are structured as follows

* containers - docker images for operation and calibration of the TART
* python_modules - python modules used by the docker containers.

The Software runs on three different computers. 

* A Raspberry Pi (Model 3) which is plugged into the TART basestation board. This runs the telescope web API server.
* A calibration server which is a fast desktop that runs a calibration routine at regular intervals (every few hours)
* An optional object position server. This is a server that provides a catalog of known objects and their elevation/azimuth for any point on earth. A public one is available so you'll only need to provide your own server if you're running a process that requires low-latency access to this information.


## Local Site Configuration

You should modify the files in software/containers/telescope_web_api/config_data to contain the new telescope configuration information (latitude, longitude, altitude) as 
well as the callibrated antenna positions.

## Installation on a target Raspberry Pi

The following procedure will install all the necessary TART software on a Raspberry Pi (Model 3) attached to the TART hardware.

### Step 1. Prepare the Pi

Set hostname, activate SPI & SSH with raspi-config ( SSH and SPI can be enabled under Interfacing Options) :

    sudo raspi-config
    
Now run:

    sudo apt install python3-pip libffi-dev python3-cffi gcc libssl-dev python3-dev git

Install docker on the raspberry pi. This is done by following commands.  
    
    
    curl -fsSL get.docker.com -o get-docker.sh && sh get-docker.sh
    sudo usermod -aG docker $USER
    sudo pip3 install docker-compose
    sudo reboot
    
Debian Buster:

    sudo apt install docker.io docker-compose
    sudo usermod -aG docker $USER
    sudo reboot
    

### Step 2. Copy code to the Pi

This step assumes that the raspberry pi is accessible as the host name 'tart2-dev.local' on your local network.

    TARGET=pi@tart2-dev.local
    (cd ../software/containers/telescope_web_api && sh pre_build.sh);
    rsync -rv --exclude=node_modules ../software ${TARGET}:.
    
There is a script, /scripts/install_pi.sh, which performs this task.

### Step 3. Build on the Pi

SSH into the raspberry pi after completing step 1.

    cd software
    
Edit docker-compose.yml and change LOGIN_PW= from passwd to your secure password. This password is used to log in to the TART web interface

Now run  
 
    docker-compose build
This last step can take ages (around 1 hour or so)

This will build all the necessary sofware on the Pi. To run all the software an services. Type

    docker-compose up

This will launch all the necessary processes in docker containers on the pi.

### Step 4.

To make the system start automatically at startup (and run in the background) modify the line in step 3 to

    docker-compose up -d


### Testing

Point your browser to the raspberry pi (http://tart2-dev.local). You should see the telescope web interface. 

On a different computer, you should be able to download data from the command line using the web api.

    sudo pip3 install tart_tools
    tart_download_data --api http://tart2-dev.local --pw <passwd> --raw 
    
This should download some HDF files to your local machine. These can be checked using the HDFCompass programme (apt install hdf-compass)


#### Documentation Server

Point your browser at  [http:/tart2-dev.local/doc/](http:/tart2-dev.local/doc/). You should see the documentation for the TART web API. 

#### Live Telescope View

Point your browser at the target Pi [http:/tart2-dev.local/](http:/tart2-dev.local/). You should see the TART web interface. Remember to login and change the mode to 'vis' so that the telescope starts acquiring data.


## Calibration

The calibration software is also packaged with docker, and should run on a reasonably fast machine with access to the radio telescope. It uses the object position server to get a catalog of known objects that should be in the telescope field of view, and calculates gains and phases based on this information. See the [containers/calibration_server](containers/calibration_server/README.md) directory.

## Object Position Server

The object position server runs on a host, and provides a list of known objects for any location on earth and any time. A public server is available at https://tart.elec.ac.nz/catalog. The default installation of the operating software uses this server. You can test it using

    https://tart.elec.ac.nz/catalog/catalog?date=2019-02-07T09:13:28+13:00&lat=-45.85177&lon=170.5456

If you wish to install your own server, you can build the docker container in the [containers/object_position_server](containers/object_position_server/README.md) directory.
