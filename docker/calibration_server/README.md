## Dockerfile for Telescope Calibration

This docker file creates a process that runs at regular intervals and calibrates the telescope. The method of calibration is
described in [1].

This instance requires a lot of processing power (approx 1.5 hours of CPU time) and calibrates the TART telescope using a catalog
of known L1-band objects (see object_position_server). These known objects are used as 'guide stars' to work out the gains and phases
for each of the telescope antennas.

## Docker Usage

The easiest way to build this is to use docker. To build the container type

    sh build.sh

To execute the container type (where <passwd> is the password to your telescope web api server:

    sh run.sh <passwd>


### Stoping or Killing the instance

    docker ps -a
    docker stop cal
    docker rm cal

    
### Debugging

This creates a running instance called 'cal'. You can check the logs using 

    docker attach cal

To exit type Ctrl-p Ctrl-q


To run a bash shell on the container. The logs are contained in the file /var/log/cron.log

    docker exec -it cal bash
    
Then you can manually run a calibration using

    sh /tart_calibrate.sh

The calibration normally takes 50 minutes to download data, and approximately 1.5 hours to run the optimization.

[1] Molteno et al. "Continuous Calibration of TART using GPS satellites". ENZCon2017.
