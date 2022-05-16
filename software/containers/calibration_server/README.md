## Dockerfile for Telescope Calibration

Author: Tim Molteno

This docker file executes a process that uses known satellite positions to calibrate the telescope. The method of calibration is described in [1].

This instance requires a lot of processing power (approx 1 hour of CPU time on a single core high-end i7) and calibrates the TART telescope using a catalog of known L1-band objects (see object_position_server). These known objects are used as 'guide stars' to work out the gains and phases for each of the telescope antennas.

The process spends the first hour capturing 3 sets of observations at 25 minute intervals. After this, it uses these observations and the positions of the known radio sources at the time of observation. The entire process will take approximately two hours to complete (with only the second hour being very CPU intensive).

## run via docker-compose.yml

#### Configuration
Configure environment variables in docker-compose.yml

#### Build and run
```bash
docker-compose build && docker-compose up -d
```

#### Follow progress
```bash
docker-compose logs --follow
```

####
```bash
docker-compose ps  # status is calibration still running?
docker-compose stop  # stop calibration run?
```



## Docker Usage

The easiest way to build this is to use docker. To build the container type:

    sh build.sh

To execute the container type (where "passwd" is the password to your telescope web api server):

    sh run.sh passwd


### Stoping or Killing the instance

    docker ps -a
    docker stop cal
    docker rm cal


### Running this regularly

Add this command as a cron job. Modify the TART_LOGIN_PW and TART_API to refer to the URL of your TART telescope. Recommended interval is every two hours.

    docker run -d \
        -e TART_LOGIN_PW=$1 \
        -e TART_API=https://tart.elec.ac.nz/signal/ \
        -v ~/calibration_results:/app \
        --name=cal -it  calibration_server
    
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
