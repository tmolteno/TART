## Dockerfile for the TART telescope web API

The telescope web API listens for restful requests that are used to condfigure and get data from the telescope. 
The config_data directory contains information about your TART telescope such as it's name. You can change this information.

### Details

The server listens on port 5000 (on localhost). It is normally deployed using docker-compose from the top level
docker-compose.yml. 

**Because of this, the following instructions are for development only**. For normal use, look into the main [software documentation](../README.md)
that uses docker-compose to manage the software install.


## Docker Usage for TESTING ONLY

The easiest way to build this is to use docker. To build the container type

    sh pre_build.sh
    sh build.sh

To execute the container type (where <passwd> will be the password for authorized access to your telescope web api server):

    sh run.sh <passwd>


### Stoping or Killing the instance

    docker ps -a
    docker stop web_api
    docker rm web_api

    
### Debugging

This creates a running instance called 'web_api'. You can check the logs using 

    docker attach web_api

To exit type Ctrl-p Ctrl-q


To run a bash shell on the container. The logs are contained in the file /var/log/cron.log

    docker exec -it web_api bash
    
