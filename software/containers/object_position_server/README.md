## OBJECT POSITION SERVER

This is a python server that will serve positions of objects that transmit in the L1 band.

Author: Tim Molteno. tim@elec.ac.nz

## Prerequisites

A computer with [docker](https://docker.io) installed, and having external web access to software repositories. We have tested these only on Linux-based computers.

## Docker For Object Position Server

The easiest way to build this is to use docker. To build the container type

    docker-compose build

To run it (the -d puts it in the background)

    docker-compose up -d

This creates an instance called 'ops'. You can check the logs using 

    docker attach ops

To exit type Ctrl-p Ctrl-q

    
To kill the instance

    docker-compose down

    
## Testing

    wget -qO- "http://localhost:8876/catalog?lat=-45.85&lon=170.54"
