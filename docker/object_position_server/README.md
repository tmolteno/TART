## OBJECT POSITION SERVER

This is a python server that will serve positions of objects that transmit in the L1 band.

Author: Tim Molteno. tim@elec.ac.nz


## Docker For Object Position Server

The easiest way to build this is to use docker. To build the container type

    sh build.sh
    sh run.sh

This creates an instance called 'ops'. You can check the logs using 

    docker attach ops

To exit type Ctrl-p Ctrl-q

    
To kill the instance

    docker ps -a
    docker stop ops
    docker rm ops

    
## Testing

    wget http://localhost:8876/catalog?lat=-45.85&lon=170.54
