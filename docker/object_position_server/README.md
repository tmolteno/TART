## Docker For Object Position Server

    sh build.sh
    sh run.sh

This creates an instance called 'ops'. You can check the logs using 

    
    docker attach ops

To exit type Ctrl-p Ctrl-1

    
To kill the instance

    docker ps -a
    docker stop ops
    docker rm ops
