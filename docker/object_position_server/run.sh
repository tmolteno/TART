#!/bin/sh
# Run the docker image for the object position server
#
docker run -d -p 8876:8876 --name=ops -it  object_position_server
