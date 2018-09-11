#!/bin/sh
# Run the docker image. The login password must be specified on the command line 
# when this image is run.
#
# Persistent storage is in the folder ~/web_api_store
#
docker run -d -P -p 80:80 --net="host" \
    --name=web_app -it  tart_web_front_end:prod
