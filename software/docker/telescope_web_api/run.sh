#!/bin/sh
# Run the docker image. The login password must be specified on the command line 
# when this image is run.
#
# Persistent storage is in the folder ~/web_api_store
#
docker run -d -e LOGIN_PW=$1 -P -p 5000:5000 \
    --device=/dev/spidev0.0 \
    --name=web_api -it  tart_web_api
# twistd -n web --port 8080 --wsgi tart_web_api.app
#     -v ~/web_api_store:/app \
