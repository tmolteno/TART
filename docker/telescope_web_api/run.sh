#!/bin/sh
# Run the docker image. The login password must be specified here.
#
docker run -e LOGIN_PW=password -p 5000:5000 --name=web_api -it  tart_web_api
# twistd -n web --port 8080 --wsgi tart_web_api.app
