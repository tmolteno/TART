#!/bin/sh
# Run the docker image for the telescope calibration cron job (run every two hours)
# Bind Mount the directory ~/calibration_results as persistent storage for the results of calibration
#
# Author. Tim Molteno tim@elec.ac.nz. (c) 2018.
# TODO. Change the TART_LOGIN_PW to be the same as used to run the telescope web api.
#
docker run \
    -e TART_LOGIN_PW=password \
    -e TART_API=https://tart.elec.ac.nz/signal/ \
    -v ~/calibration_results:/app \
    --name=cal -it  calibration_server
