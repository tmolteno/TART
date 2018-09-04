# Create a docker file for the calibration process.. This just executes a cron job
# Author. Tim Molteno.
#
FROM debian:stretch
MAINTAINER Tim Molteno "tim@elec.ac.nz"

# debian setup
RUN apt-get update -y
RUN apt-get upgrade -y 

RUN apt-get install -y python-pip cron python-numpy python-dateutil python-matplotlib python-astropy
RUN apt-get clean -y
RUN rm -rf /var/lib/apt/lists/*

RUN pip install healpy

# Install tart python packages
RUN pip install tart tart-tools requests 
# RUN pip install tart_web_api tart_hardware_interface

# setup working directory
WORKDIR /app

# Add the calibrate script.
ADD tart_calibrate.sh /tart_calibrate.sh


CMD sh /tart_calibrate.sh > /app/calibrate.log
