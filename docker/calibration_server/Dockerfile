# Create a docker file for the calibration process.. 
# NOTE. This script executes the calibration process only once.
#
#  Author. Tim Molteno. tim@elec.ac.nz (c) 2018.
#
FROM debian:stretch
MAINTAINER Tim Molteno "tim@elec.ac.nz"

RUN apt-get update -y
RUN apt-get upgrade -y 

RUN apt-get install -y python-pip python-numpy python-dateutil python-matplotlib python-scipy python-astropy
RUN apt-get clean -y
RUN rm -rf /var/lib/apt/lists/*

RUN pip install healpy

# Install tart python packages
RUN pip install tart tart-tools requests 

# setup working directory
WORKDIR /app

# Add the calibrate script.
ADD tart_calibrate.sh /tart_calibrate.sh

CMD sh /tart_calibrate.sh > /app/calibrate.log
