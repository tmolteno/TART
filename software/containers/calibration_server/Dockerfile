# Create a docker file for the calibration process.. 
# NOTE. This script executes the calibration process only once.
#
#  Author. Tim Molteno. tim@elec.ac.nz (c) 2018.
#
FROM debian:stretch
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y &&  apt-get install -y \
    python-pip python-numpy python-dateutil \
    python-matplotlib python-scipy \
    python-astropy python-healpy
    
RUN apt-get clean -y
RUN rm -rf /var/lib/apt/lists/*

# RUN pip install healpy

# Install tart python packages
RUN pip install tart tart-tools requests 

# setup working directory
WORKDIR /app

# Add the calibrate script.
ADD tart_calibrate.sh /tart_calibrate.sh

CMD sh /tart_calibrate.sh > /app/calibrate.log
