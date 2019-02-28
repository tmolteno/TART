# Create a docker file for the calibration process.. 
# NOTE. This script executes the calibration process only once.
#
#  Author. Tim Molteno. tim@elec.ac.nz (c) 2018.
#
FROM debian:buster
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y &&  apt-get install -y \
    python3-pip python3-numpy python3-dateutil \
    python3-matplotlib python3-scipy \
    python3-astropy python3-healpy
    
RUN apt-get clean -y
RUN rm -rf /var/lib/apt/lists/*

# RUN pip install healpy

# Install tart python packages
RUN pip3 install tart tart-tools requests 

# setup working directory
WORKDIR /app

# Add the calibrate script.
ADD tart_calibrate.sh /tart_calibrate.sh

CMD sh /tart_calibrate.sh > /app/calibrate.log
