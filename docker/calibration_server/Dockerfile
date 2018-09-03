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

# Add crontab file in the cron directory
ADD crontab /etc/cron.d/calibrate-cron
ADD tart_calibrate.sh /tart_calibrate.sh
 
# Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/calibrate-cron
 
# Create the log file to be able to run tail
RUN touch /var/log/cron.log
 
# Run the command on container startup
CMD cron && tail -f /var/log/cron.log
