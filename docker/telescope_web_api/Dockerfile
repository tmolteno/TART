# This is going to run on a raspberry pi, so the
# Docker container needs to be created from an armv7 
FROM debian:stretch
MAINTAINER Tim Molteno "tim@elec.ac.nz"

# debian setup
RUN apt-get update -y
RUN apt-get upgrade -y 

RUN apt-get install -y curl python-pip screen python-numpy python-dateutil python-scipy python-matplotlib


RUN apt-get install -y python-yaml python-astropy python-psycopg2 ntp python-jsonrpclib

# Install latest node
#RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
#RUN apt-get install -y nodejs
#RUN apt-get install npm 
#RUN npm cache clean -f
#RUN npm install -g n
#RUN n stable 

RUN pip install healpy
# Install tart python packages
RUN pip install tart tart_web_api tart_hardware_interface requests 

RUN apt-get install -y sqlite3

VOLUME /app
# setup working directory
COPY app /app
WORKDIR /app

# RUN python 

ENV FLASK_APP=tart_web_api.main
ENV FLASK_DEBUG=1

EXPOSE 5000

CMD ["flask", "run", "-h0.0.0.0"]
