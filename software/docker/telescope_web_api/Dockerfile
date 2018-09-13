# This is going to run on a raspberry pi, so the
# Docker container needs to be created from an armv7 
FROM debian:buster
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# debian setup
RUN apt-get update -y
RUN apt-get upgrade -y 

RUN apt-get install -y curl python-pip screen python-numpy python-dateutil python-scipy python-matplotlib

RUN apt-get install -y python-yaml python-astropy python-psycopg2 ntp python-jsonrpclib

RUN apt-get install -y python-flask python-flask-script npm

RUN rm -rf /var/lib/apt/lists/*

# Install tart python packages
RUN pip install --no-cache-dir healpy tart tart_hardware_interface requests

COPY ./tart_web_api /code
WORKDIR /code
RUN python setup.py install

RUN npm install apidoc -g

RUN apidoc -i ./ -o /telescope_data/doc

COPY ./config_data /config_data
WORKDIR /app
ENV FLASK_APP=tart_web_api.main
#ENV FLASK_DEBUG=1

EXPOSE 5000

CMD flask run --no-reload -h0.0.0.0
