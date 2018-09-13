# This is going to run on a raspberry pi, so the
# Docker container needs to be created from an armv7 
FROM debian:stretch
MAINTAINER Tim Molteno "tim@elec.ac.nz"

# debian setup
RUN apt-get update -y
RUN apt-get upgrade -y 

RUN apt-get install -y curl python-pip screen python-numpy python-dateutil python-scipy python-matplotlib

RUN apt-get install -y python-yaml python-astropy python-psycopg2 ntp python-jsonrpclib

RUN apt-get install -y python-flask python-flask-script

# Install tart python packages
RUN pip install healpy tart tart_hardware_interface requests
# RUN pip install flask flask-jwt-extended flask-cli flask-cors flask-script

COPY ./config_data /config_data
COPY ./tart_web_api /code

WORKDIR /code

RUN ls
RUN python setup.py install
# RUN apidoc -i ./ -o /var/www/html/doc

# RUN python 

ENV FLASK_APP=tart_web_api.main
#ENV FLASK_DEBUG=1

RUN ls -l /code

EXPOSE 5000

WORKDIR /app
CMD flask run --no-reload -h0.0.0.0
