FROM debian:stretch
MAINTAINER Tim Molteno "tim@elec.ac.nz"

# debian setup
RUN apt-get update -y
RUN apt-get upgrade -y 

RUN apt-get install -y curl python-pip screen python-numpy python-dateutil python-scipy python-matplotlib

RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt-get install -y nodejs
RUN apt-get install npm 

RUN apt-get install -y python-yaml python-astropy python-psycopg2 ntp python-jsonrpclib

# Install latest node
RUN npm cache clean -f
RUN npm install -g n
RUN n stable 

# Install tart python packages
RUN pip install tart tart_web_api tart_hardware_interface requests 
RUN pip install healpy

RUN apt-get install -y nginx
RUN apt-get install -y sqlite3

# setup working directory
COPY app /app
WORKDIR /app
RUN chmod 755 /app

RUN ls

ENV FLASK_APP=/app/tart_web_api
ENV FLASK_DEBUG=1

EXPOSE 8080
EXPOSE 5000

#CMD ["flask", "run", "-h0.0.0.0"]
