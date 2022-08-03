FROM debian:bullseye
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# debian setup
RUN apt-get update && apt-get install -y \
    python3-numpy python3-dateutil \
    python3-flask python3-flask-cors \
    python3-sgp4 python3-requests python3-tz \
    python3-waitress python3-pip

RUN rm -rf /var/lib/apt/lists/*
RUN ls
RUN pip3 install tart

# setup working directory
ADD ./app/ /object_position_server
WORKDIR /object_position_server

CMD waitress-serve --port 8876 'restful_api:app'
