FROM debian:buster
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# debian setup
RUN apt-get update && apt-get install -y \
    curl \
    python3-pip \
    python3-dateutil \
    python3-psycopg2 \
    python3-astropy \
    python3-healpy \
    python3-matplotlib \
    python3-jsonrpclib-pelix \
    python3-h5py \
    python3-pyfftw

RUN rm -rf /var/lib/apt/lists/*

ENV LANG C.UTF-8

WORKDIR /code
COPY ./python_modules /code/

WORKDIR /code/tart
RUN python3 setup.py install

# WORKDIR /code/tart_hardware_interface
# RUN python3 setup.py install

WORKDIR /code/tart/
CMD python3 setup.py test --test-suite=tart.imaging.test.test_visibility

