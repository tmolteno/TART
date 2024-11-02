FROM debian:buster
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# debian setup
RUN apt-get update -y && apt-get install -y \
    curl \
    python3-pip \
    python3-requests \
    python3-dateutil \
    python3-matplotlib \
    python3-yaml \
    python3-psycopg2 \
    python3-jsonrpclib-pelix \
    python3-flask python3-flask-script \
    python3-h5py \
    python3-astropy \
    python3-healpy 

RUN rm -rf /var/lib/apt/lists/*

COPY ./python_code /python_code

RUN pip3 install tart tart_tools

WORKDIR /python_code/tart_hardware_interface
RUN python3 setup.py install

WORKDIR /python_code/tart_web_api
RUN python3 setup.py install

COPY ./config_data /config_data
WORKDIR /app
ENV FLASK_APP=tart_web_api.main
#ENV FLASK_DEBUG=1

EXPOSE 5000

CMD flask run --no-reload -h0.0.0.0
