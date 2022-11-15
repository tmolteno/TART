FROM debian:bullseye
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# If host is running squid-deb-proxy on port 8000, populate /etc/apt/apt.conf.d/30proxy
# sudo apt install -y squid-deb-proxy
#RUN /sbin/ip route | awk '/default/ { print "Acquire::http { Proxy \"http://"$3":8000\";} "};' | head -1 >> /etc/apt/apt.conf.d/30proxy

# debian setup
RUN apt-get update && apt-get install -y \
    gcc g++ make \
    build-essential git \
    sudo autogen automake g++ libftdi-dev

RUN mkdir /pappy
WORKDIR /pappy


RUN git clone https://github.com/GadgetFactory/Papilio-Loader
RUN ls
WORKDIR /pappy/Papilio-Loader/papilio-prog
RUN ls -l

RUN apt-get install -y 
RUN ./autogen.sh
RUN ./configure && make

RUN ls -l
# Add the bitfile

COPY ./tart.bit tart.bit
RUN ls -l
# CMD ["papilio-prog -b ./tart.bit -f SPARTAN6"]
CMD ["papilio-prog", "-v", "-f ./tart.bit"]
