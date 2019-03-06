FROM debian:buster
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# If host is running squid-deb-proxy on port 8000, populate /etc/apt/apt.conf.d/30proxy
# sudo apt install -y squid-deb-proxy
RUN /sbin/ip route | awk '/default/ { print "Acquire::http { Proxy \"http://"$3":8000\";} "};' | head -1 >> /etc/apt/apt.conf.d/30proxy

# debian setup
RUN apt-get update && apt-get install -y \
    gcc g++ make ruby-dev \
    build-essential zlib1g-dev

# RUN apt-get install -y ruby-bundler

RUN rm -rf /var/lib/apt/lists/*

RUN gem update -q --no-document 
RUN gem install -q --no-document  bundler

# setup working directory
ADD ./code/ /ephemeris_server
WORKDIR /ephemeris_server

RUN bundle install


EXPOSE 8876
CMD ["ruby", "RackServer.rb"]
