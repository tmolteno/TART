FROM debian:buster
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# If host is running squid-deb-proxy on port 8000, populate /etc/apt/apt.conf.d/30proxy
# sudo apt install -y squid-deb-proxy
#RUN /sbin/ip route | awk '/default/ { print "Acquire::http { Proxy \"http://"$3":8000\";} "};' | head -1 >> /etc/apt/apt.conf.d/30proxy

# debian setup
RUN apt-get update && apt-get install -y \
    gcc g++ make ruby-dev \
    build-essential zlib1g-dev

# RUN apt-get install -y ruby-bundler

RUN rm -rf /var/lib/apt/lists/*

RUN gem update -q --no-document 
RUN gem install -q --no-document  bundler

# Install Dependencies
RUN mkdir /ephemeris_server
WORKDIR /ephemeris_server
COPY ./code/Gemfile* /ephemeris_server/

RUN bundle install

# Put the code in there
ADD ./code/ /ephemeris_server

EXPOSE 8876
CMD ["ruby", "RackServer.rb"]
#CMD ["thin", "-C", "thin_config.yml", "-R", "config.ru", "start"]
# address: localhost
# port: 8876
# servers: 1
# max_conns: 1024
# max_persistent_conns: 512
# timeout: 30
# environment: development
# pid: tmp/pids/thin-production.pid
# log: logs/thin-production.log
# daemonize: no
