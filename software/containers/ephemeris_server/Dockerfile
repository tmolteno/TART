FROM debian:buster
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# If host is running squid-deb-proxy on port 8000, populate /etc/apt/apt.conf.d/30proxy
# sudo apt install -y squid-deb-proxy
RUN /sbin/ip route | awk '/default/ { print "Acquire::http { Proxy \"http://"$3":8000\";} "};' | head -1 >> /etc/apt/apt.conf.d/30proxy

# debian setup
RUN apt-get update && apt-get install -y \
    gcc g++ make ruby-dev ruby-nokogiri

RUN rm -rf /var/lib/apt/lists/*

RUN gem update
RUN gem install thin rack-rpc jimson sinatra sinatra-cors nokogiri

# setup working directory
ADD ./code/ /ephemeris_server
WORKDIR /ephemeris_server

EXPOSE 8876

CMD ["thin -C thin_config.yml -R config.ru"]
