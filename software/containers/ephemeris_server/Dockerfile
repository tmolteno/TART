FROM debian:buster
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# If host is running squid-deb-proxy on port 8000, populate /etc/apt/apt.conf.d/30proxy
# sudo apt install -y squid-deb-proxy
RUN /sbin/ip route | awk '/default/ { print "Acquire::http { Proxy \"http://"$3":8000\";} "};' | head -1 >> /etc/apt/apt.conf.d/30proxy

# debian setup
RUN apt-get update && apt-get install -y \
    gcc g++ make ruby-dev ruby-nokogiri

RUN apt-get install -y thin

RUN rm -rf /var/lib/apt/lists/*

RUN gem update
# RUN gem install -q -no-rdoc --no-ri thin 
RUN gem install -q -no-rdoc --no-ri rack-rpc 
RUN gem install -q -no-rdoc --no-ri jimson 
RUN gem install -q -no-rdoc --no-ri sinatra 
RUN gem install -q -no-rdoc --no-ri sinatra-cors

# setup working directory
ADD ./code/ /ephemeris_server
WORKDIR /ephemeris_server

EXPOSE 8876
# 
# RUN mkdir -p /etc/thin-server
# ADD startup.sh /etc/thin-server/startup.sh
# ENTRYPOINT ["/bin/bash",  "/etc/thin-server/startup.sh"]
# CMD ["-p", "40000", "-e", "production"]

#CMD ["thin", "-C", "thin_config.yml", "-R", "config.ru", "start"]
CMD ["thin", "-R", "config.ru", "--debug", "start"]
