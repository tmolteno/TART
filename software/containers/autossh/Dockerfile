FROM debian:stretch-slim
ARG DEBIAN_FRONTEND=noninteractive
MAINTAINER Tim Molteno "tim@elec.ac.nz"

# debian setup
RUN apt-get update -y
RUN apt-get upgrade -y 
RUN apt-get install -y autossh
RUN rm -rf /var/lib/apt/lists/*

# autossh environment variables
ENV \
    AUTOSSH_LOGFILE=/dev/stdout \
    AUTOSSH_GATETIME=30         \
    AUTOSSH_POLL=10             \
    AUTOSSH_FIRST_POLL=30       \
    AUTOSSH_LOGLEVEL=1

ENTRYPOINT ["/entrypoint.sh"]
ADD /entrypoint.sh /entrypoint.sh
RUN chmod 755 /entrypoint.sh
