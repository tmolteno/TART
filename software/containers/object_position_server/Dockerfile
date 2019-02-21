FROM debian:buster-slim
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# debian setup
RUN apt-get update && apt-get install -y \
    python3-numpy python3-dateutil \
    python3-flask python3-flask-cors \
    python3-sgp4 python3-requests python3-tz

RUN rm -rf /var/lib/apt/lists/*

# RUN pip install --no-cache-dir flask-cors

# setup working directory
ADD ./app/ /object_position_server
WORKDIR /object_position_server

ENV FLASK_APP=restful_api.py
ENV FLASK_DEBUG=1

EXPOSE 8876

CMD ["python3", "restful_api.py", "--host=0.0.0.0"]
