FROM debian:stretch-slim
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG DEBIAN_FRONTEND=noninteractive

# debian setup
RUN apt-get update && apt-get install -y \
    python-numpy python-dateutil \
    python-flask python-sgp4 python-requests python-tz \
    python-pip

RUN rm -rf /var/lib/apt/lists/*

RUN pip install --no-cache-dir flask-cors

# setup working directory
ADD ./app/ /object_position_server
WORKDIR /object_position_server

ENV FLASK_APP=restful_api.py
ENV FLASK_DEBUG=1

EXPOSE 8876

CMD ["python", "restful_api.py", "--host=0.0.0.0"]
