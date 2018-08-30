FROM debian:stretch
MAINTAINER Tim Molteno "tim@elec.ac.nz"

# debian setup
RUN apt-get update -y
RUN apt-get upgrade -y 

RUN apt-get install -y python-pip screen python-numpy python-dateutil
RUN pip install flask sgp4 flask-cors pytz requests

# setup working directory
ADD ./app/ /object_position_server
WORKDIR /object_position_server

ENV FLASK_APP=restful_api.py
ENV FLASK_DEBUG=1

EXPOSE 8876

CMD ["python", "restful_api.py", "--host=0.0.0.0"]
