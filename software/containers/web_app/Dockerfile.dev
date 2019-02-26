#
#
# Build the web app for development purposes.
#
#
FROM node:10 as build-stage
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG tart_name

RUN mkdir /code
WORKDIR /code

ENV PATH /code/node_modules/.bin:$PATH

COPY ./web-app/package*.json /code/
COPY ./web-app/api_imaging /code/api_imaging

ENV npm_config_jobs=2
RUN npm ci

#RUN npm run ng -- build --prod --source-map --base-href=/${tart_name}/ 

COPY . /code

CMD ng serve --host 0.0.0.0
