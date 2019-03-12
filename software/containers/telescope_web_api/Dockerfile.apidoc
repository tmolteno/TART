# Stage 0, "build-stage", build the documentation
FROM node:10 as build-stage
MAINTAINER Tim Molteno "tim@elec.ac.nz"

RUN npm install apidoc -g

COPY ./python_code/tart_web_api /code

WORKDIR /code/tart_web_api
RUN sed "s/localhost/`hostname`/g" -i apidoc.json

RUN apidoc -i ./ -o /doc


# Stage 1, based on Nginx, to have only the compiled documentation, served by nginx
FROM nginx:1.15

COPY --from=build-stage /doc/ /usr/share/nginx/html

EXPOSE 80
