#
#
# Stage 0, "build-stage", based on Node.js, to build and compile the frontend
FROM node:10 as build-stage
MAINTAINER Tim Molteno "tim@elec.ac.nz"
ARG tart_name

WORKDIR /code
COPY ./web-app ./

ENV npm_config_jobs=2
RUN npm install

RUN npm run ng -- build -sm --base-href=/${tart_name}/ 
#RUN npm run ng -- build --prod -sm --base-href=/${tart_name}/ 

#
#
# Stage 1, based on Nginx, to have only the compiled app, ready for production with Nginx
FROM nginx:1.15
ARG tart_name

COPY --from=build-stage /code/dist/ /var/www/html
  
# Copy the default nginx.conf provided by tiangolo/node-frontend
COPY ./rpi_nginx.conf /etc/nginx/conf.d/default.conf
RUN /bin/sed -i "s/<tart_name>/${tart_name}/" /etc/nginx/conf.d/default.conf
RUN echo  "s/<tart_name>/$tart_name/"
EXPOSE 80
