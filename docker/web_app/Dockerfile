# Stage 0, "build-stage", based on Node.js, to build and compile the frontend
FROM node:8 as build-stage

ARG tart_name
WORKDIR /app

RUN npm install apidoc -g

COPY ./app /app

RUN ls
RUN npm install

ARG configuration=production
RUN npm run ng build -sm --stats-json --base-href /${tart_name}/ --configuration $configuration


# Stage 1, based on Nginx, to have only the compiled app, ready for production with Nginx
FROM nginx:1.15
ARG tart_name

COPY --from=build-stage /app/dist/ /usr/share/nginx/html

# Copy the default nginx.conf provided by tiangolo/node-frontend
COPY ./rpi_nginx.conf /etc/nginx/conf.d/default.conf
RUN /bin/sed -i "s/<tart_name>/${tart_name}/" /etc/nginx/conf.d/default.conf
RUN echo  "s/<tart_name>/$tart_name/"
EXPOSE 80
