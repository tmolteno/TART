#!/bin/sh
rm -rf app
cp -a ../../web-app app
docker build -t tart_web_front_end:prod .
