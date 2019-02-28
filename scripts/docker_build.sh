#!/bin/sh
# First copy the necessary python modules into the build directory
PWD=`pwd`
(cd ../software/containers/telescope_web_api && sh pre_build.sh);
(cd ../software/containers && docker-compose build);
