#!/bin/sh
# Run the docker image for the object position server
# Bind Mount the directory ~/catalog_cache as persistent storage for the catalog
# data
# 
docker run -d -p 8876:8876 -v ~/catalog_cache:/object_position_server/orbit_data --name=ops -it  object_position_server
