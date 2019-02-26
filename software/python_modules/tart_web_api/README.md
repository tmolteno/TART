# TART: Radio-telescope high-level HTTP interface

This module is used to communicate with the TART radio telescope. It listens on a port on localhost, and
allows a web-based interface to communicate with the TART hardware.

For more information see the [TART github repository](https://github.com/tmolteno/TART)

## Authors

* Tim Molteno (tim@elec.ac.nz)
* Max Scheel (max@max.ac.nz)
* Tim Miller (milletf1@gmail.com)

## Running The API Server

The API server is a flask app written in Python. This is run using the 'flask' command line tool on the telescope host (Raspberry Pi)
that is installed as a dependency of this package. A shell script 'run_api.sh' should be created with the following contents:

    #!/bin/bash
    export LOGIN_PW=password
    export FLASK_APP=tart_web_api.main
    flask run

where 'password' is a secure password that can be used to log into the telescope web-application to configure your telescope.

## Docker

We are now using to deployment using docker. There is a Dockerfile for this in the TART/software/docker/tart_web_api directory.

## Twisted

    twistd -n web --port 8080 --wsgi tart_web_api.app

## Development work
    
If you are developing this package, this should be installed using

    python3 setyp.py develop

in which case changes to the source-code will be immediately available to projects using it.

    
## NEWS

* Version 0.1.8 Python3 compatability
