#!/bin/bash
# Copy this file:
#   cp run_api.sh run_api_local.sh
# Change web api password:
#   vim run_api_local.sh
# Make it executeable:
#   chmod +x run_api_local.sh
export LOGIN_PW=password
export FLASK_APP=tart_web_api
flask run
