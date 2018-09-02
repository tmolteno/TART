from flask import Flask, request, jsonify
from flask import render_template, jsonify, send_file

from tart_web_api.main import app, get_config

import logging
from logging.handlers import RotatingFileHandler
from time import strftime
import traceback

handler = RotatingFileHandler('/tmp/tart_api.log', maxBytes=100000, backupCount=3)
logger = logging.getLogger('tdm')
logger.setLevel(logging.ERROR)
logger.addHandler(handler)

@app.after_request
def after_request(response):
    timestamp = strftime('[%Y-%b-%d %H:%M]')
    logger.error('%s %s %s %s %s %s',\
        timestamp, request.remote_addr,request.method,\
        request.scheme, request.full_path, response.status)
    return response

@app.errorhandler(Exception)
def exceptions(e):
    tb = traceback.format_exc()
    timestamp = strftime('[%Y-%b-%d %H:%M]')
    logger.error('%s %s %s %s %s 5xx INTERNAL SERVER ERROR\n%s',\
        timestamp, request.remote_addr, request.method,\
        request.scheme, request.full_path, tb)
    return e.status_code

