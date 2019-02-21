import logging
import traceback
from time import strftime

from flask import request

from tart_web_api.main import app

from logging.handlers import RotatingFileHandler

HANDLER = RotatingFileHandler('/tmp/tart_api.log', maxBytes=100000, backupCount=3)
LOGGER = logging.getLogger('tdm')
LOGGER.setLevel(logging.ERROR)
LOGGER.addHandler(HANDLER)

@app.after_request
def after_request(response):
    timestamp = strftime('[%Y-%b-%d %H:%M]')
    LOGGER.error('%s %s %s %s %s %s',\
        timestamp, request.remote_addr, request.method, \
        request.scheme, request.full_path, response.status)
    return response

@app.errorhandler(Exception)
def exceptions(err):
    tb_msg = traceback.format_exc()
    timestamp = strftime('[%Y-%b-%d %H:%M]')
    LOGGER.error('%s %s %s %s %s 5xx INTERNAL SERVER ERROR\n%s', \
        timestamp, request.remote_addr, request.method, \
        request.scheme, request.full_path, tb_msg)
    return err.status_code
