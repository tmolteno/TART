#
# TART web api main file.
#
# Maximilian Scheel (c) 2017 max@max.ac.nz
# Tim Molteno 2018-2019. tim@elec.ac.nz
#
import multiprocessing
import logging

from flask import Flask, jsonify, request
from flask_jwt_extended import JWTManager

from flask_cors import CORS, cross_origin


from tart_web_api.service import cleanup_observation_cache, cleanup_visibility_cache, TartControl
from tart_web_api.config import init_config
import tart_web_api.database as db


def tart_p(rt_config):
    tart_control = TartControl(rt_config)
    while True:
        tart_control.set_state(rt_config['mode'])
        tart_control.run()



db.setup_db()

app = Flask(__name__)

# Set up logging

@app.before_first_request
def setup_logging():
    if not app.debug:
        # In production mode, add log handler to sys.stderr.
        app.logger.addHandler(logging.StreamHandler())
        app.logger.setLevel(logging.INFO)

CORS(app)
#app.config['SECRET_KEY'] = 'super-secret-cow-key-hsa'
#from datetime import timedelta as td
#app.config['JWT_EXPIRATION_DELTA'] = td(seconds=3600)
#JWT(app, authenticate, identity)

app.secret_key = 'super-secret-123897219379179464asd13khk213'  # Change this!
app.config['JWT_HEADER_TYPE'] = 'JWT'
jwt = JWTManager(app)

import tart_web_api.views
import tart_web_api.views_auth
import tart_web_api.views_acquisition
import tart_web_api.views_cal
import tart_web_api.views_cache
import tart_web_api.views_obs
#import tart_web_api.views_log
import tart_web_api.views_channel

#if __name__ == "__main__":
m = multiprocessing.Manager()
runtime_config = init_config(m)
runtime_config['sample_delay'] = db.get_sample_delay()
app.config['CONFIG'] = runtime_config

observation_cache_process = multiprocessing.Process(target=cleanup_observation_cache, args=())
observation_cache_process.start()

visibility_cache_process = multiprocessing.Process(target=cleanup_visibility_cache, args=())
visibility_cache_process.start()


tart_process = multiprocessing.Process(target=tart_p, args=(runtime_config,))
tart_process.start()

#    app.run(debug=True, use_reloader=False, port=5000, host='0.0.0.0')
