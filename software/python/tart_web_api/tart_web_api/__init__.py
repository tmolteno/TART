## Maximilian Scheel (c) 2017
## max@max.ac.nz
#import multiprocessing

#from flask import Flask, jsonify, request
#from flask_jwt_extended import JWTManager

#from flask_cors import CORS, cross_origin


#from tart_web_api.service import cleanup_observation_cache, cleanup_visibility_cache, TartControl
#from tart_web_api.config import init_config
#import tart_web_api.database as db

#db.setup_db()

#m = multiprocessing.Manager()
#runtime_config = init_config(m)
#runtime_config['sample_delay'] = db.get_sample_delay()

#def get_config():
    #global runtime_config
    #return runtime_config

#def tart_p():
    #tart_control = TartControl(runtime_config)
    #while True:
        #tart_control.set_state(runtime_config['mode'])
        #tart_control.run()

#observation_cache_process = multiprocessing.Process(target=cleanup_observation_cache, args=())
#observation_cache_process.start()

#visibility_cache_process = multiprocessing.Process(target=cleanup_visibility_cache, args=())
#visibility_cache_process.start()


#tart_process = multiprocessing.Process(target=tart_p, args=())
#tart_process.start()

#app = Flask(__name__)
#CORS(app)
##app.config['SECRET_KEY'] = 'super-secret-cow-key-hsa'
#from datetime import timedelta as td
##app.config['JWT_EXPIRATION_DELTA'] = td(seconds=3600)
##JWT(app, authenticate, identity)

#app.secret_key = 'super-secret-123897219379179464asd13khk213'  # Change this!
#app.config['JWT_HEADER_TYPE'] = 'JWT'
#jwt = JWTManager(app)

#import tart_web_api.views
#import tart_web_api.views_auth
#import tart_web_api.views_acquisition
#import tart_web_api.views_cal
#import tart_web_api.views_cache
#import tart_web_api.views_obs
##import tart_web_api.views_log
#import tart_web_api.views_channel
