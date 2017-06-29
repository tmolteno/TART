from tart_dsp.tartspi import *
from tart_dsp.highlevel_modes_api import *
from tart_dsp.stream_vis import *

import threading
import atexit

from flask import Flask
from flask_cors import CORS, cross_origin
from flask_jwt import JWT
from werkzeug.security import safe_str_cmp

# Add authentication to app
class User(object):
    def __init__(self, id, username, password):
        self.id = id
        self.username = username
        self.password = password

Admin = User(1,'admin','password')

def authenticate(username, password):
    if safe_str_cmp(Admin.password.encode('utf-8'), password.encode('utf-8')):
       return Admin

def identity(payload):
    return Admin


POOL_TIME = 1 #Seconds

# variables that are accessible from anywhere
runtime_config = {}
telescope_instance = {}
# lock to control access to variable
dataLock = threading.Lock()
# thread handler
telescope_thread = threading.Thread()

def create_app():
    app = Flask(__name__)
    CORS(app)
    app.config['SECRET_KEY'] = 'super-secret'
    jwt = JWT(app, authenticate, identity)


    def interrupt():
        global telescope_thread
        telescope_thread.cancel()

    def telescope_run():
        global runtime_config
        global telescope_instance
        global telescope_thread
        with dataLock:
          if runtime_config['mode'] == 'diag':
              runtime_config['acquire'] = False
              run_diagnostic(telescope_instance, runtime_config)
              print 'diag'

          elif runtime_config['mode'] == 'raw':
              runtime_config['acquire'] = True
              run_acquire_raw(telescope_instance, runtime_config)
              print 'raw'

          elif runtime_config['mode'] == 'off':
              runtime_config['acquire'] = False
              print 'offline'

          elif runtime_config['mode'] == 'hd_img':
              runtime_config['acquire'] = False
              vis_to_latest_image(telescope_instance, runtime_config)
              print 'offline'

          else:
              runtime_config['acquire'] = False
              print runtime_config['mode'], 'not implemented yet'

        # Set the next thread to happen
        telescope_thread = threading.Timer(POOL_TIME, telescope_run, ())
        telescope_thread.start()

    def telescope_init():
        # Do initialisation stuff here
        global telescope_thread
        global telescope_instance
        global runtime_config
        with dataLock:
            runtime_config['spi_speed'] = 32000000
            runtime_config['acquire'] = False
            runtime_config['shifter'] = False
            runtime_config['counter'] = False
            runtime_config['verbose'] = False
            runtime_config['centre'] = True
            runtime_config['blocksize'] = 22
            runtime_config['mode'] = 'off'
            runtime_config['raw'] = {'save': 0, \
                                    'N_samples_exp': 22,\
                                    'config'   : 'telescope_config.json',\
                                    'base_path': '.'}
            runtime_config['diagnostic'] = {'num_ant': 24, 'N_samples' : 100, 'stable_threshold' : 0.97}
            runtime_config['telescope_config_path'] = '6_ant_setup/6_antenna_board_config.json'
            runtime_config['calibration_dir'] = '6_ant_setup/'
            runtime_config['realtime_image_path'] = '/var/www/html/assets/img/image.png'
            runtime_config['chunksize'] = 2

            telescope_instance = TartSPI(speed=runtime_config['spi_speed'])
        # Create thread
        telescope_thread = threading.Timer(POOL_TIME, telescope_run, ())
        telescope_thread.start()

    # Initiate
    telescope_init()
    # When you kill Flask (SIGTERM), clear the trigger for the next thread
    atexit.register(interrupt)
    return app

def get_config():
  global runtime_config
  return runtime_config

app = create_app()

import telescope_api.views

