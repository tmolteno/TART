from tart_dsp.tartspi import *
from tart_dsp.highlevel_modes_api import *
from tart_dsp.stream_vis import *

#import threading
#import atexit

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

def get_config():
  global runtime_config
  return runtime_config

def tart_p():
  telescope_instance = TartSPI(speed=runtime_config['spi_speed'])
  active = 1
  prev_mode = 'off'
  while active:
      if (prev_mode!=runtime_config['mode']):
          runtime_config['loop_idx'] = 0
          prev_mode = runtime_config['mode']

      if runtime_config['mode'] == 'off':
          runtime_config['acquire'] = False
          print 'offline'
          time.sleep(0.1)

      elif runtime_config['mode'] == 'diag':
          runtime_config['acquire'] = False
          run_diagnostic(telescope_instance, runtime_config)

      elif runtime_config['mode'] == 'raw':
          runtime_config['acquire'] = True
          run_acquire_raw(telescope_instance, runtime_config)

      elif runtime_config['mode'] == 'rt_syn_img':
          runtime_config['acquire'] = False
          vis_to_latest_image(telescope_instance, runtime_config)
      else:
          runtime_config['acquire'] = False
          print runtime_config['mode'], 'not implemented yet'

      if runtime_config['loop_mode']=='single':
          runtime_config['mode'] = 'off'

      if (runtime_config['loop_mode']=='loop_n') and (runtime_config['mode']!='off'):
          runtime_config['loop_idx'] += 1
          print runtime_config['loop_idx']
          if runtime_config['loop_idx'] == runtime_config['loop_n']:
            runtime_config['loop_idx'] = 0
            runtime_config['mode'] = 'off'

import multiprocessing
from multiprocessing import Manager

m = Manager()
runtime_config = m.dict()
runtime_config['spi_speed'] = 32000000
runtime_config['acquire'] = False
runtime_config['shifter'] = False
runtime_config['counter'] = False
runtime_config['verbose'] = False
runtime_config['centre'] = True
runtime_config['blocksize'] = 22
runtime_config['modes_available'] = ['off','diag','raw','vis', 'cal', 'rt_syn_img']
runtime_config['mode'] = 'off'
runtime_config['loop_mode'] = 'loop_n'
runtime_config['loop_mode_available'] = ['loop','single','loop_n']
runtime_config['loop_n'] = 5
runtime_config['loop_idx'] = 0

runtime_config['raw'] = {'save': 0, \
                      'N_samples_exp': 22,\
                      'config'   : 'telescope_config.json',\
                      'base_path': '.'}
runtime_config['diagnostic'] = {'num_ant': 24, 'N_samples' : 100, 'stable_threshold' : 0.97}
runtime_config['telescope_config_path'] = '6_ant_setup/6_antenna_board_config.json'
runtime_config['calibration_dir'] = '6_ant_setup/'
runtime_config['realtime_image_path'] = '/var/www/html/assets/img/image.png'
runtime_config['chunksize'] = 2

tart_process = multiprocessing.Process(target=tart_p, args=())
tart_process.start()

def create_app():
    app = Flask(__name__)
    CORS(app)
    app.config['SECRET_KEY'] = 'super-secret'
    jwt = JWT(app, authenticate, identity)
    return app

app = create_app()

import telescope_api.views
