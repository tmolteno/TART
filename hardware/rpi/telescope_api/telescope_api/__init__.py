from tart_dsp.tartspi import *
from tart_dsp.highlevel_modes_api import *
from tart_dsp.stream_vis import *

import os

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


if os.environ.has_key('LOGIN_PW'):
  pw = os.environ['LOGIN_PW']
else:
  pw = 'password'

Admin = User(1,'admin',pw)

def authenticate(username, password):
    if safe_str_cmp(Admin.password.encode('utf-8'), password.encode('utf-8')):
       return Admin

def identity(payload):
    return Admin

def get_config():
  global runtime_config
  return runtime_config

def get_sample_delay(tart, runtime_config):
  if not runtime_config.has_key('sample_delay'):
    print 'no sample_delay determinded yet. running diagnostic'
    run_diagnostic(tart, runtime_config)


def tart_p():
  try:
      telescope_instance = TartSPI(speed=runtime_config['spi_speed'])
  except:
      print 'no spi found, just serving api'


  active = 1
  prev_mode = 'off'
  while active:
      time.sleep(0.1)
      if (prev_mode!=runtime_config['mode']):
          runtime_config['loop_idx'] = 0
          prev_mode = runtime_config['mode']

      if runtime_config['mode'] == 'off':
          runtime_config['acquire'] = False
          print 'offline'

      elif runtime_config['mode'] == 'diag':
          runtime_config['acquire'] = False
          run_diagnostic(telescope_instance, runtime_config)

      elif runtime_config['mode'] == 'raw':
          get_sample_delay(telescope_instance, runtime_config)
          runtime_config['acquire'] = True
          run_acquire_raw(telescope_instance, runtime_config)

      elif runtime_config['mode'] == 'vis':
          get_sample_delay(telescope_instance, runtime_config)
          runtime_config['acquire'] = False
          vis_to_disc(telescope_instance, runtime_config)

      elif runtime_config['mode'] == 'rt_syn_img':
          get_sample_delay(telescope_instance, runtime_config)
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

from config import init_config
runtime_config = init_config(m)

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
import telescope_api.views_acquisition
import telescope_api.views_cal
import telescope_api.views_log
