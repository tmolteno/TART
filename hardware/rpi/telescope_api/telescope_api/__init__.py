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
  try:
      telescope_instance = TartSPI(speed=runtime_config['spi_speed'])
  except:
      fake_dict = {'verbose': False, 'blocksize': 22, 'modes_available': ['off', 'diag', 'raw', 'vis', 'cal', 'rt_syn_img'], 'loop_mode_available': ['loop', 'single', 'loop_n'], 'channels': [{'phase': {'stability': 0.978, 'threshold': 0.97, 'measured': 10, 'ok': 1, 'N_samples': 100}, 'id': 0, 'radio_mean': {'threshold': 0.2, 'ok': 1, 'mean': 0.4826}}, {'phase': {'stability': 0.999, 'threshold': 0.97, 'measured': 10, 'ok': 1, 'N_samples': 100}, 'id': 1, 'radio_mean': {'threshold': 0.2, 'ok': 1, 'mean': 0.51}}, {'phase': {'stability': 0.998, 'threshold': 0.97, 'measured': 10, 'ok': 1, 'N_samples': 100}, 'id': 2, 'radio_mean': {'threshold': 0.2, 'ok': 1, 'mean': 0.4816}}, {'phase': {'stability': 0.999, 'threshold': 0.97, 'measured': 10, 'ok': 1, 'N_samples': 100}, 'id': 3, 'radio_mean': {'threshold': 0.2, 'ok': 1, 'mean': 0.4868}}, {'phase': {'stability': 0.983, 'threshold': 0.97, 'measured': 10, 'ok': 1, 'N_samples': 100}, 'id': 4, 'radio_mean': {'threshold': 0.2, 'ok': 1, 'mean': 0.5583}}, {'phase': {'stability': 0.969, 'threshold': 0.97, 'measured': 10, 'ok': 0, 'N_samples': 100}, 'id': 5, 'radio_mean': {'threshold': 0.2, 'ok': 1, 'mean': 0.4809}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 6, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 7, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 8, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 9, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 10, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 11, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 12, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 13, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 14, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 15, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 16, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 17, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 18, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 19, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 20, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 21, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 22, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}, {'phase': {'stability': 1.0, 'threshold': 0.97, 'measured': 3, 'ok': 1, 'N_samples': 100}, 'id': 23, 'radio_mean': {'threshold': 0.2, 'ok': 0, 'mean': 1.0}}], 'raw': {'save': 0, 'config': 'telescope_config.json', 'base_path': '.', 'N_samples_exp': 22}, 'realtime_image_path': '/var/www/html/assets/img/image.png', 'loop_idx': 0, 'loop_n': 5, 'loop_mode': 'loop_n', 'status': {'VX_STREAM': {'data': 176}, 'TC_STATUS': {'phase': 9, 'delta': 15}, 'SPI_STATS': {'spi_busy': 0, 'FIFO overflow': 0, 'FIFO underrun': 0}, 'TC_SYSTEM': {'enabled': 1, 'error': 0, 'locked': 1, 'source': 0}, 'TC_CENTRE': {'delay': 0, 'invert': 0, 'drift': 0, 'centre': 1}, 'timestamp (UTC)': '2017-06-30T04:44:54.327886', 'hostname': 'tart2-raspberry', 'VX_STATUS': {'available': 0, 'accessed': 0, 'overflow': 0, 'bank': 0}, 'SYS_STATS': {'viz_pend': 1, 'viz_en': 0, 'acq_en': 0, 'cap_debug': 0, 'state': 0, 'cap_en': 0}, 'TC_DEBUG': {'debug': 0, 'count': 0, 'numantenna': 24, 'shift': 0}, 'VX_DEBUG': {'stuck': 0, 'limp': 0}, 'VX_SYSTEM': {'blocksize': 0, 'enabled': 0, 'overwrite': 1}, 'AQ_STREAM': {'data': 0}, 'AQ_SYSTEM': {'SDRAM ready': 1, '512Mb': 1, 'enabled': 1, 'state': 7, 'error': 1, 'overflow': 1}}, 'chunksize': 2, 'centre': True, 'acquire': False, 'sample_delay': 9.0, 'diagnostic': {'N_samples': 100, 'stable_threshold': 0.97, 'num_ant': 24}, 'telescope_config_path': '6_ant_setup/6_antenna_board_config.json', 'shifter': False, 'calibration_dir': '6_ant_setup/', 'counter': False, 'mode': 'off', 'spi_speed': 32000000}

      for (key,val) in fake_dict.iteritems():
          runtime_config[key] = val

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
