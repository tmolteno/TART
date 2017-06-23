#from tartdsp import TartSPI
import tartdsp
import time
from flask import Flask

import threading
import atexit
from flask import Flask
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

    def interrupt():
        global telescope_thread
        telescope_thread.cancel()

    def telescope_run():
        global runtime_config
        global telescope_instance
        global telescope_thread
        with dataLock:
          if runtime_config['mode'] == 'diag':
              tartdsp.run_diagnostic(telescope_instance, runtime_config)
              print 'diag'
          elif runtime_config['mode'] == 'off':
              print 'offline'

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
            telescope_instance = tartdsp.TartSPI(speed=runtime_config['spi_speed'])
        # Create thread
        telescope_thread = threading.Timer(POOL_TIME, telescope_run, ())
        telescope_thread.start()

    # Initiate
    telescope_init()
    # When you kill Flask (SIGTERM), clear the trigger for the next thread
    atexit.register(interrupt)
    return app

app = create_app()

@app.route('/status')
def get_status():
    import json
    return json.dumps(runtime_config)


@app.route('/diag')
def turn_on():
    runtime_config['mode'] = 'diag'
    return 'Diagnose Mode'


@app.route('/off')
def turn_off():
    runtime_config['mode'] = 'off'
    return 'Turned off'

#if __name__ == '__main__':
  #runtime_config = {}
  #runtime_config['spi_speed'] = 32000000
  #runtime_config['acquire'] = False
  #runtime_config['shifter'] = False
  #runtime_config['counter'] = False
  #runtime_config['verbose'] = False
  #runtime_config['centre'] = True
  #runtime_config['blocksize'] = 22

  #tart = tartdsp.TartSPI(speed=runtime_config['spi_speed'])
  #mode = 'diag'

  #while True:
    #if mode == 'diag':
      #tartdsp.run_diagnostic(tart, runtime_config)
    #elif mode == 'off':
      #print 'offline'
      #time.sleep(1)
