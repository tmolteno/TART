# SETUP

# Create local data directory and change owner to pi:
#    sudo mkdir /data
#    sudo chown pi:pi /data

import os, errno
import numpy as np
import datetime

from tartdsp import TartSPI

from tart.operation import observation
from tart.operation import settings


def mkdir_p(path): # Emulate mkdir -p functionality in python
  try:
    os.makedirs(path)
  except OSError as exc:
    if exc.errno == errno.EEXIST and os.path.isdir(path):
      pass
    else: raise

def create_timestamp_and_path(base_path):
  ts = datetime.datetime.utcnow()   # Timestamp information for directory structure
  # Create a meaningful directory structure to organize recorded data
  p = base_path + '/' + str(ts.year) +'/' + str(ts.month) + '/' + str(ts.day) + '/'
  mkdir_p(p)
  # Call timestamp again (the directory name will not have changed, but the timestamp will be more accurate)
  ts = datetime.datetime.utcnow()
  return ts, p

import argparse

def antenna_health(ant_data, epsilon=0.3):
  ret = np.abs(np.mean(ant_data, axis=1) - 0.5).__gt__(epsilon)
  return ret

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=32, type=int, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=21, type=float, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='operate telescope with fake antenna data.')
  parser.add_argument('--data-dir', required=True, help="The filesystem path for the telescope data.")
  parser.add_argument('--config-file', default='telescope_config.json', help="The telescope configuration file.")
  parser.add_argument('--reset', action='store_true', help='reset after transmission?')
  parser.add_argument('--internal', action='store_true', help='fake data generator')
  parser.add_argument('--counter', action='store_true', help='fake data using a counter')
  parser.add_argument('--shifter', action='store_true', help='fake data using a MFSR')
  parser.add_argument('--verbose', action='store_true', help='extra debug output')
  parser.add_argument('--systemcheck', action='store_true', help='')


  args = parser.parse_args()

  base_path = args.data_dir

  num_words = np.power(2, args.bramexp)

  # Initialise the TART hardware, and place it into a known state.
  tart = TartSPI(speed=args.speed*1000000)
  tart.reset()
  tart.debug(on= True, shift=args.shifter, count=args.counter, noisy=args.verbose)
  tart.debug(on=False, shift=args.shifter, count=args.counter, noisy=args.verbose)
  tart.reset(noisy=args.verbose)

  # Enable data-capture, and then raw-data acquistion.
  tart.debug(on=args.internal, shift=args.shifter, count=args.counter, noisy=args.verbose)
  tart.capture(on=True, noisy=args.verbose)


  t_stmp, path = create_timestamp_and_path(base_path)
  tart.start_acquisition(sleeptime=0.1, noisy=args.verbose)
  while not tart.data_ready():
    tart.pause()

  print '\nAcquisition complete, beginning read-back.'
  tart.capture(on=False, noisy=args.verbose)

  data = tart.read_data(num_words=num_words, blocksize=1024)

  print 'reshape antenna data'
  data = np.asarray(data,dtype=np.uint8)
  ant_data = np.flipud(np.unpackbits(data).reshape(-1,24).T)
  if args.systemcheck:
    ant_status_array = antenna_health(ant_data)
    messages = ['OK','mean not within tolerance',]
    for i, ant in enumerate(ant_status_array):
      if ant:
        print 'Problem with antenna', i, messages[ant]

  config = settings.Settings(args.config_file)
  filename = path + t_stmp.strftime('%H_%M_%S.%f') + '_data.pkl'
  print 'create observation object'
  obs = observation.Observation(t_stmp, config, savedata=ant_data)
  obs.save(filename)
  print 'saved to: ', filename
