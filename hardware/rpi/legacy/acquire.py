# SETUP

# Create local data directory and change owner to pi:
#    sudo mkdir /data
#    sudo chown pi:pi /data

from tartspi import TartSPI

import argparse
import os, errno
import numpy as np
import datetime

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


if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=8, type=int, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=21, type=float, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='operate telescope with fake antenna data.')
  parser.add_argument('--data-dir', required=True, help="The filesystem path for the telescope data.")
  parser.add_argument('--config-file', default='telescope_config.json', help="The telescope configuration file.")

  args = parser.parse_args()
  base_path = args.data_dir
  num_bytes = np.power(2,args.bramexp)

  t_SPI = TartSPI(speed=args.speed*1000000)
  t_SPI.debug(args.debug)
  t_stmp, path = create_timestamp_and_path(base_path)
  t_SPI.start_acquisition(sleeptime=3) #wait for acquisition to finish

  data = t_SPI.read_data(num_bytes=num_bytes, blocksize=1000)
  t_SPI.reset()

  print 'reshape antenna data'
  ant_data = np.flipud(np.unpackbits(data).reshape(-1,24).T)
  print ant_data[:,:15]

  config = settings.Settings(args.config_file)
  filename = path + t_stmp.strftime('%H_%M_%S.%f') + '_data.pkl'
  print 'create observation object'
  obs = observation.Observation(t_stmp, config, savedata=ant_data)
  obs.save(filename)
  print 'saved to: ', filename
