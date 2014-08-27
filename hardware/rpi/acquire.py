import spi

import numpy as np
import time
import argparse
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
  parser.add_argument('--bramexp', default=20.94, type=float, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='operate telescope with fake antenna data.')
  parser.add_argument('--data-directory', required=True, help="The filesystem path for the telescope data.")
  parser.add_argument('--config-file', default='telescope_config.json', help="The telescope configuration file.")

  args = parser.parse_args()
  base_path = args.data_directory

  num_bytes = np.power(2,args.bramexp)

  spi.openSPI(speed=args.speed*1000000)

  if (args.debug):
    print 'enter debug mode'
    spi.transfer((0b10001000,0b00000001))
    time.sleep(0.1)

  t_stmp, path = create_timestamp_and_path(base_path)

  print 'start acquision'
  print spi.transfer((0b10000001,0b00000001))
  time.sleep(0.5)

  print 'receiving 3x', num_bytes
  blocksize = 1000
  resp2 = []
  for i in range(0,int(num_bytes/blocksize)):
    resp2.append(spi.transfer((0b00000010,) + (0,0,0,)*blocksize)[1:])
  resp3 = spi.transfer((0b00000010,) + (0,0,0,)*(num_bytes%blocksize))[1:]
  print 'got data..'

  print 'resetting device'
  time.sleep(0.1)
  spi.transfer((0b10001111,0b00000001))
  time.sleep(0.1)

  print 'reshape data blocks'
  resp2 = np.array(resp2,dtype=int).reshape(-1,3)
  print 'reshape data remainder'
  resp3 = np.array(resp3,dtype=int).reshape(-1,3)
  print 'concatenate...'
  resp2 = np.concatenate((resp2,resp3))

  ant_data = []
  print 'shift into seperate antenna arrays'
  for i in range(8):
    ant_data.append(np.array((resp2[:,2] & 1<<(i))>0,dtype=int))
  for i in range(8):
    ant_data.append(np.array((resp2[:,1] & 1<<(i))>0,dtype=int))
  for i in range(8):
    ant_data.append(np.array((resp2[:,0] & 1<<(i))>0,dtype=int))
  for i in range(24):
    print 'antdata',i, ant_data[i]

  bipolar_data = (np.array(ant_data,dype=int) * 2) -1
  config = settings.Settings(args.config_file)
  filename = path + t_stmp.strftime('%H_%M_%S.%f') + '_data.pkl'

  obs = observation.Observation(t_stmp, bipolar_data, config)
  obs.save(filename)
  print 'saved to: ', filename
