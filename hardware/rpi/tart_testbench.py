from tartspi import TartSPI

import numpy as np
import time
import argparse

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=8, type=int, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=10, type=float, help='exponent of bram depth')
  parser.add_argument('--internal', action='store_true', help='internal generator')
  parser.add_argument('--reset', action='store_true', help='reset after transmission?')
  
  args = parser.parse_args()
  
  num_bytes = np.power(2,args.bramexp)
  t = TartSPI(speed=args.speed*1000000)
  t.reset()
  t.debug(args.internal)
  t.start_acquisition(sleeptime=3)

  data = t.read_data(num_bytes=num_bytes, blocksize=1024)
  print 'generate 24bit integer'
  resp_dec = (np.array(data[:,0],dtype='uint32')<<16) + (np.array(data[:,1], dtype='uint32')<<8) + (np.array(data[:,2],dtype='uint32'))
  print np.info(resp_dec)
  print 'done'
  print 'first 10: ', resp_dec[:10]
  print 'last 10: ', resp_dec[-10:]

  diffs = (resp_dec[1:]-resp_dec[:-1])
  diffssum = diffs.__ne__(1).sum()
  print diffs
  print 'yo,', resp_dec[diffs.__ne__(1)]
  print 'sum_of_errors: ', diffssum
  index = np.arange(len(diffs))
  print diffs[diffs.__ne__(1)]
  print index[diffs.__ne__(1)] 
