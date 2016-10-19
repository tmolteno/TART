#!/usr/bin/env python

# from tartspi import TartSPI
from tartdsp import TartSPI

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
  
  num_words = np.power(2,args.bramexp)
  tart = TartSPI(speed=args.speed*1000000)
  tart.reset(True)
  tart.reset()
  tart.debug(on=1, noisy=True)
  tart.debug(on=1)
  tart.debug(on=0)
  tart.debug(on=0)
  tart.debug(on=1)
  tart.debug(on=1)
  tart.debug(on=0)
  tart.debug(on=0)
  tart.reset()
  tart.reset()
  tart.reset()

  tart.debug(on=int(args.internal), noisy=True)
  tart.start_acquisition(sleeptime=1.1, noisy=True)

  t0   = time.time()
#   t0   = time.clock()
  data = tart.read_data(num_words=num_words, blocksize=1024)
  t1   = time.time()
#   t1   = time.clock()
  print 'elapsed time:\t%2.3f' % (t1-t0)

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
