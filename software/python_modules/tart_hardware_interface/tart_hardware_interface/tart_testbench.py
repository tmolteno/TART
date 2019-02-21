#!/usr/bin/env python

from tart_hardware_interface.tartspi import TartSPI

import numpy as np
import time
import argparse


def convert(val):
  res = 0
  for x in val:
    res = res << 8 | x
  return res


if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=32, type=int, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=10, type=float, help='exponent of bram depth')
  #parser.add_argument('--reset', action='store_true', help='reset after transmission?')
  parser.add_argument('--internal', action='store_true', help='fake data generator')
  parser.add_argument('--counter', action='store_true', help='fake data using a counter')
  parser.add_argument('--shifter', action='store_true', help='fake data using a MFSR')
  parser.add_argument('--verbose', action='store_true', help='extra debug output')
  parser.add_argument('--noresults', action='store_true', help='no results output')
  parser.add_argument('--dump', action='store_true', help='dump acquistion data')

  args = parser.parse_args()
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
  tart.start_acquisition(sleeptime=0.1, noisy=args.verbose)
  while not tart.data_ready():
    tart.pause()

  print('\nAcquisition complete, beginning read-back.')
  tart.capture(on=False, noisy=args.verbose)


  # Measure the data-transfer time.
  t0   = time.time()
#   data = tart.read_data(num_words=num_words, blocksize=800)
  data = tart.read_data(num_words=num_words, blocksize=1024)
  t1   = time.time()
  print('elapsed time:\t%2.3f' % (t1-t0))
  base = convert(data[0])
  t2   = time.time()
  if args.verbose:
    print('base = %d (%x)\n\n' % (base, base))
    print('time for convert:\t%2.3f' % (t2-t1))

  if args.dump:
    for x in data:
      val = convert(x)
      off = val - base
      print('off = %d (0x%06x)' % (off, val))

  # Display just the status flags, if results aren't wanted.
  if args.noresults:
    print("\nStatus flags:")
    tart.read_status(True)
  # Or else display the acquistion-unit's test-results.
  else:
    # Check the returned data.
    print('generate 24bit integer')
    resp_dec = (np.array(data[:,0],dtype='uint32')<<16) + (np.array(data[:,1], dtype='uint32')<<8) + (np.array(data[:,2],dtype='uint32'))
    print(np.info(resp_dec))
    print('done')
    print('first 10: ', resp_dec[:10])
    print('last 10: ', resp_dec[-10:])

    diffs = (resp_dec[1:]-resp_dec[:-1])
    diffssum = diffs.__ne__(1).sum()
    print(diffs)
    print('yo,', resp_dec[diffs.__ne__(1)])
    print('sum_of_errors: ', diffssum)
    index = np.arange(len(diffs))
    print(diffs[diffs.__ne__(1)])
    print(index[diffs.__ne__(1)])
