#!/usr/bin/env python

from tart_dsp.tartspi import *
from tart_dsp.highlevel_modes_api import *

import argparse


##----------------------------------------------------------------------------
##  Basic device querying and verification functionality.
##----------------------------------------------------------------------------
if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=32, type=float, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=11, type=int, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='operate telescope with fake antenna data')
  parser.add_argument('--blocksize', default=24, type=int, help='exponent of correlator block-size')
  parser.add_argument('--status', action='store_true', help='just query the device')
  parser.add_argument('--reset', action='store_true', help='just reset the device')
  parser.add_argument('--monitor', action='store_true', help='monitor for visibilities')
  parser.add_argument('--correlate', action='store_true', help='perform a single correlation')
  parser.add_argument('--verbose', action='store_true', help='extra debug output')
  parser.add_argument('--counter', action='store_true', help='fake data using a counter')
  parser.add_argument('--shifter', action='store_true', help='fake data using a MFSR')
  parser.add_argument('--acquire', action='store_true', help='use real antenna data')
  parser.add_argument('--source', default=0, type=int, help='antenna source to calibrate')
  parser.add_argument('--capture', action='store_true', help='just enable the data-capture unit')
  parser.add_argument('--centre', action='store_true', help='enable the clock-recovery unit')
  parser.add_argument('--setdelay', default=0, type=int, help='set signal phase-delay')
  parser.add_argument('--diagnostic', action='store_true', help='run diagnostic')

  # TODO:
  parser.add_argument('--phases', default='', type=str, help='file of antenna phase-delays')

  args = parser.parse_args()

  runtime_config = {}
  runtime_config['acquire'] = args.acquire
  runtime_config['shifter'] = args.shifter
  runtime_config['counter'] = args.counter
  runtime_config['verbose'] = args.verbose
  runtime_config['centre'] = args.centre
  runtime_config['blocksize'] = args.blocksize

  tart = TartSPI(speed=args.speed*1000000)


  ##------------------------------------------------------------------------##
  ##  Just enable data-capture and quit, if given option '--capture'.
  if args.capture:
    print "\nSetting TART's data-capture mode."
    tart.debug(on=not args.acquire, shift=args.shifter, count=args.counter, noisy=args.verbose)
    tart.capture(source=args.source, noisy=args.verbose)
    tart.centre(args.centre, noisy=not args.verbose)
    if args.verbose:
      tart.read_status(True)
    tart.close()
    exit(0)


  ##------------------------------------------------------------------------##
  ##  Read status and then reset.
  print "\nTART hardware checker. Copyright Max Scheel, 2016 ."
  if args.status or args.verbose:
    print "\nStatus flags:"
    tart.read_status(True)
    if args.status:
      tart.close()
      exit(0)

  print "\nIssuing reset."
  tart.reset()
  if args.reset or args.verbose:
    print "Status flags:"
    tart.read_status(True)
    if args.reset:
      tart.close()
      exit(0)

  ##------------------------------------------------------------------------##
  ##  Set the phase-delay, for the data-capture circuit.
  if args.verbose:
    print "\nSetting the input phase-delay (allowable 0-11)"
  tart.set_sample_delay(args.setdelay)
  tart.read_sample_delay(args.verbose)

  ##------------------------------------------------------------------------##
  ##  Monitor the visibilities, or perform just a single correlation?
  if args.monitor or args.correlate:
    print "\nLoading permutation vector"
    pp = tart.load_permute()

    print "Enabling DEBUG mode"
    tart.debug(on=not args.acquire, shift=args.shifter, count=args.counter, noisy=args.verbose)

    print "Setting capture registers:"
    tart.capture(on=True, source=args.source, noisy=args.verbose)
    tart.centre(args.centre, noisy=args.verbose)

    print "Setting up correlators (block-size = 2^%d):" % args.blocksize
    tart.start(args.blocksize, True)

    if args.verbose:
      print "\nStatus flags:"
      tart.read_status(True)

    print "Monitoring visibilities:"
    import time
    ts =  time.time()
    while True:
      if args.verbose:
        tart.vis_read(True)
        tart.read_status(True)
      else:
        viz = tart.vis_read(False)
        ts_now = time.time() - ts
        print " Reordered (   permuted) visibilities (@t = %g):\n%s (sum = %d)" % (ts_now, viz[pp], sum(viz))
      if args.correlate:
        tart.close()
        exit(0)

  elif args.diagnostic:
    from highlevel_modes_api import run_diagnostic
    run_diagnostic(tart, runtime_config)
    tart.close()


  else:
    print "\nCycling through sampling-delays:"
    for i in range(12):
      tart.set_sample_delay(i)
      tart.read_sample_delay(True)

    print "\nTesting acquisition."
    tart.debug(on=True, noisy=args.verbose)
    tart.capture(on=True, source=args.source, noisy=args.verbose)
    tart.start_acquisition(1.1, True)

    print "\nSetting up correlators (block-size = 2^%d)" % args.blocksize
    tart.start(args.blocksize, args.verbose)
    tart.get_blocksize(args.verbose)

    print tart.read_data(num_words=2**args.bramexp)

    print "\nStatus flags:"
    tart.read_status(True)

    print "\nReading visibilities:"
    viz = tart.vis_read(False)
    print " Visibilities:\n%s (sum = %d)" % (viz, sum(viz))
#     tart.read_visibilities(True)

    print "\nStatus flags:"
    tart.read_status(True)

    tart.close()
    print "\nDone."
