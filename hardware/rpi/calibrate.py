#!/usr/bin/env python
import argparse
import tartdsp


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
  parser.add_argument('--permute', action='store_true', help='permute the visibilities')
  parser.add_argument('--counter', action='store_true', help='fake data using a counter')
  parser.add_argument('--shifter', action='store_true', help='fake data using a MFSR')
  parser.add_argument('--acquire', action='store_true', help='use real antenna data')
  parser.add_argument('--source', default=0, type=int, help='antenna source to calibrate')

  args = parser.parse_args()
  tart = tartdsp.TartSPI(speed=args.speed*1000000)

  print "\nTART antenna calibration tool."
  print " Copyright Tim Molteno, Max Scheel, and Patrick Suggate, 2016 ."


  ##------------------------------------------------------------------------##
  ##  Set up a signal-capture source.
  ##------------------------------------------------------------------------##
  print "\nSetting TART's data-capture mode."

  # Prevent trying to lock all zeros.
  if not args.acquire and not args.shifter and not args.counter:
    print 'Using an MFSR for fake signal data.'
    args.shifter = True

  tart.debug(on=not args.acquire, shift=args.shifter, count=args.counter, noisy=args.verbose)
  if args.verbose:
    tart.read_status(True)


  ##------------------------------------------------------------------------##
  ##  Look for max, min, and average phase.
  ##------------------------------------------------------------------------##
  minphase = 11
  maxphase = 0
  sumphase = 0

  for a in range(24):
    tart.capture(on=True, source=a, noisy=args.verbose)

    for i in range(25):
      tart.centre (on=True, drift=True, noisy=False)

      while not tart.signal_locked():
        tart.pause()

      newphase = tart.read_phase_delay()
      if args.verbose:
        print '%d' % newphase
      minphase = min(minphase, newphase)
      maxphase = max(maxphase, newphase)
      sumphase = sumphase + newphase

      tart.centre (on=False, noisy=False)


  ##------------------------------------------------------------------------##
  ##  Show the computed phase values.
  ##------------------------------------------------------------------------##
#   avgphase = float(sumphase) / 120.0
  avgphase = float(sumphase) / 600.0
  print 'Average phase = %d (%g)' % (round(avgphase), avgphase)
  print 'Minimum phase = %d'      %  minphase
  print 'Maximum phase = %d'      %  maxphase

  tart.close()
