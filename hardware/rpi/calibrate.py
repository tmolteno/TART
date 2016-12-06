#!/usr/bin/env python
import argparse
import tartdsp


##--------------------------------------------------------------------------##
##
##  The calibration algorithm measures the phase-delay for each antenna, and
##  (if possible) computes the delay-value such that all data is captured
##  cleanly.
##
##--------------------------------------------------------------------------##


if __name__ == '__main__':
  print "\nTART antenna calibration tool."
  print " Copyright Tim Molteno, Max Scheel, and Patrick Suggate, 2016 ."

  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=32, type=float, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--reset', action='store_true', help='just reset the device')
  parser.add_argument('--verbose', action='store_true', help='extra debug output')
  parser.add_argument('--counter', action='store_true', help='fake data using a counter')
  parser.add_argument('--shifter', action='store_true', help='fake data using a MFSR')
  parser.add_argument('--acquire', action='store_true', help='use real antenna data')
  parser.add_argument('--source', default=-1, type=int, help='antenna source to calibrate')
  parser.add_argument('--monitor', action='store_true', help='monitor for visibilities')
  parser.add_argument('--duration', default=20, type=int, help='duration (s) to monitor signal')
  parser.add_argument('--samples', default=25, type=int, help='number of samples to measure.')

  # TODO:
  parser.add_argument('--phases', default='', type=str, help='file of antenna phase-delays')

  args = parser.parse_args()
  tart = tartdsp.TartSPI(speed=args.speed*1000000)
  tart.reset(noisy=args.verbose)


  ##------------------------------------------------------------------------##
  ##  Set up a signal-capture source.
  ##------------------------------------------------------------------------##
  print "\nSetting TART's data-capture mode."

  # Prevent trying to lock all zeros.
  if not args.acquire and not args.shifter and not args.counter:
    print 'Using an MFSR for fake signal data.'
    args.shifter = True

  tart.debug(on=not args.acquire, shift=args.shifter, count=args.counter,
             noisy=args.verbose)


  ##------------------------------------------------------------------------##
  ##  See how long it takes to lose lock.
  ##------------------------------------------------------------------------##
  if args.monitor:
    tart.capture(on=True, source=args.source, noisy=args.verbose)
    tart.centre (on=True, drift=False, noisy=False)

    ticks = 0
    args.duration = args.duration*200
    while tart.signal_locked() and ticks < args.duration:
      if ticks % 1000 == 0 and args.verbose:
        tart.read_status(True)
      ticks = ticks + 1
      if ticks % 100 == 0 and args.verbose:
        print ' ticks =\t%5d\r' % ticks
      tart.pause()

    print 'Lock-ticks = %d (%g seconds)' % (ticks, ticks*0.005)
    exit(0)


  if args.verbose:
    tart.read_status(True)


  ##------------------------------------------------------------------------##
  ##  Look for max, min, and average phase.
  ##------------------------------------------------------------------------##
  minphase = 11
  maxphase = 0
  sumphase = 0

  SAMPLES  = args.samples

  if args.source == -1:
    channels = range(24)
  else:
    channels = [args.source]

  for a in channels:
    tart.capture(on=True, source=a, noisy=args.verbose)
    tart.centre (on=True, invert=False, drift=False, noisy=False)

    while not tart.signal_locked():
      tart.pause()

    phases = tart.read_phases(SAMPLES)
    if args.verbose:
      print '%s' % phases
    minphase  = min(minphase, min(phases))
    maxphase  = max(maxphase, max(phases))
    sumphase += sum(phases)

  tart.centre (on=False, noisy=False)


  if args.verbose:
    tart.read_status(True)


  ##------------------------------------------------------------------------##
  ##  Show the computed phase values.
  ##------------------------------------------------------------------------##
  avgphase = float(sumphase) / (float(SAMPLES) * len(channels))
  print 'Average phase = %d (%g)' % (round(avgphase), avgphase)
  print 'Minimum phase = %d'      %  minphase
  print 'Maximum phase = %d'      %  maxphase

  tart.close()
