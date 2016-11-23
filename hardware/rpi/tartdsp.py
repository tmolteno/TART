#!/usr/bin/env python
import spidev
import numpy
import time
import argparse


def tobin(arr):
  return [bin(i) for i in arr]


class TartSPI:
  '''Command for configuring, and querying TART hardware.'''

  ##--------------------------------------------------------------------------
  ##  TART SPI control, data, and status registers.
  ##--------------------------------------------------------------------------
  TC_CENTRE = 0x00
  TC_STATUS = 0x01
  TC_DEBUG  = 0x02
  TC_SYSTEM = 0x03

  AQ_STREAM = 0x20
  AQ_SYSTEM = 0x23

  VX_STREAM = 0x40
  VX_STATUS = 0x41
  VX_DEBUG  = 0x42
  VX_SYSTEM = 0x43

  SYS_STATS = 0x60
  SPI_STATS = 0x61
  SPI_RESET = 0x63

  NEWLINE   = 0x7F
  WRITE_CMD = 0x80

  LATENCY   = 2

  ##--------------------------------------------------------------------------
  ##  TART SPI interface commands.
  ##--------------------------------------------------------------------------
  def __init__(self, speed=32000000):
    self.spi = spidev.SpiDev()
    self.spi.open(0, 0)
    self.spi.mode = 0b00
    self.spi.bits_per_word = 8
    self.spi.max_speed_hz = int(speed)
    self.perm = None

  def close(self, noisy=False):
    self.spi.close()
    if noisy:
      print 'SPI <-> TART interface closed.'
    return 1

  def pause(self, duration=0.005, noisy=False):
    if noisy:
      print ' pausing for %1.3fs' % duration
    time.sleep(duration)


  ##--------------------------------------------------------------------------
  ##  TART system commands.
  ##--------------------------------------------------------------------------
  def reset(self, noisy=False):
    '''Issue a global reset to the TART hardware.'''
    ret = self.spi.xfer([self.WRITE_CMD | self.SPI_RESET, 0x01])
    if noisy:
      print tobin(ret)
      print ' reset issued.'
    self.pause()
    return 1

  def status(self, noisy=False):
    '''Query all TART (SPI-mapped) registers.'''
    return self.read_status(noisy)

  def read_status(self, noisy=False):
    '''Read back the status registers of the hardware.'''
    vals = []
    regs = [self.TC_CENTRE, self.TC_STATUS, self.TC_DEBUG, self.TC_SYSTEM,
            self.NEWLINE,
            self.AQ_STREAM,                                self.AQ_SYSTEM,
            self.NEWLINE,
            self.VX_STREAM, self.VX_STATUS, self.VX_DEBUG, self.VX_SYSTEM,
            self.NEWLINE,
            self.SYS_STATS, self.SPI_STATS] #, self.SPI_RESET]
    for reg in regs:
      ret = self.spi.xfer([reg] + [0x0]*self.LATENCY)
      val = ret[self.LATENCY]
      vals.append(val)
      if noisy:
        print self.show_status(reg, val)
#         print '0x%02x: %s' % (reg, tobin(ret))
    return vals

  def show_status(self, reg, val):
    '''Generates a human-readable string from the given register number and contents.'''
    bits = []
    vals = []
    for b in range(8):
      vals.append(val >> b)
      bits.append(vals[b] & 0x01 == 0x01)

    msgs = {
      # Capture registers:
      self.TC_CENTRE: 'TC_CENTRE:\tcentre = %s, drift = %s, delay = %d' % (bits[7], bits[6], val & 0x0f),
      self.TC_STATUS: 'TC_STATUS:\tinvalid = %s, locked = %s, phase = %d' % (bits[7], bits[6], val & 0x0f),
      self.TC_DEBUG:  'TC_DEBUG: \tdebug = %s, count = %s, shift = %s, #antenna = %d' % (bits[7], bits[1], bits[0], val & 0x1f),
      self.TC_SYSTEM: 'TC_SYSTEM:\tenabled = %s, source = %d' % (bits[7], val & 0x01f),

      # Acquisition registers:
      self.AQ_STREAM: 'AQ_STREAM:\tdata = %x' % val,
      self.AQ_SYSTEM: 'AQ_SYSTEM:\tenabled = %s, error = %s, ready = %s, 512Mb = %s, state = %d' % (bits[7], bits[6], bits[5], bits[4], val & 0x07),

      # Visibilities registers:
      self.VX_STREAM: 'VX_STREAM:\tdata = %x' % val,
      self.VX_STATUS: 'VX_STATUS:\tavailable = %s, accessed = %s, overflow = %s, bank = %d' % (bits[7], bits[6], bits[5], val & 0xf),
      self.VX_DEBUG:  'VX_DEBUG: \tstuck = %s, limp = %s' % (bits[7], bits[6]),
      self.VX_SYSTEM: 'VX_SYSTEM:\tenabled = %s, overwrite = %s, blocksize = %d' % (bits[7], bits[6], val & 0x1f),

      # System register:
      self.SYS_STATS: 'SYS_STATS:\tviz_en = %s, viz_pend = %s, cap_en = %s, cap_debug = %s, acq_en = %s, state = %d' % (bits[7], bits[6], bits[5], bits[4], bits[3], val & 0x07),

      # SPI & system registers:
      self.SPI_STATS: 'SPI_STATS:\tFIFO {overflow = %s, underrun = %s}, spi_busy = %s' % (bits[7], bits[6], bits[0]),
      self.SPI_RESET: 'SPI_RESET:\treset = %s' % bits[0],

      # Miscellaneous:
      self.NEWLINE: '\r'
    }
    return msgs.get(reg, 'WARNING: Not a status register.')


  ##--------------------------------------------------------------------------
  ##  Data-capture & clock-recovery settings.
  ##--------------------------------------------------------------------------
  def capture(self, on=True, source=0, noisy=False):
    '''Enable/disable the data-capture unit.'''
    if on:
      val = 0x80 | (source & 0x1f)
      flg = 'ENABLED'
    else:
      val = source & 0x1f
      flg = 'DISABLED'

    ret = self.spi.xfer([self.WRITE_CMD | self.TC_SYSTEM] + [val])
    if noisy:
      print ' capture %s' % flg
    return ret

  def debug(self, on=True, shift=False, count=False, noisy=False):
    '''Read the debug register, and update the debug-mode flag, and then write back the new debug register value.'''
    if on:
      val = self.spi.xfer([self.TC_DEBUG] + [0x0]*self.LATENCY)[self.LATENCY]
      val = val | 0x80
      # Set counter mode:
      if shift:
        val = val | 0x20
      if count:
        val = val | 0x40
      self.pause()
      ret = self.spi.xfer([self.WRITE_CMD | self.TC_DEBUG, val])
      if noisy:
        print tobin(ret)
        print ' debug now ON'
    else:
      val = self.spi.xfer([self.TC_DEBUG] + [0x0]*self.LATENCY)
      val = val[self.LATENCY] & 0x7F
      self.pause()
      ret = self.spi.xfer([self.WRITE_CMD | self.TC_DEBUG, val])
      if noisy:
        print tobin(ret)
        print ' debug now OFF'
    self.pause()
    return 1

  def centre(self, centre=True, drift=False, delay=0, noisy=False):
    '''Control the clock-recovery and centring unit.'''
    return -1

  def read_sample_delay(self, noisy=False):
    '''Read back the data sampling delays.'''
    ret = self.spi.xfer([self.TC_CENTRE] + [0x0]*self.LATENCY)
    val = ret[self.LATENCY] & 0x0f
    if noisy:
      print tobin(ret)
    self.pause()
    return val

  def set_sample_delay(self, phase=0, noisy=False):
    '''Read the sampling-delay register, and update the delay, and then write back, the new register value.'''
    if (phase < 12 and phase >= 0):
      val = self.spi.xfer([self.TC_CENTRE] + [0x0]*self.LATENCY)
      val = val[self.LATENCY] & 0xF8 | int(phase)
      self.pause()
      ret = self.spi.xfer([self.WRITE_CMD | self.TC_CENTRE, val])
      self.pause()
      if noisy:
        print tobin(ret)
      return 1
    else:
      if noisy:
        print 'WARNING: phase value (%d) not within [0,5]' % phase
      return 0

  def read_phase_delay(self, noisy=False):
    '''Read back the current signal/antenna phase-delay.'''
    ret = self.spi.xfer([self.TC_STATUS] + [0x0]*self.LATENCY)
    val = ret[self.LATENCY] & 0x0f
    if noisy:
      print tobin(ret)
    self.pause()
    return val


  ##--------------------------------------------------------------------------
  ##  Data acquisition settings, and streaming read-back.
  ##--------------------------------------------------------------------------
  def start_acquisition(self, sleeptime=0.2, noisy=False):
    '''Enable the data-acquisition flag, and then read back the acquisition-status register, to verify that acquisition has begun.'''
    old = self.spi.xfer([self.AQ_SYSTEM] + [0x0]*self.LATENCY)
    self.pause()
    ret = self.spi.xfer([self.AQ_SYSTEM | self.WRITE_CMD, 0x0, 0x80 | old[self.LATENCY]])
    self.pause()

    res = self.spi.xfer([self.AQ_SYSTEM] + [0x0]*self.LATENCY)
    val = (res[self.LATENCY] >> 7) == 0x01
    if noisy:
      print ' attempting to set acquisition-mode to ON'
      print tobin(old)
      print tobin(ret)
    if val:
      self.pause(sleeptime)       # optionally wait for acquisition to end
      res = self.spi.xfer([self.SPI_STATS] + [0x0]*self.LATENCY)
      self.pause()
      if noisy:
        print tobin(res)
      fin = res[self.LATENCY] & 0x03 == 0x03
    return val and fin

  def read_data(self, num_words=2**21, blocksize=1000):
    '''Read back the requested number of 24-bit words.'''
    blk = [self.AQ_STREAM] + [0xff]*self.LATENCY + [0,0,0]*blocksize
    dat = []
    for i in range(0, int(num_words/blocksize)):
      dat += self.spi.xfer(blk)[self.LATENCY:]
    lst = num_words % blocksize
    dat += self.spi.xfer([self.AQ_STREAM] + [0xff]*self.LATENCY + [0,0,0]*lst)[self.LATENCY:]
    dat = numpy.array(dat).reshape(-1,3)
    return dat


  ##--------------------------------------------------------------------------
  ##  Visibility-calculation settings, and streaming read-back.
  ##--------------------------------------------------------------------------
  def start(self, blocksize=24, noisy=False):
#     self.set_blocksize_and_start(blocksize, overwrite=True, noisy)
    self.set_blocksize_and_start(blocksize, overwrite=False, noisy=noisy)
#     self.start_acquisition(noisy)

  def set_blocksize_and_start(self, blocksize=24, overwrite=False, noisy=False):
    '''Set the correlator blocksize to 2^blocksize.\nNOTE: With `overwrite` enabled, the TART device can get itself into invalid states.'''
    if blocksize > 9 and blocksize < 25:
      if overwrite:
        ow = 0x40
      else:
        ow = 0x00
      bs  = 0x80 | ow | int(blocksize)
      ret = self.spi.xfer([self.WRITE_CMD | self.VX_SYSTEM, bs])
      self.pause()
      if noisy:
        print tobin(ret)
      return 1
    else:
      return 0

  def get_blocksize(self, noisy=False):
    '''Get the (exponent of the) correlator block-size.'''
    ret = self.spi.xfer([self.VX_SYSTEM] + [0x0]*self.LATENCY)
    self.pause()
    if noisy:
      print tobin(ret)
    return ret[self.LATENCY] & 0x1f

  def read_visibilities(self, noisy=True):
    '''Read back visibilities data.'''
    res = self.spi.xfer([self.VX_STREAM] + [0x0]*self.LATENCY + [0x0, 0x0, 0x0, 0x0]*576)
    val = self.vis_convert(res[self.LATENCY:])
    if noisy:
      tim = time.clock()
      print self.show_status(self.SYS_STATS, res[1])
      print " Visibilities (@t = %g):\n%s (sum = %d)" % (tim, val, sum(val))
    return val

  def vis_ready(self, noisy=False):
    res = self.spi.xfer([self.VX_STATUS] + [0x0]*self.LATENCY)
    rdy = res[self.LATENCY] & 0x80 != 0
    if noisy:
      print '%s   \t(ready = %s)' % (tobin(res), rdy)
    return rdy

  def vis_read(self, noisy=False):
#     while not self.vis_ready(noisy):
    while not self.vis_ready(False):
      self.pause()
    vis = self.read_visibilities(noisy)
    return vis

  def vis_convert(self, viz):
    arr = numpy.zeros(576, dtype='int')
    for i in range(0,576):
      j = i*4
      x = viz[j] | (viz[j+1] << 8) | (viz[j+2] << 16) | ((viz[j+3] & 0x7f) << 24)
      if viz[j+3] > 0x7f:
        x = -x
      arr[i] = x
    return arr

  def load_permute(self, filepath='../FPGA/tart_spi/data/permute.txt', noisy=False):
    '''Load a permutation vector from the file at the given filepath.'''
    if self.perm is None:
      pp = numpy.loadtxt(filepath, dtype='int')
      self.perm = pp
    return self.perm

#endclass TartSPI


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
  parser.add_argument('--permute', action='store_true', help='permute the visibilities')
  parser.add_argument('--counter', action='store_true', help='fake data using a counter')
  parser.add_argument('--shifter', action='store_true', help='fake data using a MFSR')
  parser.add_argument('--acquire', action='store_true', help='use real antenna data')
  parser.add_argument('--source', default=0, type=int, help='antenna source to calibrate')
#   parser.add_argument('--capture', action='store_true', help='enable the data-capture unit')

  args = parser.parse_args()
  tart = TartSPI(speed=args.speed*1000000)

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

  if args.monitor or args.correlate:
    print "\nLoading permutation vector"
    pp = tart.load_permute()
#     pp = numpy.array(range(0,576), dtype='int')
#     print pp

    print "Enabling DEBUG mode"
    tart.debug(on=not args.acquire, shift=args.shifter, count=args.counter, noisy=args.verbose)

    print "Setting capture registers:"
    tart.capture(on=True, noisy=args.verbose)

    print "Setting up correlators (block-size = 2^%d):" % args.blocksize
    tart.start(args.blocksize, True)

    if args.verbose:
      print "\nStatus flags:"
      tart.read_status(True)

    print "Monitoring visibilities:"
    while True:
      if args.verbose:
        tart.vis_read(True)
        tart.read_status(True)
      else:
        viz = tart.vis_read(False)
        tim = time.clock()
        print " Reordered visibilities (@t = %g):\n%s (sum = %d)" % (tim, viz[pp], sum(viz))
      if args.correlate:
        tart.close()
        exit(0)

  else:
    print "\nCycling through sampling-delays:"
    for i in range(12):
      tart.set_sample_delay(i)
      tart.read_sample_delay(True)

    print "\nTesting acquisition."
    tart.debug(on=True, noisy=args.verbose)
    tart.capture(on=True, noisy=args.verbose)
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
