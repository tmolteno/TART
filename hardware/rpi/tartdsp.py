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
  AX_STREAM = 0x00
  AX_DATA1  = 0x01
  AX_DATA2  = 0x02
  AX_DATA3  = 0x03

  AQ_STATUS = 0x04
  AQ_DEBUG  = 0x06
  AQ_SYSTEM = 0x07

  VX_STREAM = 0x08
  VX_STATUS = 0x09
  VX_SYSTEM = 0x0A

  SPI_STATS = 0x78
  SPI_EXTRA = 0x79
  SPI_MISC  = 0x7A
  SPI_RESET = 0x7B

  CHECKSUM1 = 0x79
  CHECKSUM2 = 0x7A
  CHECKSUM3 = 0x7B

  VIZ_STATS = 0x6f

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
    return self.read_status(noisy)

  def read_status(self, noisy=False):
    '''Read back the status registers of the hardware.'''
    vals = []
    regs = [self.AQ_STATUS, self.AQ_DEBUG, self.AQ_SYSTEM,
            self.VX_STATUS, self.VX_SYSTEM,
            self.SPI_STATS, # self.SPI_EXTRA, self.SPI_MISC]
            self.CHECKSUM1, self.CHECKSUM2, self.CHECKSUM3]
    for reg in regs:
      ret = self.spi.xfer([reg] + [0x0]*self.LATENCY)
      val = ret[self.LATENCY]
      vals.append(val)
      if noisy:
        print self.show_status(reg, val)
#         print '0x%02x: %s' % (reg, tobin(ret))
    return vals

  def show_status(self, reg, val):
    bits = []
    vals = []
    for b in range(8):
      vals.append(val >> b)
      bits.append(vals[b] & 0x01 == 0x01)

    msgs = {
      # Acquisition registers:
      self.AQ_STATUS: 'AQ_STATUS:\taddress = %d' % val,
      self.AQ_DEBUG:  'AQ_DEBUG: \tdebug = %s, stuck = %s, limp = %s, count = %s, shift = %s' % (bits[7], bits[6], bits[5], bits[1], bits[0]),
      self.AQ_SYSTEM: 'AQ_SYSTEM:\tenabled = %s, delay = %d' % (bits[7], val & 0x7),
      # Visibilities registers:
      self.VX_STATUS: 'VX_STATUS:\tready = %s, accessed = %s, overflow = %s, block = %d' % (bits[7], bits[6], bits[5], val & 0xf),
      self.VX_SYSTEM: 'VX_SYSTEM:\tenabled = %s, overwrite = %s, blocksize = %d' % (bits[7], bits[6], val & 0x1f),
      # SPI & system registers:
      self.SPI_STATS: 'SPI_STATS:\tFIFO {underrun = %s, overflow = %s}, spireq = %s, enabled = %s, debug = %s, state = %d' % (bits[7], bits[6], bits[5], bits[4], bits[3], val & 0x07),
      self.CHECKSUM1: 'CHECKSUM1:\tchecksum[7:0]   = 0x%02x' % val,
      self.CHECKSUM2: 'CHECKSUM2:\tchecksum[15:8]  = 0x%02x' % val,
      self.CHECKSUM3: 'CHECKSUM3:\tchecksum[23:16] = 0x%02x' % val,
#       self.SPI_EXTRA: 'SPI_EXTRA:\t',
#       self.SPI_MISC:  'SPI_MISC:\t'
#       self.VIZ_STATS: 'VIZ_STATS:\twrite-block = %d, read-block = %d' % (vals[4], val & 0x0f)
      self.VIZ_STATS: 'VIZ_STATS:\tdebug = %s, enabled = %s, available = %s, overflow = %s, block = %d' % (bits[7], bits[6], bits[5], bits[4], val & 0x0f)
    }
    return msgs.get(reg, 'WARNING: Not a status register.')

  def debug(self, on=1, shift=False, count=False, noisy=False):
    '''Read the debug register, and update the debug-mode flag, and then write back the new debug register value.'''
    if on:
      val = self.spi.xfer([self.AQ_DEBUG] + [0x0]*self.LATENCY)[self.LATENCY]
      val = val | 0x80
      # Set counter mode:
      if shift:
        val = val | 0x01
      if count:
        val = val | 0x02
      self.pause()
      ret = self.spi.xfer([self.WRITE_CMD | self.AQ_DEBUG, val])
      if noisy:
        print tobin(ret)
        print ' debug now ON'
    else:
      val = self.spi.xfer([self.AQ_DEBUG] + [0x0]*self.LATENCY)
      val = val[self.LATENCY] & 0x7F
      self.pause()
      ret = self.spi.xfer([self.WRITE_CMD | self.AQ_DEBUG, val])
      if noisy:
        print tobin(ret)
        print ' debug now OFF'
    self.pause()
    return 1


  ##--------------------------------------------------------------------------
  ##  Data acquisition settings, and streaming read-back.
  ##--------------------------------------------------------------------------
  def read_sample_delay(self, noisy=False):
    '''Read back the data sampling delays.'''
    ret = self.spi.xfer([self.AQ_SYSTEM] + [0x0]*self.LATENCY)
    val = ret[self.LATENCY] & 0x07
    if noisy:
      print tobin(ret)
    self.pause()
    return val

  def set_sample_delay(self, phase=0, noisy=False):
    '''Read the sampling-delay register, and update the delay, and then write back, the new register value.'''
    if (phase < 6 and phase >= 0):
      val = self.spi.xfer([self.AQ_SYSTEM] + [0x0]*self.LATENCY)
      val = val[self.LATENCY] & 0xF8 | int(phase)
      self.pause()
      ret = self.spi.xfer([self.WRITE_CMD | self.AQ_SYSTEM, val])
      self.pause()
      if noisy:
        print tobin(ret)
      return 1
    else:
      if noisy:
        print 'WARNING: phase value (%d) not within [0,5]' % phase
      return 0

  def start_acquisition(self, sleeptime=0.2, noisy=False):
    '''Enable the data-acquisition flag, and then read back the acquisition-status register, to verify that acquisition has begun.'''
    old = self.spi.xfer([self.AQ_SYSTEM] + [0x0]*self.LATENCY)
    self.pause()
    ret = self.spi.xfer([self.AQ_STATUS | self.WRITE_CMD, 0x0, 0x80 | old[self.LATENCY]])
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

  def read_slow(self, num_words=2**21, blocksize=1024):
    '''Read back the requested number of 24-bit words.'''
    num = int(blocksize*3)
    blk = [self.AX_STREAM] + [0xff]*self.LATENCY + [0,0,0]*blocksize
    dat = numpy.zeros(num_words*3)
    ptr = int(0)
    for i in range(0, int(num_words/blocksize)):
      dat[range(ptr, ptr+num)] = self.spi.xfer(blk)[self.LATENCY:]
      ptr += num
    lst = int(num_words % blocksize)
    dat[range(ptr, ptr+lst*3)] = self.spi.xfer([self.AX_STREAM] + [0xff]*self.LATENCY + [0,0,0]*lst)[self.LATENCY:]
    return dat.reshape(-1,3)

  def read_data(self, num_words=2**21, blocksize=1000):
    '''Read back the requested number of 24-bit words.'''
    blk = [self.AX_STREAM] + [0xff]*self.LATENCY + [0,0,0]*blocksize
    dat = []
    for i in range(0, int(num_words/blocksize)):
      dat += self.spi.xfer(blk)[self.LATENCY:]
    lst = num_words % blocksize
    dat += self.spi.xfer([self.AX_STREAM] + [0xff]*self.LATENCY + [0,0,0]*lst)[self.LATENCY:]
    dat = numpy.array(dat).reshape(-1,3)
    return dat

  def read_test(self, noisy=True):
    '''Read back just the current 24-bit word.'''
    dat1 = self.spi.xfer([self.AX_DATA1] + [0x0]*self.LATENCY)
    dat2 = self.spi.xfer([self.AX_DATA2] + [0x0]*self.LATENCY)
    dat3 = self.spi.xfer([self.AX_DATA3] + [0x0]*self.LATENCY)
    return [dat1[self.LATENCY], dat2[self.LATENCY], dat3[self.LATENCY]]


  ##--------------------------------------------------------------------------
  ##  Visibility-calculation settings, and streaming read-back.
  ##--------------------------------------------------------------------------
  def start(self, blocksize=24, noisy=False):
#     self.set_blocksize_and_start(blocksize, overwrite=True, noisy)
    self.set_blocksize_and_start(blocksize, overwrite=False, noisy=noisy)
#     self.start_acquisition(noisy)

  def set_blocksize_and_start(self, blocksize=24, overwrite=False, noisy=False):
    '''Set the correlator blocksize to 2^blocksize.\nNOTE: With `overwrite` enabled, the TART device can get itself into invalid states.'''
    if blocksize > 0 and blocksize < 25:
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
    return ret[self.LATENCY] & 0x1F

  def read_visibilities(self, noisy=True):
    '''Read back visibilities data.'''
    res = self.spi.xfer([self.VX_STREAM] + [0x0]*self.LATENCY + [0x0, 0x0, 0x0, 0x0]*576)
    val = self.vis_convert(res[self.LATENCY:])
    if noisy:
      tim = time.clock()
      print self.show_status(self.VIZ_STATS, res[1])
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
    pp = numpy.loadtxt(filepath, dtype='int')
    self.perm = pp
    return pp

#endclass TartSPI


##----------------------------------------------------------------------------
##  Basic device querying and verification functionality.
##----------------------------------------------------------------------------
if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=64, type=float, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=11, type=int, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='operate telescope with fake antenna data.')
  parser.add_argument('--blocksize', default=24, type=int, help='exponent of correlator block-size')
  parser.add_argument('--status', action='store_true', help='just query the device')
  parser.add_argument('--reset', action='store_true', help='just reset the device')
  parser.add_argument('--monitor', action='store_true', help='monitor for visibilities')
  parser.add_argument('--correlate', action='store_true', help='perform a single correlation')
  parser.add_argument('--verbose', action='store_true', help='extra debug output')
  parser.add_argument('--permute', action='store_true', help='permute the visibilities')
  parser.add_argument('--counter', action='store_true', help='fake data using a counter')
  parser.add_argument('--shifter', action='store_true', help='fake data using a MFSR')

  args = parser.parse_args()
  tart = TartSPI(speed=args.speed*1000000)

  print "\nTART hardware checker. Copyright Max Scheel, 2016 ."
  print "\nStatus flags:"
  tart.read_status(True)
  if args.status:
    tart.close()
    exit(0)

  print "\nIssuing reset:"
  tart.reset()
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
    tart.debug(on=True, shift=args.shifter, count=args.counter, noisy=True)
    tart.read_status(True)

    print "Setting up correlators (block-size = 2^%d):" % args.blocksize
    tart.start(args.blocksize, True)

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
    for i in range(6):
      tart.set_sample_delay(i)
      tart.read_sample_delay(True)

    print "\nTesting acquisition:"
    tart.debug(on=True, noisy=True)
    tart.start_acquisition(1.1, True)

    print "\nSetting up correlators (block-size = 2^%d):" % args.blocksize
    tart.start(args.blocksize, True)
    tart.get_blocksize(True)

    print tart.read_test(True)
    print tart.read_data(num_words=2**args.bramexp)
    print tart.read_test(True)

    print "\nReading visibilities:"
    viz = tart.vis_read(False)
    print " Visibilities:\n%s (sum = %d)" % (viz, sum(viz))
#     tart.read_visibilities(True)

    print "\nStatus flags:"
    tart.read_status(True)

    tart.close()
    print "\nDone."
