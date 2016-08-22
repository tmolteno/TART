#!/usr/bin/env python
import spidev
import numpy as np
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

  VX_STREAM = 0x04
  VX_STATUS = 0x05

  AQ_DEBUG  = 0x06
  AQ_STATUS = 0x07

  SPI_STATS = 0x0C
  SPI_EXTRA = 0x0D
  SPI_MISC  = 0x0E
  SPI_RESET = 0x0F

  WRITE_CMD = 0x80


  ##--------------------------------------------------------------------------
  ##  TART SPI interface commands.
  ##--------------------------------------------------------------------------
  def __init__(self, speed=32000000):
    self.spi = spidev.SpiDev()
    self.spi.open(0, 0)
    self.spi.mode = 0b00
    self.spi.bits_per_word = 8
    self.spi.max_speed_hz = int(speed)

  def close(self):
    self.spi.close()
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
      print '\tReset issued.'
    self.pause()
    return 1

  def status(self, noisy=False):
    return self.read_status(noisy)

  def read_status(self, noisy=False):
    '''Read back the status registers of the hardware.'''
    vals = []
    regs = [self.VX_STATUS, self.AQ_DEBUG, self.AQ_STATUS,
            self.SPI_STATS, self.SPI_EXTRA, self.SPI_MISC]
    for reg in regs:
      ret = self.spi.xfer([reg, 0x0, 0x0])
      vals.append(ret[2])
      if noisy:
        print '0x%02x: %s' % (reg, tobin(ret))
    return vals

  def debug(self, on=1, noisy=False):
    '''Read the debug register, and update the debug-mode flag, and then write back the new debug register value.'''
    if on:
      val = self.spi.xfer([self.AQ_DEBUG, 0x0, 0x0])
      val = val[2] | 0x80
      self.pause()
      ret = self.spi.xfer([self.WRITE_CMD | self.AQ_DEBUG, val])
      if noisy:
        print tobin(ret)
        print 'debug now ON'
    else:
      val = self.spi.xfer([self.AQ_DEBUG, 0x0, 0x0])
      val = val[2] & 0x7F
      self.pause()
      ret = self.spi.xfer([self.WRITE_CMD | self.AQ_DEBUG, val])
      if noisy:
        print tobin(ret)
        print 'debug now OFF'
    self.pause()
    return 1


  ##--------------------------------------------------------------------------
  ##  Data aquisition settings, and streaming read-back.
  ##--------------------------------------------------------------------------
  def read_sample_delay(self, noisy=False):
    '''Read back the data sampling delays.'''
    ret = self.spi.xfer([self.AQ_DEBUG, 0x0, 0x0])
    val = ret[2] & 0x03
    if noisy:
      print tobin(ret)
    self.pause()
    return val

  def set_sample_delay(self, latency=0, noisy=False):
    '''Read the sampling-delay register, and update the delay, and then write back, the new register value.'''
    if (latency < 6 and latency >= 0):
      val = self.spi.xfer([self.AQ_DEBUG, 0x0, 0x0])
      val = val[2] & 0x80 | int(latency)
      self.pause()
      ret = self.spi.xfer([self.WRITE_CMD | self.AQ_DEBUG, val])
      self.pause()
      if noisy:
        print tobin(ret)
      return 1
    else:
      if noisy:
        print 'WARNING: latency value (%d) not within [0,5]' % latency
      return 0

  def start_acquisition(self, sleeptime=0.2, noisy=False):
    '''Enable the data-aquisition flag, and then read back the aquisition-status register, to verify that aquisition has begun.'''
    old = self.spi.xfer([self.WRITE_CMD | self.AQ_STATUS, 0x01])
    self.pause()
    ret = self.spi.xfer([self.AQ_STATUS, 0x0, 0x0])
    self.pause()
    val = ret[2] & 0x01
    if noisy:
      print 'Attempting to set aquisition-mode to ON'
      print tobin(old)
      print tobin(ret)
    if val:
      self.pause(sleeptime)       # optionally wait for aquisition to end
      res = self.spi.xfer([self.AQ_STATUS, 0x0, 0x0])
      self.pause()
      if noisy:
        print tobin(res)
      fin = res[2] & 0x03
    return val == 0x01 and fin == 0x03

  def read_data(self, num_words=2**21, blocksize=1000):
    '''Read back the requested number of 24-bit words.'''
    blk = [self.AX_STREAM, 0xff,] + [0,0,0,]*blocksize
    dat = []
    for i in range(0, int(num_words/blocksize)):
      dat += self.spi.xfer(blk)[2:]
    lst = num_words % blocksize
    dat += self.spi.xfer([self.AX_STREAM, 0xff,] + [0,0,0,]*lst)[2:]
    dat = np.array(dat).reshape(-1,3)
    return dat

  def read_test(self, noisy=True):
    '''Read back just the current 24-bit word.'''
    dat1 = self.spi.xfer([self.AX_DATA1, 0x0, 0x0])
    dat2 = self.spi.xfer([self.AX_DATA2, 0x0, 0x0])
    dat3 = self.spi.xfer([self.AX_DATA3, 0x0, 0x0])
    return [dat1[2], dat2[2], dat3[2]]


  ##--------------------------------------------------------------------------
  ##  Visibility-calculation settings, and streaming read-back.
  ##--------------------------------------------------------------------------
  def start(self, blocksize=24, noisy=False):
    self.set_blocksize(blocksize, noisy)
    self.start_aquisition(noisy)

  def set_blocksize(self, blocksize=24, noisy=False):
    '''Set the correlator blocksize to 2^blocksize.'''
    if blocksize > 0 and blocksize < 25:
      bs  = int(blocksize)
      ret = self.spi.xfer([self.WRITE_CMD | self.VX_STATUS, bs])
      self.pause()
      if noisy:
        print tobin(ret)
      return 1
    else:
      return 0

  def get_blocksize(self, noisy=False):
    '''Get the (exponent of the) correlator block-size.'''
    ret = self.spi.xfer([self.VX_STATUS, 0x0, 0x0])
    self.pause()
    if noisy:
      print tobin(ret)
    return ret[2]

  def read_visibilities(self, noisy=True):
    '''Read back visibilities data.'''
    res = self.spi.xfer([self.VX_STREAM, 0x0,] + [0x0, 0x0, 0x0, 0x0]*576)[2:]
    val = np.array(res)
    if noisy:
      print val
    return val

  def vis_ready(self, noisy=False):
    res = self.spi.xfer([self.VX_STATUS, 0x0, 0x0])
    rdy = res[2] & 0x80 != 0
    if noisy:
      print '%s\t(ready = %s)' % (res, rdy)
    return rdy

  def vis_read(self, noisy=False):
    while not self.vis_ready(noisy):
      self.pause()
    vis = self.read_visibilities(noisy)
    return vis


##----------------------------------------------------------------------------
##  Basic device querying and verification functionality.
##----------------------------------------------------------------------------
if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=8, type=float, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=11, type=int, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='operate telescope with fake antenna data.')
  parser.add_argument('--blocksize', default=24, type=int, help='exponent of correlator block-size')

  args = parser.parse_args()
  tart = TartSPI(speed=args.speed*1000000)

  print "\nTART hardware checker. Copyright Max Scheel, 2016 ."
  print "\nStatus flags:"
  tart.read_status(True)

  print "\nIssuing reset:"
  tart.reset()
  print "Status flags:"
  tart.read_status(True)

  print "\nCycling through sampling-delays:"
  for i in range(6):
    tart.set_sample_delay(i)
    tart.read_sample_delay(True)

  print "\nSetting up correlators (block-size = 2^%d):" % args.blocksize
  tart.set_blocksize(args.blocksize, True)
  tart.get_blocksize(True)

  print "\nTesting aquisition:"
  tart.debug(on=True, noisy=True)
  tart.start_acquisition(1.1, True)
  print tart.read_test(True)
  print tart.read_data(num_words=2**args.bramexp)
  print tart.read_test(True)

  print "\nReading visibilities:"
  tart.read_visibilities(True)

  print "\nStatus flags:"
  tart.read_status(True)

  print "\nDone."
