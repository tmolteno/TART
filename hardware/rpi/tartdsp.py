#!/usr/bin/env python
import spidev
import numpy as np
import time

def tobin(arr):
  return [bin(i) for i in arr]

class TartSPI:

  # TART SPI control and status registers:
  AX_STREAM = 0x00
  AX_DATA1  = 0x01
  AX_DATA2  = 0x02
  AX_DATA3  = 0x03

  VX_STREAM = 0x04
  VX_STATUS = 0x05

  AQ_DEBUG  = 0x06
  AQ_STATUS = 0x07

  SPI_RESET = 0x0F

  WRITE_CMD = 0x80

  def __init__(self, speed=32000000):
    self.spi = spidev.SpiDev()
    self.spi.open(0, 0)
    self.spi.mode = 0b00
    self.spi.bits_per_word = 8
    self.spi.max_speed_hz = int(speed)

  def reset(self):
    ret = self.spi.xfer([WRITE_CMD | SPI_RESET, 0x01])
#     print tobin(ret)
    time.sleep(0.005)
    print 'reset command has been sent to the device'
    return 1
 
  def read_sample_delay(self):
    ret = self.spi.xfer([AQ_STATUS, 0x0, 0x0])
#     print tobin(ret)
    delay = ret[1]
    time.sleep(0.005)
    return delay

  # TODO: Read the debug register, and update the delay, and then write back
  #   the new debug register value.
  def set_sample_delay(self, n_fast_clk_cycles=0):
    if ((n_fast_clk_cycles<6) and (n_fast_clk_cycles>=0)):
      self.spi.xfer([WRITE_CMD | AQ_DEBUG, n_fast_clk_cycles])
      time.sleep(0.005)
      return 1
    else:
      return 0

  # TODO: Read the debug register, and update the debug-mode flag, and then
  #   write back the new debug register value.
  def debug(self,on=1):
    if on:
      ret = self.spi.xfer([WRITE_CMD | AQ_DEBUG, 0x80])
#       print tobin(ret)
      print 'debug now ON'
    else:
      ret = self.spi.xfer([WRITE_CMD | AQ_DEBUG, 0x00])
#       print tobin(ret)
      print 'debug now OFF'
    time.sleep(0.005)
    return 1

  def start_acquisition(self,sleeptime=0.2):
    self.spi.xfer([WRITE_CMD | AQ_STATUS, 0x01])
    time.sleep(sleeptime)
    return 1

  def read_data(self, num_bytes=2**21, blocksize=1000):
    resp2 = []
    for i in range(0,int(num_bytes/blocksize)):
      resp2.append(self.spi.xfer([AX_STREAM, 0xff,] + [0,0,0,]*blocksize)[2:])
    resp3 = self.spi.xfer([AX_STREAM, 0xff,] + [0,0,0,]*(num_bytes%blocksize))[2:]
    resp2 = np.concatenate(resp2).reshape(-1,3)
    resp3 = resp3.reshape(-1,3)
    ret = np.concatenate((resp2,resp3))
    #print 'read', np.shape(ret)
    return ret

  def close(self):
    self.spi.close()
    return 1

def lags(a,b,lag_max=10):
  ret = []
  for i in range(lag_max):
    ret.append(np.abs(np.dot(a[i:],b[:len(b)-i]))/(1.*len(a)))
  print np.argmax(ret)
  return np.array(ret)

import argparse
if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=8, type=float, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=11, type=int, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='operate telescope with fake antenna data.')
  args = parser.parse_args()

  t_SPI = TartSPI(speed=args.speed*1000000)
  import time
  for i in range(6):
    t_SPI.reset()
    time.sleep(1)
    t_SPI.read_sample_delay()
    time.sleep(0.5)
    t_SPI.set_sample_delay(i)
    time.sleep(0.2)
    t_SPI.read_sample_delay()
    time.sleep(1)
    print '__________'
  t_SPI.start_acquisition()
  time.sleep(1)
  print t_SPI.read_data(num_bytes=2**10)
