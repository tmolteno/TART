import spinumpy as spi
import numpy as np
import time

def tobin(arr):
  return [bin(i) for i in arr]

class TartSPI:
  def __init__(self, speed=8000000):
    spi.openSPI(speed=int(speed))

  def reset(self):
    ret = spi.transfer((0x8f,0x01))
    print tobin(ret)
    time.sleep(0.005)
    print 'device now resetted'
    return 1
 
  def read_sample_delay(self):
    ret = spi.transfer((0x5,0x0,0x0))
    print tobin(ret)
    delay = ret[1]
    time.sleep(0.005)
    return delay
 
  def set_sample_delay(self, n_fast_clk_cycles=0):
    if ((n_fast_clk_cycles<6) and (n_fast_clk_cycles>=0)):
      spi.transfer((0x85,n_fast_clk_cycles))
      time.sleep(0.005)
      return 1
    else:
      return 0


  def debug(self,on=1):
    if on:
      ret = spi.transfer((0x86,0x01))
      print tobin(ret)
      print 'debug now on'
    else:
      ret = spi.transfer((0x86,0x00))
      print tobin(ret)
      print 'debug now off'
    time.sleep(0.005)
    return 1

  def start_acquisition(self,sleeptime=0.2):
    spi.transfer((0x87,0x01))
    #print sleeptime
    time.sleep(sleeptime)
    #print 'acquision done'
    return 1

  def read_data(self, num_bytes=2**21, blocksize=1000):
    resp2 = []
    for i in range(0,int(num_bytes/blocksize)):
      resp2.append(spi.transfer((0x03,0xff,) + (0,0,0,)*blocksize)[2:])
    resp3 = spi.transfer((0x03,0xff,) + (0,0,0,)*(num_bytes%blocksize))[2:]
    resp2 = np.concatenate(resp2).reshape(-1,3)
    resp3 = resp3.reshape(-1,3)
    ret = np.concatenate((resp2,resp3))
    #print 'read', np.shape(ret)
    return ret

  def close(self):
    spi.closeSPI()
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
  
