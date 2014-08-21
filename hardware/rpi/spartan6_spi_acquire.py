import spi

import numpy as np
import time
import argparse

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=8, type=int, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=9, type=int, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='print debug output to screen')
 
  args = parser.parse_args()
  spi.openSPI(speed=args.speed*1000000)
  time.sleep(0.1)
  
  # START AQUSITION BY SETTING REG[0001] to 1
  print spi.transfer((0b10000001,0x1))
  
  time.sleep(0.3)
  resp = []
  
  num_bytes = np.power(2,args.bramexp)-32
  resp = []

  for i in range(num_bytes):
    resp.append(spi.transfer((0b0000010,0x0,0x0,0x0))[1:])
  
  if args.debug:
    resp = np.array(resp)
    resp_bin = np.array([[np.binary_repr(i,8) for i in j] for j in resp])
    resp_dec = np.array([j[0] * np.power(2,16) +j[1] * np.power(2,8) + j[2] for j in resp])
    resp_hex = np.array([[hex(i) for i in j] for j in resp])
  
    for i, ent in enumerate(resp_dec):
      print i, ent, resp_bin[i], resp_hex[i]
 
    print (resp_dec[1:]-resp_dec[:-1])
    print (resp_dec[1:]-resp_dec[:-1]).__ne__(1).sum()
  
  '''
  resp = []
  for i in range(num_bytes):
    resp.append(spi.transfer((0b00000100,0x0))[1:])
  
  resp = np.array(resp)
  resp_bin = [[np.binary_repr(i,8) for i in j] for j in resp]
  resp_hex = [[hex(i) for i in j] for j in resp]
  for i in range(len(resp)/10):
    j = i*10
    print [resp[j+k] for k in range(10)]
  print resp_bin, resp_hex
  #'''

