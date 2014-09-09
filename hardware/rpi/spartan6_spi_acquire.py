import spinumpy as spi

import numpy as np
import time
import argparse

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=8, type=int, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=9, type=float, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='print debug output to screen')
  parser.add_argument('--fast', action='store_true', help='use faster transfer!')
  parser.add_argument('--fast2', action='store_true', help='use faster transfer!')
  parser.add_argument('--reset', action='store_true', help='reset after transmission?')
  
  args = parser.parse_args()
  
  num_bytes = np.power(2,args.bramexp)
  spi.openSPI(speed=args.speed*1000000)
  
  if args.reset: 
    print 'resetting device'
    time.sleep(0.1)
    spi.transfer((0b10001111,0b00000001))
    time.sleep(0.1)
  
  print 'enter debug mode'
  spi.transfer((0b10001000,0b00000001))
  time.sleep(0.1)
  
  #time.sleep(2)
  # START ACQUSITION BY SETTING REG[0001] to 1
  print 'start Acquision'
  print spi.transfer((0b10000001,0b00000001))
  time.sleep(0.5)
  
 
  print 'receiving 3x', num_bytes
  blocksize = 1000
  if (args.fast2):
    import itertools
    
    ret_array = []
    remainder = num_bytes%blocksize
    for i in range(0,int(num_bytes/blocksize)):
      ret_array.append(spi.transfer((0b00000010,) + (0,0,0,)*blocksize)[1:])
    ret_array.append(spi.transfer((0b00000010,) + (0,0,0,)*remainder)[1:])
    ret_array = list(itertools.chain.from_iterable(ret_array))
    
  elif (args.fast):
    resp2 = []
    for i in range(0,int(num_bytes/blocksize)):
      resp2.append(spi.transfer((0b00000010,) + (0,0,0,)*blocksize)[1:])
    resp3 = spi.transfer((0b00000010,) + (0,0,0,)*(num_bytes%blocksize))[1:]
  
  else:
    resp = []
    for i in range(num_bytes):
      resp.append(spi.transfer((0b0000010,0x0,0x0,0x0))[1:])
  
  print 'got data..' 
  
    
  if args.debug:
    ant_data = [] 
    if (args.fast2):
      print 'generating numpy array'
      ret_array = np.array(ret_array,dtype=int)
      print 'generate 24bit integer'
      resp_dec = (ret_array[0::3] << 16) + (ret_array[1::3] << 8) + ret_array[2::3]
      
    elif (args.fast):
      print 'reshape data blocks'
      resp2 = np.concatenate(resp2).reshape(-1,3)
      print 'reshape data remainder'
      resp3 = resp3.reshape(-1,3)
      print 'concatenate...'
      resp2 = np.concatenate((resp2,resp3))
      print 'generate 24bit integer'
      resp_dec = (resp2[:,0] << 16) + (resp2[:,1] << 8) + resp2[:,2]
      print 'done'
      
      #print 'shift into seperate antenna arrays' 
      #for i in range(8):
      #   ant_data.append(np.array((resp2[:,2] & 1<<(i))>0,dtype=int))
      #
      #for i in range(8):
      #   ant_data.append(np.array((resp2[:,1] & 1<<(i))>0,dtype=int))
      # 
      #for i in range(8):
      #   ant_data.append(np.array((resp2[:,0] & 1<<(i))>0,dtype=int))
      #
      #for i in range(24):
      #  print 'antdata',i, ant_data[i]

    else:
      resp = np.array(resp)
      resp_dec = (resp[:,0] << 16) + (resp[:,1] << 8) + resp[:,2]
    #resp_bin = np.array([[np.binary_repr(i,8) for i in j] for j in resp])
    #resp_hex = np.array([[hex(i) for i in j] for j in resp])
    #for i, ent in enumerate(resp_dec):
    #  print i, ent, resp_bin[i], resp_hex[i]
    diffs = (resp_dec[1:]-resp_dec[:-1])
    
    diffssum = diffs.__ne__(1).sum()
    print diffs
    print diffssum
    index = np.arange(len(diffs))
    print diffs[diffs.__ne__(1)]
    print index[diffs.__ne__(1)] 
    

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

