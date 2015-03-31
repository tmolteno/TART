import spinumpy as spi
import numpy as np
import time

class TartSPI:
  def __init__(self, speed=8000000):
    spi.openSPI(speed=int(speed))

  def reset(self):
    spi.transfer((0b10001111,0b00000001))
    time.sleep(0.1)
    print 'device now resetted'
    return 1

  def debug(self,on=1):
    if on:
      spi.transfer((0b10001000,0b00000001))
      print 'debug now on'
    else:
      spi.transfer((0b10001000,0b00000000))
      print 'debug now off'
    time.sleep(0.1)
    return 1

  def start_acquisition(self,sleeptime=0.2):
    spi.transfer((0b10000001,0b00000001))
    print sleeptime
    time.sleep(sleeptime)
    print 'acquision done'
    return 1

  def read_data(self, num_bytes=2**21, blocksize=1000):
    resp2 = []
    for i in range(0,int(num_bytes/blocksize)):
      resp2.append(spi.transfer((0b00000010,) + (0,0,0,)*blocksize)[1:])
    resp3 = spi.transfer((0b00000010,) + (0,0,0,)*(num_bytes%blocksize))[1:]
    resp2 = np.concatenate(resp2).reshape(-1,3)
    resp3 = resp3.reshape(-1,3)
    ret = np.concatenate((resp2,resp3))
    print 'read', np.shape(ret)
    return ret

  def close(self):
    spi.closeSPI()
    return 1

import argparse
if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=8, type=float, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=11, type=int, help='exponent of bram depth')
  parser.add_argument('--debug', action='store_true', help='operate telescope with fake antenna data.')
  parser.add_argument('--monitor', action='store_true', help="The telescope configuration file.")
  args = parser.parse_args()

  if args.monitor:
    import time
    import matplotlib
    matplotlib.use('gtk')
    import matplotlib.pyplot as plt
    from matplotlib import mlab
    p_data = [[] for _ in range(6)]
    plt.ion()
    f2, ax2 = plt.subplots(6,sharex=True,figsize=(5,10))
    fft_p = [ax2[i].plot([])[0] for i in range(6)]
    #f2.tight_layout()
    plt.show()
    t_SPI = TartSPI(speed=args.speed*1000000)
    while 1:
      t_SPI.debug(args.debug)
      t_SPI.start_acquisition()
      data = t_SPI.read_data(num_bytes=2**args.bramexp, blocksize=1024)
      t_SPI.reset()
      ant_data = np.flipud(np.unpackbits(data).reshape(-1,24).T)*2-1.
      for i,ant_idx in enumerate(range(0,6)):
        mean = ant_data[ant_idx].mean()
        print ant_idx, mean
        power, freq = mlab.psd(ant_data[ant_idx]-mean,Fs=16.368e6, NFFT=4096)#NFFT=8192)
        fft_p[i].set_data(freq/1e6,10.*np.log10(power))
        #ax2[i].set_xlim(0,8)
        #ax2[i].set_ylim(-80,-50)
      	ax2[i].relim()
	print
      	ax2[i].autoscale_view(True,True,True)
      plt.draw()

