from tartspi import TartSPI
import matplotlib
#matplotlib.use('gtk')
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import mlab
import argparse
import time
import datetime

from tart.operation import observation
from tart.operation import settings

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Test bench for TART commuication via SPI.')
  parser.add_argument('--speed', default=8, type=float, help='Specify the SPI CLK speed (in MHz)')
  parser.add_argument('--bramexp', default=11, type=int, help='exponent of bram depth')
  parser.add_argument('--spectrum', action='store_true', help="Display spectre")
  parser.add_argument('--plot', action='store_true', help="Display spectre")
  parser.add_argument('--num_plots', default=1, type=int, help="Number of antennas to plot")
  parser.add_argument('--config-file', default='telescope_config.json', help="The telescope configuration file.")
  parser.add_argument('--save', action='store_true', help="Save to observation file.")

  args = parser.parse_args()
  # Setup plotting
  num_plots = args.num_plots
  plt.ion() 
  if args.plot: 
    p_data = [[] for _ in range(num_plots)]
    f2, ax2 = plt.subplots(num_plots+1,figsize=(5,10))
    fft_p = [ax2[i].plot([])[0] for i in range(num_plots+1)]
    plt.show()

  t_SPI = TartSPI(speed=args.speed*1000000)
  j_max=100
  corr_amplitude=np.zeros(j_max)
  
  def blocklen(s):
    ret = 0
    count = 0
    for j in range(1,len(s)):
      if s[j] == s[j-1]:
        count = count + 1
      else:
        if count > ret:
          ret = count
        count=0
    return ret

  j = 0
  while 1:
    j = j+1
    if j==j_max:
      j=0
    # Acquire data
    t_SPI.start_acquisition()
    data = t_SPI.read_data(num_bytes=2**args.bramexp, blocksize=1024)
    t_SPI.reset()
    ant_data = np.flipud(np.unpackbits(data).reshape(-1,24).T)
    ts = datetime.datetime.utcnow()

    if args.save:
      config = settings.Settings(args.config_file)
      filename = 'fringes/' + ts.strftime('%H_%M_%S.%f') + '_data.pkl'
      print 'create observation object'
      obs = observation.Observation(ts, config, savedata=ant_data)
      obs.save(filename)
      print 'saved to: ', filename
    else:
      pn_data = ant_data*2-1.
      corr_amplitude[j] = np.abs(np.dot(pn_data[0],pn_data[1])/(1.*len(pn_data[0])))
      print corr_amplitude[j]
      means =  np.mean(pn_data,axis=1)
      for i in range(24):
        print i, means[i]
        if means[i] < 1. and np.abs(means[i])> 0.2:
          #fig  = plt.figure()
          print 'STREAK:', blocklen(pn_data[i])
          #plt.plot(pn_data[i])
          #plt.savefig(ts.strftime('%H_%M_%S.%f') + "_ant_%i.png" % (i))
          #plt.show()
    
    if args.plot:
      fft_p[0].set_data(np.arange(j_max),corr_amplitude)
      ax2[0].relim()
      ax2[0].autoscale_view(True,True,True)
      if args.spectrum:
        for i,ant_idx in enumerate(range(0,num_plots)):
          #mean = pn_data[ant_idx].mean()
          #print ant_idx, mean
          power, freq = mlab.psd(pn_data[ant_idx]-means[i],Fs=16.368e6, NFFT=1024)#NFFT=8192)
          fft_p[i+1].set_data(freq/1e6,10.*np.log10(power))
          ax2[i+1].relim()
          ax2[i+1].autoscale_view(True,True,True)
        plt.draw()
