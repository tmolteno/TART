from tart.operation import observation
from tart.imaging import visibility
from tart.util import angle

import numpy as np

#import pyfftw
#from pyfftw.interfaces.scipy_fftpack import hilbert as fftw_hilbert
#from scipy.fftpack import hilbert as fftw_hilbert
# from tart.util.hilbert import hilbert_fftw as fftw_hilbert
import time

def van_vleck_correction(R):
  return np.sin(np.pi/2. * R)

def V(x,y,yhilb):
  real = van_vleck_correction(np.dot(x,y)*1./len(x)/2.)
  imag = van_vleck_correction(np.dot(x,yhilb)*1./len(x)/2.)
  return real-1.j*imag

class Correlator:

  def correlate(self, obs, debug=False):
    vis = visibility.Visibility(obs, angle.from_dms(90.), angle.from_dms(0.))
    visibilities, baselines = self.compute_complex_vis(obs, debug=debug)
    vis.set_visibilities(visibilities, baselines)
    return vis

  def compute_complex_vis(self, obs, debug=False):
    '''Return an array of baselines and visibilities from this observation'''
    v = []
    baselines = []
    data = []
    data_hilb = []
    for i in range(obs.config.num_antennas):
      ant_i = obs.get_antenna(i)
      mean_i = np.mean(ant_i)
      data.append(ant_i-mean_i)

    for i, d in enumerate(data):
      #print float(i)/obs.config.num_antennas
      #h_i = -np.sign(fftw_hilbert(d))
      #print type(h_i[0])
      #data_hilb.append(h_i)
      data_hilb.append(np.roll(d,1))

    for i in range(0, obs.config.num_antennas):
      for j in range(i+1, obs.config.num_antennas):
        print len(baselines)
        v.append(V(data[i],data[j],data_hilb[j]))
        baselines.append([i,j])
    return v, baselines

  def correlate_roll(self, obs, debug=False):
    vis = visibility.Visibility(obs, angle.from_dms(90.), angle.from_dms(0.))
    v = []
    baselines = []
    data = []
    num_ant = obs.config.num_antennas
    data = obs.data # use obs.data[i] instead of get_antenna to keep mem usage low
    means = obs.get_means()
    for i in range(0, num_ant):
      for j in range(i+1, num_ant):
        progress = len(baselines)
        if (progress%10==0):
          print progress
        v_real = van_vleck_correction( -means[i]*means[j] + corr_b(data[i], data[j])       )
        v_imag = van_vleck_correction( -means[i]*means[j] + corr_b(data[i], fast_roll_1(data[j])))
        #v_imag = van_vleck_correction(np.dot(data[i],np.roll(data[j],1))*1./len(data[i])/2.)
        v_com = v_real-1.j*v_imag
        v.append(v_com)
        baselines.append([i,j])
    vis.set_visibilities(v, baselines)
    return vis

def corr_b(x, y):
  num_not_same = (x ^ y).sum()
  n = len(x)
  ret = 0.5 - num_not_same/float(n)
  return ret

def fast_roll_1(d):
  return np.concatenate(([d[-1],],d[:-1]))

from tart.util import constants

class FxCorrelator(Correlator):
  '''A FX correlator.
     We need an FX correlator because the bandwidth is sufficient that it washes out the fringes away
     from the phase center. The distance we can get away from the phase center is given by

     q = arcsin(c / B D)

     where B is the baseline length, D is the bandwidth, c is the speed of light.

     where

        c = 3e8 (m s^-1)
        B = baseline (m)
        D = bw (s^-1)
  '''

  def __init__(self, bandwidth, linewidth):
    self.bandwidth = bandwidth
    n_log2 = int(np.log(bandwidth / linewidth) / np.log(2.0) + 0.5)
    self.n_freq = 2**n_log2
    self.linewidth = bandwidth / self.n_freq

  def get_linewidth(self):
    return self.linewidth

  def correlate(obs):
    data = self.convert_to_baseband(obs)
    vis = visibility.Visibility(obs, angle.from_dms(90), angle.from_dms(0))
    visibilities, baselines = self.compute_complex_vis(obs)
    vis.set_visibilities(visibilities, baselines)
    return vis


  '''TODO this is not working yet.
     this should return the maximum field of view possible before fringes get washed
     out by the bandwidth of the signal.'''
  def angular_resolution(self, baseline):
    x = constants.V_LIGHT / (self.linewidth * baseline)
    if (x >= 1.0):
      x = 1.0
    return angle.asin(x)

if __name__ == '__main__':

  fxc = FxCorrelator(2e6, 2e3)
  print fxc.get_linewidth()
  print fxc.angular_resolution(10.)
