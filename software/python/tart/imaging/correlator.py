from tart.operation import observation
from tart.imaging import visibility
from tart.util import angle

import numpy as np
from scipy.signal import hilbert

def V(x,y):
  return sum(x*np.conjugate(y)) * 1./ len(x)

def convert_to_baseband(xd, fs, fc0):
  # import time
  # aaa = time.time()
  xd_padded = np.concatenate((xd,[1])) # pad to length np.power(2,n)
  analytic = hilbert(xd_padded) * np.exp(-2.0j * np.pi * fc0 * np.arange(0.,len(xd_padded)/fs,1./fs))
  # analytic = hilbert(xd) * np.exp(-2.0j * np.pi * fc0 * np.arange(0.,len(xd)/fs,1./fs))
  # print 'hilbert: ', time.time()-aaa
  # from tart.simulation import spectrum
  # spectrum.plotSpectrum(xd, fs, c='red')
  # spectrum.plotSpectrum(analytic, fs, c='green')
  # plt.show()
  return analytic


class Correlator:

  def correlate(self, obs):
    vis = visibility.Visibility(obs, angle.from_dms(90.), angle.from_dms(0.))
    visibilities, baselines = self.compute_complex_vis(obs)
    vis.set_visibilities(visibilities, baselines)
    return vis

  def convert_to_baseband(self, obs):
    fs = obs.get_sampling_rate()
    fc0 = 4.092e6
    data = []
    for i in range(0, obs.config.num_antennas):
      xr = obs.get_antenna(i+1)
      xc = convert_to_baseband(xr, fs, fc0)
      data.append(xc)
    return data

  def compute_complex_vis(self, obs):
    '''Return an array of baselines and visibilities from this observation'''
    v = []
    baselines = []
    data = self.convert_to_baseband(obs)
    for i in range(0, obs.config.num_antennas):
      for j in range(i+1, obs.config.num_antennas):
        v.append(V(data[i],data[j]))
        baselines.append([i,j])
    return v, baselines

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