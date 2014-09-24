from tart.operation import observation
from tart.imaging import visibility
from tart.util import angle

import numpy as np

from pyfftw.interfaces.scipy_fftpack import hilbert as fftw_hilbert
def hilbert(x):
 return x-1j*fftw_hilbert(x)

# from scipy.signal import hilbert

def V(x,y):
  return np.dot(x,np.conjugate(y))/len(x)


class Correlator:

  def correlate(self, obs):
    vis = visibility.Visibility(obs, angle.from_dms(90.), angle.from_dms(0.))
    visibilities, baselines = self.compute_complex_vis(obs)
    vis.set_visibilities(visibilities, baselines)
    return vis

  def compute_complex_vis(self, obs):
    '''Return an array of baselines and visibilities from this observation'''
    v = []
    baselines = []
    fs = obs.get_sampling_rate()
    fc0 = 4.092e6
    shift = np.exp(-2.0j * np.pi * fc0 * np.arange(0.,len(obs.get_antenna(1))/fs,1./fs))
    print 'here_0.1'
    data = [hilbert(obs.get_antenna(i+1)) for i in range(obs.config.num_antennas)]
    print 'here_0.2'
    for i in range(0, obs.config.num_antennas):
      #data_i = hilbert(obs.get_antenna(i+1)) #*shift
      print 'herei', i
      for j in range(i+1, obs.config.num_antennas):
        #data_j = hilbert(obs.get_antenna(j+1)) #*shift
        #v.append(V(data_i,data_j))
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
