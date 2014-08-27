from tart.operation import observation
from tart.imaging import visibility
from tart.util import angle

import numpy as np
from scipy.signal import hilbert

from tart.imaging.correlator import *


import unittest
class TestCorrelator(unittest.TestCase):
  def test_correlator_simp(self):
    from tart.operation import settings
    from tart.simulation import simulation_source
    from tart.util import angle
    from tart.util import constants
    from tart.simulation import radio
    import datetime

    class Fake_Config():
      def __init__(self):
        self.num_antennas = 2
    class Fake_Observation():
      def __init__(self, signallist, sampling_rate):
        self.config = Fake_Config()
        self.signallist = signallist
        self.timestamp = datetime.datetime.utcnow()
        self.sampling_rate = sampling_rate
      def get_antenna(self, i):
        return self.signallist[i-1]
      def get_sampling_rate(self):
        return self.sampling_rate

    config = settings.Settings('test_telescope_config.json')
    rad = radio.Max2769B(sample_duration = 4.02e-3, noise_level = np.ones(config.num_antennas))
    src = simulation_source.SimulationSource(amplitude = 1., azimuth = angle.from_dms(0.), elevation = angle.from_dms(5.), sample_duration = rad.sample_duration)
    fc0 = rad.int_freq

    int_sig = np.exp(-2.0j*np.pi*fc0*rad.baseband_timebase)


    def add_noise(sig):
      return sig + np.random.normal(0.,0.1,len(sig))

    def sig2binary(signal):
      return np.array([1.0 if (x>=0) else -1.0 for x in signal])


    antsig1 = src.s_baseband(rad.baseband_timebase) * int_sig

    for fraction in np.random.uniform(-np.pi,np.pi,10):
      dt = ((2.*np.pi)/src.omega)*fraction
      print 'dt', dt
      antsig2 = (src.s_baseband(rad.baseband_timebase+dt) * int_sig * np.exp(-1.0j*src.omega*dt))
      # print antsig1, antsig2

      antsig1 = add_noise(antsig1.real)
      antsig2 = add_noise(antsig2.real)
      obs = Fake_Observation([antsig1, antsig2], rad.ref_freq)

      cor = Correlator()
      vis = cor.correlate(obs)
      cor_out = angle.wrap_360(np.angle(vis.v[0], deg=True))
      input_angle = angle.from_dms(angle.wrap_360(dt*src.omega*180./np.pi)).to_degrees()
      print cor_out, input_angle

      self.assertTrue(np.abs(cor_out - input_angle) < 10.)

      obs = Fake_Observation([sig2binary(antsig1), sig2binary(antsig2)], rad.ref_freq)
      vis = cor.correlate(obs)
      cor_out = angle.wrap_360(np.angle(vis.v[0], deg=True))
      print cor_out, input_angle
      self.assertTrue(np.abs(cor_out - input_angle) < 10.)

class TestHilbert(unittest.TestCase):
  def test_hilbert(self):
    import scipy.signal
    print 'hilbert'
    x = np.sin(np.arange(10000))
    h_scipy = scipy.signal.hilbert(x)
    h_pyfftw = hilbert(x)
    [self.assertAlmostEqual(i,j, 6) for i, j in zip(h_scipy,h_pyfftw)]

class TestFxCorrelator(unittest.TestCase):

  def test_single_freq(self):
    fxc = FxCorrelator(bandwidth=2e6, linewidth=2e6)
    self.assertEqual(fxc.n_freq, 1)
    self.assertEqual(fxc.get_linewidth(), 2e6)

  def test_100_freq(self):
    bw = 2e6
    lw = 2e4
    fxc = FxCorrelator(bandwidth=bw, linewidth=lw)
    self.assertEqual(fxc.n_freq, 128)
    self.assertEqual(fxc.get_linewidth(), bw/128)

  def test_angular_resolution(self):
    bw = 2e6
    c0 = FxCorrelator(bandwidth=bw, linewidth=2e6)
    c1 = FxCorrelator(bandwidth=bw, linewidth=1e4)
    baseline = 100 # Meters
    self.assertGreaterEqual(c1.angular_resolution(baseline).to_degrees(), c0.angular_resolution(baseline).to_degrees())