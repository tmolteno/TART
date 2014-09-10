from tart.imaging.antenna_model import *
import numpy as np

import unittest

class TestAntennaModel(unittest.TestCase):
  '''Test our antenna models.'''
  def setUp(self):
    self.ant = EmpiricalAntenna(1)
    for e in np.arange(0., 89., 2.0):
      for a in np.arange(0., 359., 5.0):
        el = angle.from_dms(e)
        az = angle.from_dms(a)
        self.ant.add_measurement(el,az, e*(1+np.cos(np.pi/180.*a)))
    for e in np.arange(0., -89., -2.0):
      for a in np.arange(0., 359., 5.0):
        el = angle.from_dms(e)
        az = angle.from_dms(a)
        self.ant.add_measurement(el,az, 0.0)


  def test_gps(self):
    '''Test the GPS patch antenna.'''
    ant = GpsPatchAntenna()
    self.assertEqual(ant.get_gain(angle.from_dms(120), angle.from_dms(120)), 0.0)
    self.assertEqual(ant.get_gain(angle.from_dms(80), angle.from_dms(120)), 1.0)
    self.assertEqual(ant.get_gain(angle.from_dms(-80), angle.from_dms(120)), 0.0)

  def test_ideal(self):
    '''Test the ideal hemispherical antenna'''
    ant = IdealHemisphericalAntenna()
    self.assertEqual(ant.get_gain(angle.from_dms(120), angle.from_dms(120)), 0.0)
    self.assertEqual(ant.get_gain(angle.from_dms(80), angle.from_dms(120)), 1.0)
    self.assertEqual(ant.get_gain(angle.from_dms(-80), angle.from_dms(120)), 0.0)

  def test_empirical(self):
    '''Test the ideal hemispherical antenna'''
    self.ant.get_gain(angle.from_dms(70), angle.from_dms(120), n_pix=4, nside_exp=10)
    self.assertAlmostEqual(self.ant.get_gain(angle.from_dms(70), angle.from_dms(120)) / (70.*(1+np.cos(np.pi/180.*120.))), 1.0 , 1)
    self.assertAlmostEqual(self.ant.get_gain(angle.from_dms(45), angle.from_dms(45)) / (45.*(1+np.cos(np.pi/180.*45.))), 1.0, 1)
    self.assertAlmostEqual(self.ant.get_gain(angle.from_dms(-80), angle.from_dms(120)), 0.0, 1)
    for e in np.arange(0., 89., 12.0):
      for a in np.arange(0., 359., 25.0):
        el = angle.from_dms(e)
        az = angle.from_dms(a)
        if e != 0:
          check = self.ant.get_gain(el, az) - e*(1.+np.cos(np.pi/180.*a))
          self.assertAlmostEqual(check, 0 , 1)

  def test_json_load(self):
    self.ant.to_json('test.json')
    ant2 = EmpiricalAntenna.from_json('test.json')
    for e in np.arange(0, 89, 3.0):
      for a in np.arange(0, 359, 8.0):
        el = angle.from_dms(e)
        az = angle.from_dms(a)
        self.assertAlmostEqual(self.ant.get_gain(el, az), ant2.get_gain(el, az), 3)
