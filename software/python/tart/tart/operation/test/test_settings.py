
import unittest
import numpy as np

from tart.operation.settings import rotate_location

class TestSettings(unittest.TestCase):
  '''Test Settings'''
  def test_rotate_location_30(self):
    east = np.array([1., 0., 0.])
    r_e, r_n, r_u = rotate_location(30, east)
    self.assertAlmostEqual(r_e, 0.86602540378, 5)
    self.assertAlmostEqual(r_n, 0.5, 5)
  def test_rotate_location_90(self):
    east = np.array([1., 0., 0.])
    r_e, r_n, r_u = rotate_location(90, east)
    self.assertAlmostEqual(r_e, 0, 5)
    self.assertAlmostEqual(r_n, 1, 5)
