
import numpy as np


from tart.simulation.skymodel import *


import unittest

class TestSkymodel(unittest.TestCase):
  """Testharness for Skymodel class"""
  def test_from_to_state_vector(self):
    """Generate Model from state vector. Then generate state vector from model and compare"""
    sv_init = np.array([15, 0, 90, -60, 2, 2, 0.5, 0.1])
    model = from_state_vector(sv_init)
    sv_final = model.get_state_vector()
    for i, f in zip(sv_init, sv_final):
      self.assertAlmostEqual(i, f, 10)
