import unittest

from tart.simulation.util import *

class TestUtil(unittest.TestCase):

  def test_rem(self):
    self.assertEqual(Util.rad2deg(0.0), 0.0)
    self.assertEqual(Util.rad2deg(2*Util.PI), 360.0)
    self.assertEqual(Util.rad2deg(Util.PI), 180.0)

  def test_rem(self):
    self.assertEqual(Util.rem2pi(Util.PI), Util.PI)
    self.assertEqual(Util.rem2pi(Util.PI+Util.PI2), Util.PI)
