import unittest

from tart.simulation.util import *
import numpy as np


class TestUtil(unittest.TestCase):
    def test_rad2deg(self):
        self.assertEqual(Util.rad2deg(0.0), 0.0)
        self.assertEqual(Util.rad2deg(2 * Util.PI), 360.0)
        self.assertEqual(Util.rad2deg(Util.PI), 180.0)

    def test_rem(self):
        for k in range(1000):
            x = np.random.uniform(-1000, 1000)
            y = np.random.uniform(-100, 100)
            self.assertAlmostEqual(Util.rem(x, y), x % y, 9)

    def test_rem2pi(self):
        self.assertEqual(Util.rem2pi(Util.PI), Util.PI)
        self.assertEqual(Util.rem2pi(Util.PI + Util.PI2), Util.PI)
        self.assertEqual(Util.rem2pi(Util.PI - Util.PI2), Util.PI)
        self.assertEqual(Util.rem2pi(Util.PI / 10 - Util.PI2), Util.PI / 10)

    def test_idiv(self):
        for k in range(1000):
            x = np.random.uniform(-1000, 1000)
            y = np.random.uniform(-100, 100)
            self.assertEqual(Util.idiv(x, y), x // y)
