import unittest
import math

from tart.util.angle import atan, atan2, asin, acos

class TestAngle(unittest.TestCase):

    def test_atan(self):
        ax = atan(1.0)
        self.assertEqual(ax.to_degrees(), 45.0)
    def test_atan2(self):
        ax = atan2(10.,-10.)
        self.assertEqual(ax.to_degrees(), 135.0)
    def test_sin(self):
        ax = asin(1.0)
        self.assertEqual(ax.to_rad(), math.pi/2.)
    def test_acos(self):
        ax = acos(1.0)
        self.assertEqual(ax.to_hours(), 0.)
        ax = acos(-1.0)
        self.assertEqual(ax.to_degrees(), 180.)
