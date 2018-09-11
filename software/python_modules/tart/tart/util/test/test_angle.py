import unittest
import math

from tart.util.angle import atan, atan2, asin, acos, from_dms

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

    def test_hms(self):
        x = from_dms(167.7958333333)
        h,m,s = x.to_hms()
        self.assertEqual(h, 11)
        self.assertEqual(m, 11)
        self.assertAlmostEqual(s, 11)
        
    def test_dms(self):
        x = from_dms(180,20,11)
        h,m,s = x.to_dms()
        self.assertEqual(h, 180)
        self.assertEqual(m, 20)
        self.assertEqual(s, 11)
