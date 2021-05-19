import unittest
from tart.imaging.gps_time import *


class TestGpsTime(unittest.TestCase):
    def test_jump(self):
        g1 = GpsTime(2013, 9, 21, 0, 59, 3)
        g2 = GpsTime(2013, 9, 21, 1, 0, 3)
        self.assertEqual(g1.m_week, g2.m_week)
        self.assertEqual(g1.sec_of_week, g2.sec_of_week - 60.0)
        g1 = GpsTime.from_time(utc.utc_datetime(2013, 9, 21, 0, 59, 3))
        g2 = GpsTime.from_time(utc.utc_datetime(2013, 9, 21, 1, 0, 3))
        self.assertEqual(g1.m_week, g2.m_week)
        self.assertEqual(g1.sec_of_week, g2.sec_of_week - 60.0)

    def test_from_time(self):
        g1 = GpsTime.from_time(utc.utc_datetime(2013, 9, 21, 0, 59, 3))
        g2 = GpsTime(2013, 9, 21, 0, 59, 3)
        self.assertEqual(g1.m_week, g2.m_week)
        self.assertEqual(g1.sec_of_week, g2.sec_of_week)
