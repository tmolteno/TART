import unittest
import numpy as np
from tart.imaging.ephemerides_proxy import *


class TestEphemeridesProxy(unittest.TestCase):
    def setUp(self):
        self.ep = EphemeridesProxy.Instance()

    def test_proxying(self):
        t = utc.utc_datetime(2013, 9, 21, 0, 59, 3)
        sv = 21
        for i in range(0, 100):
            t = t + datetime.timedelta(seconds=1.0)
            pos = self.ep.get_sv_position(t, sv)
            pos_remote = self.ep.get_remote_position(t, sv)
            diff = np.array(pos) - np.array(pos_remote)
            dr = np.sqrt(diff.dot(diff))
            if dr > 3.0:
                print("test_proxying %i %s %f %s %s" % (i, t, dr, pos, pos_remote))
            self.assertLess(dr, 3.0)  # Maximum difference of 3 meters

    # TODO sp3 orbit interpolation from the precise sp3 positions
    # Eg at ftp://cddis.gsfc.nasa.gov/pub/gps/products/1172/
    # or ftp://nfs.kasi.re.kr/glonass/products/1355/
    # And compare against predicted files. There is a BUG in
    # the ephemeris file.

    # def test_all_positions(self):
    # t = utc.utc_datetime(2012, 10, 31, 0, 2, 2)
    # for i in range(0,10):
    # t = t + datetime.timedelta(seconds=100.0)
    # pos = self.ep.get_sv_positions(t)

    def test_cache_jump(self):
        t1 = utc.utc_datetime(2013, 9, 21, 0, 59, 3)
        t = utc.utc_datetime(2013, 9, 21, 0, 59, 3)
        sv = 21
        p1 = self.ep.get_sv_position(t1, sv)
        for i in range(0, 100):
            t = t + datetime.timedelta(seconds=1.0)
            p2 = self.ep.get_sv_position(t, sv)
            diff = np.array(p2) - np.array(p1)
            dr = np.sqrt(diff.dot(diff))
            if dr > 1000:
                print("test_cache_jump %i %s %f" % (i, p2, dr))
            self.assertLess(dr, 3000.0 * (i + 1))

    def test_sp3_proxying(self):
        t = utc.utc_datetime(2013, 9, 21, 0, 59, 3)
        sv = 21
        for i in range(0, 100):
            t = t + datetime.timedelta(seconds=1.0)
            pos = self.ep.get_sv_position_sp3(t, sv)
            pos_remote = self.ep.get_remote_position(t, sv)
            diff = np.array(pos) - np.array(pos_remote)
            dr = np.sqrt(diff.dot(diff))
            if dr > 1.0:
                print("test_sp3_proxying %i %s %f %s %s" % (i, t, dr, pos, pos_remote))
            self.assertLess(dr, 1.0)  # Maximum difference of 1 meters
