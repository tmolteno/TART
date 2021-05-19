from tart.simulation.util import Util
from tart.imaging import gps_time
from tart.util import utc

import jsonrpclib
import datetime
import numpy as np

import unittest

from tart.imaging.ephemeris import *
from tart.imaging.ephemerides_proxy import EphemeridesProxy


class TestEphemeris(unittest.TestCase):
    def setUp(self):
        self.ep = EphemeridesProxy.Instance()
        self.s = self.ep.server
        self.t = utc.utc_datetime(2002, 10, 31, 0, 2, 2)

        self.eph_hash = self.s.get_ephemeris(self.t.isoformat(), 1)

    def test_eph(self):
        self.assertEqual(self.eph_hash["toe"], 345600.0)
        self.assertEqual(self.eph_hash["roota"], 5153.57448578)

        eph = Ephemeris(self.eph_hash)
        self.assertEqual(self.eph_hash["toe"], eph.toe)

        sow = 123.0
        pos = eph.get_location(sow)

        gpst = gps_time.GpsTime.from_time(self.t)
        pos2 = eph.get_sv_position(gpst)

        t = self.t
        for i in range(0, 100):
            t = t + datetime.timedelta(seconds=200.0)
            gpst = gps_time.GpsTime.from_time(t)
            pos = eph.get_sv_position(gpst)
            pos_remote = self.s.get_sv_position(t.isoformat(), 1)
            diff = np.array(pos) - np.array(pos_remote)
            dr = np.sqrt(diff.dot(diff))
            dt = abs(eph.toe - gpst.sow())
            if dt > 3600:
                self.eph_hash = self.s.get_ephemeris(t.isoformat(), 1)
                eph = Ephemeris(self.eph_hash)

            self.assertLess(dr, 3.0)  # Maximum difference of 3 meters
