# from tart.imaging import radio_source
# from tart.imaging.ephemerides_proxy import EphemeridesProxy
from tart.imaging import location

# from tart.util import utc
# from tart.util import constants
# from tart.util import vector

import datetime
import numpy as np

import unittest
from tart.util import utc
from tart.util import angle

from tart.imaging.gps_satellite import *


class TestGpsSatellite(unittest.TestCase):
    def setUp(self):
        self.t = utc.utc_datetime(2002, 10, 31, 0, 2, 2)

    def test_gps(self):

        import ephem

        dnd = ephem.Observer()
        dnd.lon = str(location.Dunedin.lon.to_degrees())
        dnd.lat = str(location.Dunedin.lat.to_degrees())
        dnd.elevation = location.Dunedin.alt
        dnd.pressure = 0.0
        t = utc.utc_datetime(2013, 9, 20, 0, 0, 3)
        dnd.date = t

        for svnum in range(31, 32):
            l1 = "GPS BIIRM-2 (PRN 31)    "
            l2 = "1 29486U 06042A   13264.26023969 -.00000084  00000-0  00000+0 0  2292"
            l3 = "2 29486  56.1466 336.5502 0081195 316.5190  69.5394  2.00563866 51246"
            j = ephem.readtle(l1, l2, l3)
            # http://blog.thetelegraphic.com/2012/gps-sattelite-tracking-in-python-using-pyephem/
            # j = ephem.Gps(svnum)
            j.compute(dnd)
            ra_e = angle.from_rad(j.ra.real)
            dec_e = angle.from_rad(j.dec.real)

            sv = GpsSatellite(svnum, location.Dunedin)
            ra, dec = sv.radec(t)

            print(angle.wrap_360(ra.to_degrees()))
            self.assertAlmostEqual(ra.to_degrees(), ra_e.to_degrees(), 0)
            self.assertAlmostEqual(dec.to_degrees(), dec_e.to_degrees(), 0)

    def test_radec(self):
        sv = GpsSatellite(31, location.Dunedin)
        t = utc.utc_datetime(2013, 9, 20, 0, 0, 3)
        ra, dec = sv.radec(t)
        self.assertLess(ra.to_degrees(), 360.0)
        self.assertLess(abs(dec.to_degrees()), 90.0)

    def test_power(self):
        sv = GpsSatellite(31, location.Dunedin)
        t = utc.utc_datetime(2013, 9, 20, 0, 0, 3)
        self.assertLess(sv.jansky(t), 5.0e6)
        self.assertGreater(sv.jansky(t), 3.0e6)
