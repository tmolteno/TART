"""
RadioSource Radio Source

Some bright cosmic radio sources
"""
#
# Copyright (c) Tim Molteno 2013. tim@elec.ac.nz
#
import datetime

from tart.util import angle
from tart.imaging import location
from tart.util import skyloc

import numpy as np


class RadioSource(object):
    """This is the base class for all possible radio sources"""

    def __init__(self, r, jy=1000.0, width=0.01):
        self.flux = jy
        self.width = width
        self.r = r

    def radec(self, utc_date):  # Get the RA and Declination
        raise RuntimeError("Use a class derived from RadioSource")

    def jansky(self, utc_date):
        """ Spectral flux density S. This is the spectral radiance B(theta, phi) integrated over the solid angle of the source. """
        return self.flux

    def to_horizontal(self, location, utc_date):
        RA, declination = self.radec(utc_date)
        el, az = location.equatorial_to_horizontal(utc_date, RA, declination)
        return el, az


class ArtificialSource(RadioSource):
    def __init__(self, location, utc_time, r, el, az, jy=1000.0, width=0.001):
        RadioSource.__init__(self, r, jy, width)
        a_az = angle.from_dms(az)
        a_el = angle.from_dms(el)
        self.skyloc = skyloc.Skyloc.from_horizontal(location, utc_time, a_el, a_az)

    def radec(self, utc_date):  # Get the RA and Declination
        return self.skyloc.ra, self.skyloc.dec


class CosmicSource(RadioSource):
    """
    A cosmic radio source at fixed equatorial coordinates.
    """

    def __init__(self, ra, dec, r, jy=1000.0, width=0.001):
        RadioSource.__init__(self, r, jy, width)
        self.skyloc = skyloc.Skyloc(ra, dec)

    def __repr__(self):
        return "COSMIC" + str(self.skyloc)

    def radec(self, utc_date):  # Get the RA and Declination
        return self.skyloc.ra, self.skyloc.dec
