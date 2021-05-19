"""
Solar Radio Source

Calculate the position in horizontal coordinates for the sun.
"""
#
# Copyright (c) Tim Molteno 2013. tim@elec.ac.nz
#
import datetime
import math

from tart.imaging import location
from tart.imaging import radio_source
from tart.imaging import tart_util
from tart.util import angle


class Sun(radio_source.RadioSource):
    """
    A RadioSource descendent that represents the sun.
    """

    def __init__(self, jy=1200.0):
        radio_source.RadioSource.__init__(self, r=149e9, jy=jy, width=0.5)

    def __repr__(self):
        return "SUN"

    """ Compute ecliptic longitude of the Sun: latitude B is assumed to be zero"""

    def solar_longitude_to_RA(self, L, utc_date):  # require L to be an angle object!!!
        jd = tart_util.JulianDay(utc_date)
        D = jd - 2451545.0  # where jd is the Julian date of interest. Then compute
        # Mean anomaly of the Sun:
        g = angle.from_dms(357.529 + 0.98560028 * D)
        # g = angle.to_rad(g)
        # Mean longitude of the Sun:
        q = 280.459 + 0.98564736 * D
        R = (
            1.00014 - 0.01671 * g.cos() - 0.00014 * math.cos(g.to_rad() * 2)
        )  # The distance of the Sun from the Earth, R, in astronomical units (AU)
        e = angle.from_dms(
            23.439 - 0.00000036 * D
        )  # mean obliquity of the ecliptic, in degrees:
        # L = angle.to_rad()
        # e = angle.to_rad(e)
        tan_RA = e.cos() * L.sin() / L.cos()
        sin_d = e.sin() * L.sin()
        # RA = math.atan(tan_RA)
        RA = angle.atan2(e.cos() * L.sin(), L.cos())
        RA = RA.to_ra()
        delta = angle.asin(sin_d)
        return RA, delta

    # http://aa.usno.navy.mil/faq/docs/SunApprox.php
    def solar_longitude(self, utc_date):
        jd = tart_util.JulianDay(utc_date)
        D = jd - 2451545.0
        # Mean anomaly of the Sun:
        g = 357.529 + 0.98560028 * D
        # Mean longitude of the Sun:
        q = 280.459 + 0.98564736 * D
        g = angle.from_dms(g)
        # Geocentric apparent ecliptic longitude of the Sun (adjusted for aberration):
        L = q + 1.915 * g.sin() + 0.020 * math.sin(g.to_rad() * 2)
        L = angle.from_dms(angle.wrap_360(L))
        return L

    # Get the RA and Declination
    def radec(self, utc_date):
        L = self.solar_longitude(utc_date)
        RA, declination = self.solar_longitude_to_RA(L, utc_date)
        return RA, declination
