
from tart.imaging import location, sun
import sky_object
import numpy as np


class SunObject(sky_object.SkyObject):

    def __init__(self):
        sky_object.SkyObject.__init__(self, "Sun")

    def get_az_el(self, utc_date, lat, lon, alt, elevation):
        s = sun.Sun()
        loc = location.Location(lat, lon, alt=alt)
        ra, decl = s.radec(utc_date)
        _el, _az = loc.equatorial_to_horizontal(utc_date, ra, decl)
        el, az = np.round([_el.to_degrees(), _az.to_degrees()], decimals=6)
        ret = []
        if (el > elevation):
            ret.append({'name': 'sun', 'r': 1e10,
                       'el': el, 'az': az, 'jy': 10000.0})
        return ret


if __name__ == "__main__":
    import tart.util.utc as utc
    import tart.util.angle as angle

    cache = SunObject()
    print(cache.get_az_el(utc.now(), lat=angle.from_dms(-45.86391200),
          lon=angle.from_dms(170.51348452), alt=46.5))
