
class Skyloc(object):
    """
    A class representing a location in the sky in equatorial co-ordinates.
    Holds Right Ascension and Declination
    """
    def __init__(self, ra, dec):
        self.ra = ra
        self.dec = dec

    def to_horizontal(self, location, utc_time):
        """Returns the horizontal (el,az) coordinates for a given time"""
        return location.equatorial_to_horizontal(utc_time, self.ra, self.dec)

    @classmethod
    def from_equatorial(self, ra, dec):
        """Generate Skyloc object from equatorial coordinates"""
        return Skyloc(ra, dec)

    # Call this as Skyloc.from_horizontal()
    @classmethod
    def from_horizontal(self, location, utc_time, el, az):
        """Generate Skyloc object from horizontal coordinates"""
        ra, dec = location.horizontal_to_equatorial(utc_time, el, az)
        return Skyloc(ra, dec)
