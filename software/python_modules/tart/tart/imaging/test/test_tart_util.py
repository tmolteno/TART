import datetime
import unittest

from astropy import time 
from astropy.time import Time 

from tart.imaging.tart_util import *

class TestTartUtil(unittest.TestCase):

  def test_jdl(self):
    utcd = datetime.datetime.now()
    jd1 = JulianDay(utcd)
    jd2 = get_julian_date(utcd)
    self.assertAlmostEqual(jd1, jd2, 7)

  def test_astropy(self):
    utcd = datetime.datetime.now()
    obstime = time.Time(utcd, scale='utc')

    jd = JulianDay(utcd)
    
    self.assertAlmostEqual(obstime.jd, jd, 5)
