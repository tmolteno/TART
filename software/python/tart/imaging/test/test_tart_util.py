import datetime

import unittest
from tart.imaging.tart_util import *

class TestTartUtil(unittest.TestCase):

  def test_jdl(self):
    utcd = datetime.datetime.now()
    jd1 = JulianDay(utcd)
    jd2 = get_julian_date(utcd)
    self.assertAlmostEqual(jd1, jd2, 7)
