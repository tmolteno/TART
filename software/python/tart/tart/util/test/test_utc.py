import unittest
import math

from astropy import time 
from astropy.time import Time 

from tart.util.utc import utc_datetime

class TestUTC(unittest.TestCase):

    def test_basic(self):
        utcd = utc_datetime(year=2009, month=7, day=22, hour=5, minute=9, second=50.3)
        apyt = time.Time('2009-07-22 05:09:50.3', format='iso', scale='utc')
        
        utcd2 = apyt.to_datetime()
        
        self.assertEqual(utcd2.year, utcd.year)
        self.assertEqual(utcd2.month, utcd.month)
        self.assertEqual(utcd2.day, utcd.day)
        self.assertEqual(utcd2.hour, utcd.hour)
        self.assertEqual(utcd2.minute, utcd.minute)
        self.assertEqual(utcd2.second, utcd.second)
