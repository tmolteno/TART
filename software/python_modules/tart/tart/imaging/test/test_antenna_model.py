import unittest
import numpy as np
import datetime

from tart.imaging.antenna_model import *
from tart.util.db import *


def create_empirical_antenna():
    sv = 1
    ant = EmpiricalAntenna(1)
    for e in np.arange(0.0, 89.0, 2.0):
        for a in np.arange(0.0, 359.0, 5.0):
            el = angle.from_dms(e)
            az = angle.from_dms(a)
            ant.add_measurement(el, az, e * (1 + np.cos(np.pi / 180.0 * a)), sv)
    for e in np.arange(0.0, -89.0, -2.0):
        for a in np.arange(0.0, 359.0, 5.0):
            el = angle.from_dms(e)
            az = angle.from_dms(a)
            ant.add_measurement(el, az, 0.0, sv)
    return ant


class TestAntennaModel(unittest.TestCase):
    """Test our antenna models."""

    def test_gps(self):
        """Test the GPS patch antenna."""
        ant = GpsPatchAntenna()
        self.assertEqual(ant.get_gain(angle.from_dms(120), angle.from_dms(120)), 0.0)
        self.assertEqual(ant.get_gain(angle.from_dms(80), angle.from_dms(120)), 1.0)
        self.assertEqual(ant.get_gain(angle.from_dms(-80), angle.from_dms(120)), 0.0)

    def test_ideal(self):
        """Test the ideal hemispherical antenna"""
        ant = IdealHemisphericalAntenna()
        self.assertEqual(ant.get_gain(angle.from_dms(120), angle.from_dms(120)), 0.0)
        self.assertEqual(ant.get_gain(angle.from_dms(80), angle.from_dms(120)), 1.0)
        self.assertEqual(ant.get_gain(angle.from_dms(-80), angle.from_dms(120)), 0.0)

    # def test_empirical(self):
    #'''Test the empirical hemispherical antenna'''
    # ant = create_empirical_antenna()
    ##self.assertAlmostEqual(ant.get_gain(angle.from_dms(70), angle.from_dms(120)) / (70.*(1+np.cos(np.pi/180.*120.))), 1.0 , 1)
    ##self.assertAlmostEqual(ant.get_gain(angle.from_dms(45), angle.from_dms(45)) / (45.*(1+np.cos(np.pi/180.*45.))), 1.0, 1)
    ##self.assertAlmostEqual(ant.get_gain(angle.from_dms(-80), angle.from_dms(120)), 0.0, 1)
    # for e in np.arange(0., 89., 12.0):
    # for a in np.arange(0., 359., 25.0):
    # el = angle.from_dms(e)
    # az = angle.from_dms(a)
    # if e != 0:
    # check = ant.get_gain(el, az) - e*(1.+np.cos(np.pi/180.*a))
    # self.assertAlmostEqual(check, 0 , 1)

    def test_json_load(self):
        ant = create_empirical_antenna()
        ant.to_json("test.json")
        ant2 = EmpiricalAntenna.from_json("test.json")
        for e in np.arange(0, 89, 3.0):
            for a in np.arange(0, 359, 8.0):
                el = angle.from_dms(e)
                az = angle.from_dms(a)
                self.assertAlmostEqual(ant.get_gain(el, az), ant2.get_gain(el, az), 3)

    def test_db_load(self):
        ant = create_empirical_antenna()
        ant.to_db(utc_date=datetime.datetime.utcnow(), db_file="test.db")
        ant2 = EmpiricalAntenna.from_db(antenna_num=ant.antenna_num, db_file="test.db")
        for e in np.arange(0, 89, 3.0):
            for a in np.arange(0, 359, 8.0):
                el = angle.from_dms(e)
                az = angle.from_dms(a)
                self.assertAlmostEqual(ant.get_gain(el, az), ant2.get_gain(el, az), 3)
