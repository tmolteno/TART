import unittest
import numpy as np

from tart.util.compare_phases import unwrap, compare_phases

class TestComparePhases(unittest.TestCase):
    def test_unwrap(self):
        torad = np.pi/180.
        ret = unwrap(0)
        self.assertAlmostEqual(ret, 0*torad)
        ret = unwrap(180*torad)
        self.assertAlmostEqual(ret, 180*torad)
        ret = unwrap(3*np.pi)
        self.assertAlmostEqual(ret, np.pi)
        ret = unwrap(-3*np.pi)
        self.assertAlmostEqual(ret, np.pi)
        ret = unwrap(-9*np.pi)
        self.assertAlmostEqual(ret, np.pi)
    
    def test_compare_phases(self):
        torad = np.pi/180.
        ret = compare_phases(350*torad, 0)
        self.assertAlmostEqual(ret, 10*torad)
        ret = compare_phases(10*torad, 0)
        self.assertAlmostEqual(ret, 10*torad)
        ret = compare_phases(350*torad, -5*torad)
        self.assertAlmostEqual(ret, 5*torad)
        ret = compare_phases(-5*torad, 350*torad)
        self.assertAlmostEqual(ret, 5*torad)
        ret = compare_phases(10*torad, 20*torad)
        self.assertAlmostEqual(ret, 10*torad)
        ret = compare_phases(10*torad, 350*torad)
        self.assertAlmostEqual(ret, 20*torad)
        ret = compare_phases(-10*torad, -30*torad)
        self.assertAlmostEqual(ret, 20*torad)
        ret = compare_phases(-900.*torad, -0.*torad)
        self.assertAlmostEqual(ret, 180*torad)
