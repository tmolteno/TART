import unittest

import numpy as np
import datetime
import matplotlib.pyplot as plt
from tart.util import angle

from tart.simulation.simulation_source import *


class TestSimulationSource(unittest.TestCase):
    def setUp(self):
        self.crab = SimulationSource(
            r=1e9,
            amplitude=1.0,
            azimuth=angle.from_dms(0.0),
            elevation=angle.from_dms(45.0),
            sample_duration=1e-3,
        )  # 0.1 ms sample duration
        self.s2 = SimulationSource(
            r=1e9,
            amplitude=1.0,
            azimuth=angle.from_dms(0.0),
            elevation=angle.from_dms(90.0),
            sample_duration=1e-3,
        )  # 0.1 ms sample duration
        self.s3 = SimulationSource(
            r=1e9,
            amplitude=1.0,
            azimuth=angle.from_dms(0.0),
            elevation=angle.from_dms(0.0),
            sample_duration=1e-3,
        )  # 0.1 ms sample duration

    def test_full_simp(self):
        tend = 1e-4
        full_time = np.arange(0, tend, 1.0 / (1.575e9))
        crab_s = self.crab.s(full_time)

        samp_time = np.arange(0, tend, 1.0 / (16.0e6))
        samp_time2 = np.arange(0, tend, 1.0 / (16.0e6)) + 1e-6

        crab_s_baseband = self.crab.s_baseband(samp_time)
        crab_s_baseband2 = self.crab.s_baseband(samp_time2)
        plt.plot(full_time, crab_s / np.cos(full_time * self.crab.omega), label="full")
        plt.plot(samp_time, crab_s_baseband, label="sampled")
        plt.plot(samp_time2, crab_s_baseband2, label="sampled")

        plt.legend()
        # plt.show()

    def test_signal_deterministic(self):
        samp_freq = 16.0e6
        samp_time = np.arange(0, 1e-4, 1.0 / (samp_freq))
        a = self.crab.s_baseband(samp_time)
        dt = 1.0 / (16.0e6)
        b = self.crab.s_baseband(samp_time + dt)

        for i in np.abs(a[1:] - b[:-1]):
            self.assertLess(i, 1e-5)

        dt = 0.5 / (16.0e6)
        b = self.crab.s_baseband(samp_time + dt)
        for i in range(1, 100):
            print(i)
            if a[i + 1] - a[i] > 0:
                self.assertTrue(a[i + 1] - a[i] > a[i + 1] - b[i])
                self.assertTrue(a[i + 1] - a[i] > b[i] - a[i])

    def test_signal(self):
        timebase = np.arange(0, 1e-3, 2e-10)
        crab_signal = self.crab.s(timebase)
        mean = crab_signal.mean()
        stdev = crab_signal.std()
        self.assertLess(abs(mean), 1.0e-6)
        self.assertLess(stdev, 0.40)  # Should be 0.33
        self.assertGreater(stdev, 0.31)

    def test_baseband(self):
        timebase = np.arange(0, 1e-3, 1e-7)
        crab_signal = self.crab.s_baseband(timebase).real
        mean = crab_signal.mean()
        stdev = crab_signal.std()
        self.assertLess(abs(mean), 2.0e-2)
        self.assertLess(stdev, 0.55)  # Should be 0.5
        self.assertGreater(stdev, 0.47)
