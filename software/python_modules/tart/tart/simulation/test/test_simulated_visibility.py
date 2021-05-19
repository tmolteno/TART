import unittest

import numpy as np
import datetime
import matplotlib.pyplot as plt


from tart.imaging import antenna_model
from tart.operation import settings
from tart.operation import observation
from tart.simulation import simulation_source
from tart.simulation import radio
from tart.simulation import antennas
from tart.util import angle
from tart.imaging import correlator

TEST_CONFIG = "./tart/test/test_telescope_config.json"
TEST_ANTENNA_POSITIONS = "./tart/test/test_calibrated_antenna_positions.json"


def relative_diff(l1, l2):
    return (l1 - l2) / np.sqrt(np.power(l1, 2) + np.power(l2, 2))


class TestSimulatedVisibility(unittest.TestCase):
    def setUp(self):
        self.utc_date = datetime.datetime.utcnow()
        self.config = settings.from_file(TEST_CONFIG)
        self.config.load_antenna_positions(
            cal_ant_positions_file=TEST_ANTENNA_POSITIONS
        )

        self.rad = radio.Max2769B(
            noise_level=[0.0 for _ in range(self.config.get_num_antenna())]
        )
        self.sources = [
            simulation_source.SimulationSource(
                r=1e9,
                amplitude=1.0,
                azimuth=angle.from_dms(2.0),
                elevation=angle.from_dms(50.0),
                sample_duration=self.rad.sample_duration,
            )
        ]
        self.ants = [
            antennas.Antenna(self.config.get_loc(), pos)
            for pos in self.config.get_antenna_positions()
        ]
        self.ant_models = [
            antenna_model.GpsPatchAntenna()
            for i in range(self.config.get_num_antenna())
        ]
        self.cor = correlator.Correlator()
        self.timebase = np.arange(
            0, self.rad.sample_duration, 1.0 / self.rad.sampling_rate
        )

        ant_sigs_full = antennas.antennas_signal(
            self.ants, self.ant_models, self.sources, self.timebase
        )
        obs = self.rad.get_full_obs(
            ant_sigs_full, self.utc_date, self.config, self.timebase
        )
        vis = self.cor.correlate(obs)
        self.full_vis = np.array(vis.v)

        ant_sigs_simp = antennas.antennas_simplified_signal(
            self.ants,
            self.ant_models,
            self.sources,
            self.rad.baseband_timebase,
            self.rad.int_freq,
        )
        obs = self.rad.get_simplified_obs(
            ant_sigs_simp, self.utc_date, config=self.config
        )
        vis2 = self.cor.correlate(obs)
        self.sim_vis = np.array(vis2.v)

        plt.figure(figsize=(4, 4))
        plt.scatter(self.full_vis.real, self.full_vis.imag, label="full")
        plt.scatter(self.sim_vis.real, self.sim_vis.imag, color="red", label="simpl")
        plt.legend()
        plt.show()

    # def test_full_vis(self):
    #   self.assertGreater(self.full_vis.max(), 0.8)
    #   self.assertLess(self.full_vis.min(), -0.8)

    # def test_simp_vis(self):
    #   self.assertGreater(self.sim_vis.max(), 0.8)
    #   self.assertLess(self.sim_vis.min(), -0.8)

    def test_compare_vis(self):
        d = self.full_vis - self.sim_vis
        print(d)
        print("relative diffs", relative_diff(self.full_vis, self.sim_vis))
        self.assertLess(abs(d.max()), 0.3)
        self.assertLess(d.std(), 0.1)


class TestTelescope(unittest.TestCase):
    def setUp(self):

        # rad = radio.Max2769B(noise_level=0.)
        utc_date = datetime.datetime.utcnow()
        self.config = settings.from_file(TEST_CONFIG)
        self.config.load_antenna_positions(
            cal_ant_positions_file=TEST_ANTENNA_POSITIONS
        )

        rad = radio.Max2769B(
            noise_level=[0.0 for _ in range(self.config.get_num_antenna())]
        )
        sources = [
            simulation_source.SimulationSource(
                r=1e9,
                amplitude=1.0,
                azimuth=angle.from_dms(2.0),
                elevation=angle.from_dms(50.0),
                sample_duration=rad.sample_duration,
            )
        ]
        ant_models = [
            antenna_model.GpsPatchAntenna()
            for i in range(self.config.get_num_antenna())
        ]
        ants = [
            antennas.Antenna(self.config.get_loc(), pos)
            for pos in self.config.get_antenna_positions()
        ]
        self.timebase = np.arange(0, rad.sample_duration, 1.0 / rad.sampling_rate)

        ant_sigs = antennas.antennas_signal(ants, ant_models, sources, self.timebase)
        self.obs = rad.get_full_obs(ant_sigs, utc_date, self.config, self.timebase)

    def test_data_limits(self):
        for i in range(1, self.obs.config.get_num_antenna()):
            data = self.obs.get_antenna(i)

            self.assertGreater(data.max(), 0.99)
            self.assertLess(data.min(), -0.99)

            self.assertGreater(data.mean(), -0.1)
            self.assertLess(data.mean(), 0.1)

    def test_settings(self):
        self.assertEqual(self.config.get_num_antenna(), 24)

    def test_simulated_observation(self):
        fname = "test_observation.pkl"
        self.obs.save(fname)
        nobs = observation.Observation_Load(fname)
        self.assertEqual(self.obs.timestamp, nobs.timestamp)
        self.assertTrue((self.obs.get_antenna(1) == nobs.get_antenna(1)).all())
