from tart.operation import observation
from tart.imaging import visibility
from tart.util import angle

import numpy as np
from scipy.signal import hilbert

from tart.imaging.correlator import *

TEST_SCOPE_CONFIG = "tart/test/test_telescope_config.json"

import unittest


class TestCorrelator(unittest.TestCase):
    def test_correlator_simp(self):
        from tart.operation import settings
        from tart.simulation import simulation_source
        from tart.util import angle
        from tart.util import constants
        from tart.simulation import radio
        import datetime

        import numpy as np

        N = 20
        a0 = np.random.randint(0, 2, 2 ** N)
        a1 = np.random.randint(0, 2, 2 ** N)
        self.assertAlmostEqual(corr_b(a0, a1, N), corr_b_pat(a0, a1))

        from tart.operation import observation
        from tart.operation import settings
        import datetime

        t = datetime.datetime.utcnow()
        c = settings.from_file(TEST_SCOPE_CONFIG)
        c.Dict["num_antenna"] = 2
        d = [a0, a1]
        o = observation.Observation(timestamp=t, config=c, data=d)

        cor = Correlator(van_vleck_corr=True)
        vis_a = cor.correlate(o, mode="roll")
        # vis_b = cor.correlate_roll(o)
        vis_c = cor.correlate(o, mode="fftw_hilbert")
        # print(vis_a.vis(0,1), vis_b.vis(0,1), vis_c.vis(0,1))

        sample_duration = 16.02e-1
        sample_duration = 4e-1
        config = settings.from_file(TEST_SCOPE_CONFIG)
        rad = radio.Max2769B(noise_level=np.ones(config.get_num_antenna()))
        src = simulation_source.SimulationSource(
            amplitude=1.0,
            azimuth=angle.from_dms(0.0),
            elevation=angle.from_dms(5.0),
            sample_duration=sample_duration,
        )
        fc0 = rad.int_freq

        int_sig = np.exp(-2.0j * np.pi * fc0 * rad.baseband_timebase)

        def add_noise(sig):
            return sig + np.random.normal(0.0, 0.1, len(sig))

        def sig2binary(signal):
            return np.array([1.0 if (x >= 0) else 0.0 for x in signal])

        antsig1 = src.s_baseband(rad.baseband_timebase) * int_sig

        for fraction in np.random.uniform(-np.pi / 3, np.pi / 3, 10):
            dt = ((2.0 * np.pi) / src.omega) * fraction
            print("dt", dt)
            antsig2 = (
                src.s_baseband(rad.baseband_timebase + dt)
                * int_sig
                * np.exp(1.0j * src.omega * dt)
            )

            antsig1 = add_noise(antsig1.real)
            antsig2 = add_noise(antsig2.real)

            d = [sig2binary(antsig1), sig2binary(antsig2)]
            obs = observation.Observation(timestamp=t, config=c, data=d)

            vis_a = cor.correlate(o, mode="roll")
            # vis_b = cor.correlate_roll(o)
            vis_c = cor.correlate(o, mode="fftw_hilbert")
            vis_d = cor.correlate(o, mode="fftw_hilbert_sign")

            # print(vis_a.vis(0,1),vis_b.vis(0,1))
            print(vis_a.vis(0, 1), vis_c.vis(0, 1))
            print(vis_a.vis(0, 1), vis_d.vis(0, 1))
            # print(vis_b.vis(0,1),vis_c.vis(0,1))
            # print(vis_b.vis(0,1),vis_d.vis(0,1))
            print(vis_c.vis(0, 1), vis_d.vis(0, 1))

            # self.assertAlmostEqual(vis_a.vis(0,1),vis_b.vis(0,1),4)
            self.assertAlmostEqual(vis_a.vis(0, 1), vis_c.vis(0, 1), 4)
            self.assertAlmostEqual(vis_a.vis(0, 1), vis_d.vis(0, 1), 4)
            # self.assertAlmostEqual(vis_b.vis(0,1),vis_c.vis(0,1),4)
            # self.assertAlmostEqual(vis_b.vis(0,1),vis_d.vis(0,1),4)
            self.assertAlmostEqual(vis_c.vis(0, 1), vis_d.vis(0, 1), 4)

            vis = cor.correlate(obs, mode="roll")
            cor_out = angle.from_dms(angle.wrap_360(np.angle(vis.v[0], deg=True)))
            input_angle = angle.from_dms(
                angle.wrap_360(dt * src.omega * 180.0 / np.pi)
            )  # .to_degrees()
            # print(cor_out, input_angle)
            # print(type(cor_out),  type(input_angle))
            # print(cor_out - input_angle)
            self.assertLess(np.abs((cor_out - input_angle).to_degrees()), 30.0)

            # d = [sig2binary(antsig1),sig2binary(antsig2)]
            # obs = observation.Observation(timestamp=t, config=c, data=d)
            # vis = cor.correlate(obs,mode='fftw_hilbert')
            # cor_out = angle.wrap_360(np.angle(vis.v[0], deg=True))
            # print(cor_out, input_angle)
            # self.assertTrue(np.abs(cor_out - input_angle) < 20.)


class TestHilbert(unittest.TestCase):
    def test_hilbert(self):
        import scipy.signal

        print("hilbert")
        x = np.sin(np.arange(10000))
        h_scipy = scipy.signal.hilbert(x)
        h_pyfftw = hilbert(x)
        [self.assertAlmostEqual(i, j, 6) for i, j in zip(h_scipy, h_pyfftw)]


if __name__ == "__main__":
    unittest.main()
