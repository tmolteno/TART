# -*- coding: utf-8 -*-
# This module takes an array of amplitudes of length X and returns an array of
# X independent random sources of required amplitudes and sample length n

import numpy as np
from tart.util import constants
from scipy import interpolate


class HorizontalSource(object):
    def __init__(self, r, azimuth, elevation):
        self.r = r
        self.azimuth = azimuth
        self.elevation = elevation


class SimulationSource(HorizontalSource):
    def __init__(self, r, amplitude, azimuth, elevation, sample_duration):
        super(SimulationSource, self).__init__(r, azimuth, elevation)
        self.omega = constants.L1_OMEGA
        self.duration = sample_duration
        self.amplitude = amplitude

        Fs = 16.368e6
        max_baseline = 100  # in m
        max_time = max_baseline / constants.V_LIGHT
        F_noise = 1.0e6 * np.power(np.pi, 3) / 2.9 / 3.7123 * 2.5

        from tart.simulation import butter_filter

        noisetime = np.arange(-max_time, self.duration + max_time, 1.0 / (F_noise))
        randnoise = np.random.uniform(-1.0, 1.0, len(noisetime))

        self.f = interpolate.InterpolatedUnivariateSpline(
            noisetime, self.amplitude * randnoise
        )

    def s_baseband(self, t):
        return self.f(t)

    def s(self, t):
        return self.s_baseband(t) * np.cos(self.omega * t)


if __name__ == "__main__":
    from . import spectrum
    import pylab
    from tart.util import angle
    import matplotlib.pyplot as plt

    crab = SimulationSource(
        r=1e10,
        amplitude=1.0,
        azimuth=angle.from_dms(0.0),
        elevation=angle.from_dms(45.0),
        sample_duration=1e-3,
    )  # 10 ms sample duration

    timebase = np.arange(0, 1e-5, 1e-6)
    for t in timebase:
        print("s(t)=%g s(t-tg)=%g" % (crab.s(t), crab.s(t - 1e-11)))

    sample_duration = 1e-4

    bb_timebase = np.arange(0, sample_duration, 1.0 / (16.384e6))
    crab_bbsignal = crab.s_baseband(bb_timebase)

    spectrum.plotSpectrum(crab_bbsignal, 16.384e6)
    pylab.show()

    from scipy import signal

    S_an = np.abs(np.fft.fft(signal.hilbert(crab_bbsignal)))
    freqs = np.fft.fftfreq(len(bb_timebase), 1.0 / 16.384e6)
    plt.figure()
    plt.plot(freqs, S_an)
    plt.show()

    radio_sampling_rate = 4.19e9
    print("Radio Sampling Rate %g" % radio_sampling_rate)

    timebase = np.arange(0, sample_duration, 1.0 / radio_sampling_rate)
    crab_signal = crab.s(timebase)

    spectrum.plotSpectrum(crab_signal, radio_sampling_rate)

    plt.figure()
    plt.plot(timebase, crab_signal, "-x", c="blue", label="full signal")
    plt.plot(bb_timebase, crab_bbsignal, c="red", label="baseband")
    plt.legend()
    plt.show()
