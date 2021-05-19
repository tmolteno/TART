# -*- coding: utf-8 -*-

import numpy as np
from tart.simulation import radio
from tart.simulation import antennas
from tart.simulation import simulation_source
from tart.operation import settings


if __name__ == "__main__":
    import numpy as np

    rad = radio.Max2769B(sample_duration=1e-5)
    sources = [
        simulation_source.SimulationSource(
            amplitude=1.0,
            elevation=30.0,
            azimuth=0.0,
            sample_duration=rad.sample_duration,
        )
    ]

    print("Radio Sampling Rate %g" % rad.sampling_rate)

    config = settings.Settings("../../tools/operation/telescope_config.json")
    ants = [antennas.Antenna(config.get_loc(), pos) for pos in config.ant_positions]
    ant_sigs = antennas.antennas_new(ants, sources, rad.timebase)

    num_radio_samples = (len(rad.timebase) / rad.freq_mult) + 1
    sampled_signals = np.zeros((config.get_num_antenna(), num_radio_samples))
    for i in range(0, config.get_num_antenna()):
        sampled_signals[i, :] = rad.sampled_signal(ant_sigs[i, :])

    print("The (truncated) array of radio signals is; ", sampled_signals[:, :10])
    print("Signal from each radio are in rows")
