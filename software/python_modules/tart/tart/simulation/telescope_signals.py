# -*- coding: utf-8 -*-


def telescope_signals(ref_freq, freq_mult, sample_duration, baseline, angle_1, angle_2):

    import numpy as np

    SNR = 20  # Signal to noise ratio

    # Introduce the geometric delay
    delay_1 = baseline * np.sin(angle_1) / 2.99793e8
    delay_2 = baseline * np.sin(angle_2) / 2.99793e8

    # how many source rates is this delay
    sample_delay_1 = int(delay_1 * (ref_freq * freq_mult))
    sample_delay_2 = int(delay_2 * (ref_freq * freq_mult))

    print("delay_1 = ", delay_1)
    print("delay_2 = ", delay_2)
    print("sample_delay_1 = ", sample_delay_1)
    print("sample_delay_2 = ", sample_delay_2)

    # Calculate the number of points required
    n = int(ref_freq * freq_mult * sample_duration)

    # create two signals
    source_1 = np.random.random(n + sample_delay_1)  # Add gaussian noise
    source_2 = np.random.random(n + sample_delay_2)  # Add gaussian noise
    noise_1 = np.random.random(n) / SNR  # Add uncorrelated gaussian noise
    noise_2 = np.random.random(n) / SNR  # Add uncorrelated gaussian noise
    s1 = source_1[
        :n
    ]  # + source_2[:n] + noise_1 # Un-delayed signal with instrument noise
    s2 = source_1[
        sample_delay_1 : n + sample_delay_1
    ]  # + source_2[sample_delay_2:n+sample_delay_2]+ noise_2 # Delayed signal with instrument noise

    return (s1, s2)


if __name__ == "__main__":
    import numpy as np

    ref_freq = 16.368e6
    freq_mult = 256
    sample_duration = 1e-3
    ref_div = 16
    main_div = 1536
    bandwidth = 2.5e6
    order = 5
    baseline = 20.0  # Meters
    angle_1 = 30 * np.pi / 180  # Radians
    angle_2 = 60 * np.pi / 180  # Radians

    telescope_signals(
        ref_freq,
        freq_mult,
        sample_duration,
        ref_div,
        main_div,
        bandwidth,
        order,
        baseline,
        angle_1,
        angle_2,
    )
