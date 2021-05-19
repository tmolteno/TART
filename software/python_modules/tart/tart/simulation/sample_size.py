# -*- coding: utf-8 -*-

# This module calculates the length of sample required from each
# astronomical radio noise source given parameters including
# the phase delays required

# An example to aid understanding;
# -3 -2 -1 0 1 2 3 4 5 ... E E+1 E+2 E+3	A sampled radio source
#     ^						0th pos phase leads 2
#              ^				Oth pos phase lags 2

# Max lag represents the position of the zero value
# Phase Lead is positive number, Phase Lag is a negative number
import numpy as np


def sample_size(ref_freq, freq_mult, sample_duration, sample_delay):

    # Calculate the required sample size excluding offsets
    base_size = int(ref_freq * freq_mult * sample_duration)

    # Calculate max and min offsets for each signal
    extrema = np.zeros((2, sample_delay.shape[0]))  # To hold extreme offsets
    extrema[0] = sample_delay.max(axis=1)  # Max values
    extrema[1] = sample_delay.min(axis=1)  # Min values

    # Calculate the sample size given the offsets
    sizes = np.ones(extrema.shape[1])  # blank array to hold sample sizes
    for sig in range(0, extrema.shape[1]):
        sizes[sig] = base_size + extrema[0, sig] - extrema[1, sig]  # Row 1 -ve vals

    max_size = sizes.max(0)

    # The maximum values indicate
    zero_off = extrema[0]

    return zero_off, max_size, base_size


if __name__ == "__main__":

    ref_freq = 16.368e6
    freq_mult = 256
    sample_duration = 1e-3

    sample_delay = np.array(
        [
            [-49, -39, -29, -19, -9, 0, 9, 19, 29, 39, 49],
            [-34, -27, -20, -13, -6, 0, 6, 13, 20, 27, 34],
            [-18, -14, -10, -7, -3, 0, 3, 7, 10, 14, 18],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [18, 14, 10, 7, 3, 0, -3, -7, -10, -14, -18],
            [34, 27, 20, 13, 6, 0, -6, -13, -20, -27, -34],
            [49, 39, 29, 19, 9, 0, -9, -19, -29, -39, -49],
        ]
    )

    zero_off, max_size, base_size = sample_size(
        ref_freq, freq_mult, sample_duration, sample_delay
    )
    print("Jump this many places from left to find phase zero position for each sample")
    print(zero_off)
    print("The base size is ", base_size)
    print("Length of sample required to accommodate all phase changes is ", max_size)
