# -*- coding: utf-8 -*-
import numpy as np

# Displacement is the (+/-) displacement of an antenna from the origin
# Angle is in degrees. Clockwise angles from vertical +ve, anticlockwise -ve.
# A +ve delay indicates antenna leads center, -ve that it lags.
# Variable "sample_delay" is in seconds.


def sample_delay(displacement, angle):
    sample_delay = displacement * np.sin(angle * np.pi / 180) / 2.99793e8
    return sample_delay


if __name__ == "__main__":

    displacement = -5
    angle = 45
    delay = sample_delay(displacement, angle)
    print(delay)
