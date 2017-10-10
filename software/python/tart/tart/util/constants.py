"""
Constants used throughout the TART

"""
import numpy as np

V_LIGHT = 2.99793e8 # Speed of light
TWO_PI = 2.0*np.pi

L1_FREQ = 1.57542e9
L1_OMEGA = 2.*np.pi*L1_FREQ
L1_WAVELENGTH = V_LIGHT / L1_FREQ
