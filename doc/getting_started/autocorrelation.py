#
# Tim Molteno (2018-2022)
# tim@elec.ac.nz
#
# About:
#    Simple script demonstrate how to image from raw data
#
# Requires:
#   - tart python package to be installed (sudo pip3 install tart)
#   - raw data HDF file downloaded from the telescope web front end
#
#
# Usage:
#   python3 image_from_raw.py --file  ~/Downloads/data_2019-07-24_22_47_14.353231.hdf

import argparse

import numpy as np
import numpy.fft as fft

import matplotlib.pyplot as plt

from tart.operation import observation
from tart.imaging import correlator
from tart.util import constants

from tart_tools import api_handler

'''
    Create an image from raw (hdf5) data downloaded from the TART telescope
    
    Step 1: Download raw data (
        tart_set_mode --raw --pw <password>
        tart_download_data --raw --n 1 --pw <password>
        tart_set_mode --vis --pw <password>
        
    Step 2: Run this file
        python3 image_from_raw.py --file ./data_2022-06-21_01_57_06.161084.hdf

'''
if __name__ == '__main__':
    PARSER = argparse.ArgumentParser(description='Create an image from raw (hdf5) data downloaded from the TART telescope.')
    PARSER.add_argument('--file', required=True, help="The raw data data file ")

    ARGS = PARSER.parse_args()
    
    #############################################################################################################
    #
    #                                    Step 1. Correlate the data 
    #
    #############################################################################################################
    # Load the Observation file
    obs = observation.Observation_Load(ARGS.file)

    n_ant = obs.config.get_num_antenna()
    for i in range(1):
        y = obs.get_antenna(i)
        acorr = np.correlate(y, y, mode = 'full')
        plt.semilogy(acorr, label=f"{i}")
        break
    plt.legend()
    plt.show()
