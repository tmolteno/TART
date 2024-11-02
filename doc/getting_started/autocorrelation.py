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
import scipy.signal

import numpy as np
import numpy.fft as fft

import matplotlib.pyplot as plt

from tart.operation import observation


def acorr(y):
    ret = scipy.signal.correlate(y, y, mode = 'full')
    return ret

def estimate_weights(obs):
    '''
    The variance of visibility v_ij is v_ii * v_jj / K, in other words the product of the
    corresponding autocorrelations divided by the number of samples that went into
    calculating the visibility. This is based on Wishart statistics. The weight is therefore
    K / (v_ii * v_jj). This is accessed in katdal as the weights property on the dataset
    (and is written to the WEIGHT_SPECTRUM column when converting to MS).
    '''
    n_ant = obs.config.get_num_antenna()
    y = obs.get_antenna(0)
    n = y.shape[0]

    K = n / 2  # Number of samples
    
    variances = [np.sum(acorr(obs.get_antenna(i)))/np.sqrt(n) for i in range(n_ant)]
    print(variances)
    weights = np.zeros((n_ant, n_ant))
    for i in range(n_ant):
        for j in range(n_ant):
            v_ii = variances[i]
            v_jj = variances[j]
            weights[i,j] = K / (v_ii * v_jj)
            
    return weights

    


'''
    Create an image from raw (hdf5) data downloaded from the TART telescope
    
    Step 1: Download raw data (
        tart_set_mode --raw --pw <password>
        tart_download_data --raw --n 1 --pw <password>
        tart_set_mode --vis --pw <password>
        
    Step 2: Run this file
        python3 autocorrelation.py --file ./data_2022-06-21_01_57_06.161084.hdf
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

    w = estimate_weights(obs)
    print(w)
    n_ant = obs.config.get_num_antenna()
    
    N = 4
    fig, ax = plt.subplots(N)

    for i in range(N):
        ax[i].set_yscale("log", nonpositive='clip')
        ax[i].set_ylim(bottom=1e-2, top=1e-1)
        y = obs.get_antenna(i)
        centre = y.shape[0] // 2
        z = acorr(y) / centre
        noncentral_sample = z[centre-1000:centre-10]
        sigma = np.mean(noncentral_sample)
        ax[i].plot(z, label=f"Ant {i}: {sigma :6.3g}")
        ax[i].legend()
        ax[i].grid(True)
    plt.savefig('autocorrelation.png')
    plt.show()
