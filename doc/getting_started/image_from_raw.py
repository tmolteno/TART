#
# Tim Molteno (2018-2019)
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

if __name__ == '__main__':
    PARSER = argparse.ArgumentParser(description='Acquire data from the TART telescope.')
    PARSER.add_argument('--file', required=True, help="The raw data data file ")

    ARGS = PARSER.parse_args()
    
    #############################################################################################################
    #
    #                                    Step 0. Use the API to get information 
    #
    #############################################################################################################
    API_SERVER = 'https://tart.elec.ac.nz/signal'
    api = api_handler.APIhandler(API_SERVER)
    
    ant_pos = np.array(api.get('imaging/antenna_positions'))
    
    calibration_gains = api.get('calibration/gain')
    gains = np.array(calibration_gains['gain'])
    phases = np.array(calibration_gains['phase_offset'])


    
    #############################################################################################################
    #
    #                                    Step 1. Correlate the data 
    #
    #############################################################################################################
    # Load the Observation file
    obs = observation.Observation_Load(ARGS.file)

    corr = correlator.Correlator()
    vis = corr.correlate(obs)
    print("Timestamp: {}".format(vis.timestamp))
    print("Config: {}".format(vis.config.Dict))
    print("Baselines: {}".format(vis.baselines))
    print("visibilities: {}".format(vis.v))
    
    baselines = np.asarray(vis.baselines)
    v_arr = np.asarray(vis.v)


    #############################################################################################################
    #
    #                                    Step 2. Apply the calibration gains and phases 
    #
    #############################################################################################################
    ## Multiply the visiblities by the complex gains
    bl = baselines
    vis_l = v_arr * gains[bl[:,0]] * gains[bl[:,1]] * np.exp(-1j*(phases[bl[:,0]]-phases[bl[:,1]]))


    #############################################################################################################
    #
    #                                    Step 3. Perform the inverse FFT imaging.
    #
    #############################################################################################################
    num_bin = 2**9  # Image resolution
    nw=num_bin/4

    bl_pos = ant_pos[baselines]
    uu_a, vv_a, ww_a = (bl_pos[:,0] - bl_pos[:,1]).T/constants.L1_WAVELENGTH

    # Grid the visibilities in the UV plane.
    uu_edges = np.linspace(-nw, nw, num_bin+1)
    vv_edges = np.linspace(-nw, nw, num_bin+1)

    uv_plane = np.zeros((num_bin, num_bin), dtype=np.complex64)
    uu_comb = np.concatenate((uu_a, -uu_a))
    vv_comb = np.concatenate((vv_a, -vv_a))
    all_v     = np.concatenate((vis_l, np.conjugate(vis_l)))
    h_real,_,_ = np.histogram2d(vv_comb, uu_comb, weights = all_v.real, bins=[vv_edges, uu_edges])
    h_imag,_,_ = np.histogram2d(vv_comb, uu_comb, weights = all_v.imag, bins=[vv_edges, uu_edges])
    num_entries,_,_ = np.histogram2d(vv_comb, uu_comb, bins=[vv_edges, uu_edges])
    uv_plane[:,:] = (h_real+(1j*h_imag))
    pos = np.where(num_entries.__gt__(1))
    uv_plane[pos] /= num_entries[pos]


    cal_ift = np.fft.fftshift(fft.ifft2(np.fft.ifftshift(uv_plane)))

    # Take the absolute value to make an intensity image
    img = np.abs(cal_ift)
    # Scale it to multiples of the image standard deviation
    img /= np.std(img)


    #############################################################################################################
    #
    #                                    Step 4. Plot the image.
    #
    #############################################################################################################
    plt.figure(figsize=(8, 6), dpi=num_bin/6)
    plt.title("Inverse FFT image")

    print("Dynamic Range: {}".format(np.max(img)))

    plt.imshow(img, extent=[-1, 1, -1, 1])

    plt.xlim(-1, 1)
    plt.ylim(-1, 1)
    cb = plt.colorbar()
    plt.savefig('raw_image.jpg')
    plt.show()
