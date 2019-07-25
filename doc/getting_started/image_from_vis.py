'''
    Simple TART imaging script from api visibilities.

    Tim Molteno 2018-2019.

    This requires the TART api_imaging and api_handler packages to be installed on your system
    from the public repositories:

    sudo pip3 install tart_tools
'''

import numpy as np
import matplotlib.pyplot as plt

from tart_tools import api_imaging
from tart_tools import api_handler

# The API server URL identifies which TART telescope to use.
# 
API_SERVER = 'https://tart.elec.ac.nz/signal'


'''
    STEP 1:

    Get telescope configuration (config), visibility data, and calibration data (gains) from the server
'''
print("Downloading data from {}".format(API_SERVER))
api = api_handler.APIhandler(API_SERVER)

config = api_handler.get_config(api)
mode = api.get('mode/current')

if mode['mode'] != 'vis':
    print("ERROR: Telescope must be in visibility mode to allow imaging. Set via the web API")

gains = api.get('calibration/gain')
visibility_data = api.get('imaging/vis')


'''
    STEP 2: Apply Calibration to the visiblilties
'''
print("Apply Calibration Data")
cv, timestamp = api_imaging.vis_calibrated(visibility_data, config, gains['gain'], gains['phase_offset'], flag_list=[])


'''
    STEP 3: Perform the imaging
'''
print("Generate Dirty Image")
n_bin = 2**9  # Image resolution
cal_ift, cal_extent, n_fft, bin_width = api_imaging.image_from_calibrated_vis(cv, nw=n_bin/4, num_bin=n_bin)

# Take the absolute value 
img = np.abs(cal_ift)

# Scale it to multiples of the image standard deviation
sd = np.std(img)
scaled_image = img/sd

'''
    STEP 4: Save the file (with a timestamp)
'''
time_repr = "{:%Y_%m_%d_%H_%M_%S_%Z}".format(timestamp)
fname = 'snapshot_{}.png'.format(time_repr)
api_imaging.make_image(plt, scaled_image, "Image Title", n_bin, None, False)
print("Saving image as {}".format(fname))
plt.savefig(fname)
