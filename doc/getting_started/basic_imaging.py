'''
    Simple TART imaging script from api visibilities.

    Tim Molteno 2018-2019.

    This requires the TART api_imaging and api_handler packages to be installed on your system
    from the public repositories:

    sudo pip3 install tart_tools
'''

import numpy as np
import matplotlib.pyplot as plt
import requests
import numpy.fft as fft

#
# The API server URL identifies which TART telescope to use.
#
# Documentation URL. https://api.elec.ac.nz/tart/mu-udm/doc/

API_SERVER = 'https://api.elec.ac.nz/tart/mu-udm'

def get_api(api):
    response = requests.get(f"{API_SERVER}/api/v1/{api}")
    return response.json()

'''
    STEP 1:

    Get telescope configuration (config), visibility data, and calibration data (gains) from the server
'''
print(f"Downloading data from {API_SERVER}")
mode = get_api('mode/current')

if mode['mode'] != 'vis':
    print("ERROR: Telescope must be in visibility mode to allow imaging. Set via the web API")

gains = get_api('calibration/gain')
visibility_data = get_api('imaging/vis')
ant_pos = get_api('imaging/antenna_positions')

print(f"Visibilities time: {visibility_data['timestamp']}")

ant_pos = np.array(ant_pos)



'''
    STEP 2: Calibration of the visiblilties
'''
print("Apply Calibration Data")

gains_complex = np.array(gains['gain']) * np.exp(1.0j*np.array(gains['phase_offset']))
uv_max = 0
for v in visibility_data['data']:
    v_complex = v['re'] + v['im']*1.0j
    i = v['i']
    j = v['j']
    v_calib = v_complex * gains_complex[i] * np.conj(gains_complex[j])
    v['cal'] = v_calib
    bl = ant_pos[i] - ant_pos[j]
    if np.linalg.norm(bl) > uv_max:
        uv_max = np.linalg.norm(bl)
    v['bl'] = bl

print(visibility_data['data'][0])
print(f"uvmax: {uv_max}")
# Image resolution

num_bin = 256


'''
    Grid the visibilities in the UV plane.

    I = FT(V*exp(2*pi*j(u*l + v*m)))

    where l, m are the direction cosines
        * l = sin(theta)sin(phi)
        * m = sin(theta)cos(phi)

    we want l and m to go between -1 and 1 as we're doing all sky imaging
'''

wavelength = 0.2
uv_max = uv_max / wavelength

def uv_index(u):
    # u = -nw -> 0
    # u = nw  -> num_bin-1
    du = (u + uv_max)
    scaled = du*(num_bin - 1)
    return int(scaled / (2*uv_max))

uv_plane = np.zeros((num_bin, num_bin), dtype=np.complex64)

for v in visibility_data['data']:
    uu = v['bl'][0]
    vv = v['bl'][1]
    u_idx = uv_index(uu)
    v_idx = uv_index(vv)
    uv_plane[u_idx, v_idx] += v['cal']

plt.figure(figsize=(8, 6), dpi=num_bin/6)
plt.title("U-V plane image")

plt.imshow(np.abs(uv_plane), extent=[-uv_max, uv_max, -uv_max, uv_max])

plt.xlim(-1, 1)
plt.ylim(-1, 1)
cb = plt.colorbar()
plt.savefig('uv_plane.jpg')
plt.show()

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
plt.savefig('basic_image.jpg')
plt.show()
