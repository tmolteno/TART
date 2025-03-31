'''
    Simple TART imaging script from api visibilities.

    Tim Molteno 2018-2025.

    This requires the TART api_imaging and api_handler packages to be installed on your system
    from the public repositories:
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


#############################################################################################################
#
#                                             Step 1.
#
#   Get visibility data, and calibration data (gains) from the TART telescope web interface
#
#############################################################################################################

print(f"Downloading data from {API_SERVER}")
mode = get_api('mode/current')

if mode['mode'] != 'vis':
    print("ERROR: Telescope must be in visibility mode to allow imaging. Set via the web API")

gains = get_api('calibration/gain')
visibility_data = get_api('imaging/vis')
ant_pos = get_api('imaging/antenna_positions')
ant_pos = np.array(ant_pos)

print(f"Visibilities time: {visibility_data['timestamp']}")




#############################################################################################################
#
#                                    Step 2. Apply the calibration to the visibilities.
#
#############################################################################################################

gains_complex = np.array(gains['gain']) * np.exp(1.0j*np.array(gains['phase_offset']))
uv_max = 0
wavelength = 0.2

for v in visibility_data['data']:
    v_complex = v['re'] + v['im']*1.0j
    i = v['i']
    j = v['j']
    v_calib = v_complex * gains_complex[i] * np.conj(gains_complex[j])
    v['cal'] = v_calib

    # Work out the baselines
    bl = ant_pos[j] - ant_pos[i]
    v['bl'] = bl / wavelength


#############################################################################################################
#
#                                    Step 3. Grid the visibilities.
#
#############################################################################################################
'''
    Grid the visibilities in the UV plane.

    I(l,m) = IFFT(V*exp(2*pi*j(u*l + v*m)))

    where l, m are the direction cosines
        * l = sin(theta)sin(phi)
        * m = sin(theta)cos(phi)

    we want l and m to go between -1 and 1 as we're doing all sky imaging
    This means that the uv_plane has to have a dimension per pixel

    Step1.  Choose pixel resolution (radians per pixel) to be full sky
            180 degrees in N_FFT. res = pi / N_FFT

    Step2.  The uv distances are in meters. This can be changed to wavelenths, and
            then to radians (2.pi*u / wavelength). Maximum u,v values should then be
            corresponding to the highest resolution in the image. I.e. radians_per_pixel

    The resolution for a baseline is given by the rayleigh criterion

            resolution =  1.2 lambda / uv_max

    So: 1.2 lambda / uv_max  = pi / N_FFT. ==> N_FFT / pi = uv_max / 1.2 lambda

    uv_max(in lambdas) = N_FFT / 1.2 np.pi
'''
N_FFT = 512
uv_plane = np.zeros((N_FFT, N_FFT), dtype=np.complex64)

uv_max = N_FFT / (1.2 * np.pi)
middle = N_FFT // 2


def uv_index(u):
    pixels = (u / uv_max)*(N_FFT/2)
    u_pix = middle + pixels
    return int(u_pix)


for v in visibility_data['data']:
    uu, vv, ww = v['bl']
    u_idx = uv_index(uu)
    v_idx = uv_index(vv)
    uv_plane[u_idx, v_idx] += v['cal']

    u_idx = uv_index(-uu)
    v_idx = uv_index(-vv)
    uv_plane[u_idx, v_idx] += np.conj(v['cal'])


plt.figure(figsize=(4, 3), dpi=N_FFT/6)
plt.title("U-V plane image")

plt.imshow(np.abs(uv_plane), extent=[-uv_max, uv_max, -uv_max, uv_max])

plt.xlim(-uv_max, uv_max)
plt.ylim(-uv_max, uv_max)
plt.savefig('uv_plane.jpg')
plt.show()

#############################################################################################################
#
#                                    Step 4. Do the inverse fourier transform.
#
#############################################################################################################


cal_ift = np.fft.fftshift(fft.ifft2(np.fft.ifftshift(uv_plane)))

# Take the absolute value to make an intensity image
img = np.abs(cal_ift)
# Scale it to multiples of the image standard deviation
img /= np.std(img)


#############################################################################################################
#
#                                    Step 5. Plot the image.
#
#############################################################################################################
plt.figure(figsize=(4, 3), dpi=N_FFT/4)
plt.title("Inverse FFT image")

print("Dynamic Range: {}".format(np.max(img)))

plt.imshow(img, extent=[-1, 1, -1, 1])

plt.xlim(-1, 1)
plt.ylim(-1, 1)
cb = plt.colorbar()
plt.savefig('basic_image.jpg')
plt.show()
#
