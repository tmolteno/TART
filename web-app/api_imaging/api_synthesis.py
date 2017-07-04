
import requests
from requests.auth import HTTPDigestAuth
import json

# Replace with the correct URL
api_endpoint = "http://tart2-raspberry/api/v1"

def get_from_api(view):
    myResponse = requests.get(api_endpoint+view,)
    if(myResponse.ok):
        jData = json.loads(myResponse.content)
    else:
        myResponse.raise_for_status()
    return jData

def get_status():
    return get_from_api('/mode')

def api_get_vis():
    res = get_from_api('/imaging/vis')
    v_complex = {}
    print res
    for key, v in res.iteritems():
      v_real, v_imag = v
      v_complex[key] = v_real*1 + v_imag*1j
    return v_complex

def api_get_ant_pos():
    return get_from_api('/imaging/antenna_positions')

def api_get_timestamp():
    return get_from_api('/imaging/timestamp')

import numpy as np

get_status()

vis = api_get_vis()
ant_pos = api_get_ant_pos()
ts = api_get_timestamp()

nw = 30 # Number of wavelengths
num_bin = 2**7 # bins in the fft

L1_WAVELENGTH = 0.1905 # meter

def get_uuvvwwvis_zenith():
  bls = vis.keys()
  uu_a = []
  vv_a = []
  ww_a = []
  vis_l = []
  for ant_pair in bls:
    a1, a2 = eval(ant_pair)
    uu_a.append( (ant_pos[a1][0] - ant_pos[a2][0])/L1_WAVELENGTH )
    vv_a.append( (ant_pos[a1][1] - ant_pos[a2][1])/L1_WAVELENGTH )
    ww_a.append( (ant_pos[a1][2] - ant_pos[a2][2])/L1_WAVELENGTH )
    vis_l.append(vis[ant_pair])
  return uu_a, vv_a, ww_a, vis_l

print get_uuvvwwvis_zenith()

def get_grid_idxs(uu_a, vv_a, num_bin, nw):
  uu_edges = np.linspace(-nw, nw, num_bin+1)
  vv_edges = np.linspace(-nw, nw, num_bin+1)
  grid_idx = []
  for uu, vv in zip(uu_a, vv_a):
    print uu
    i = uu_edges.__lt__(uu).sum()-1
    j = vv_edges.__lt__(vv).sum()-1
    i2 = uu_edges.__lt__(-uu).sum()-1
    j2 = vv_edges.__lt__(-vv).sum()-1
    grid_idx.append([i,j,i2,j2])
  grid_idx = np.array(grid_idx)
  return grid_idx

def get_uvplane_zenith(num_bin = 1600, nw = 36,):
  uu_a, vv_a, ww_a, vis_l = get_uuvvwwvis_zenith()
  arr = np.zeros((num_bin, num_bin), dtype=np.complex64)
  # place complex visibilities in the UV grid and prepare averaging by counting entries.
  grid_idxs = get_grid_idxs(uu_a, vv_a, num_bin, nw)
  arr[grid_idxs[:,1],grid_idxs[:,0]] = vis_l
  arr[grid_idxs[:,3],grid_idxs[:,2]] = np.conjugate(vis_l)
  return arr

uv_plane = get_uvplane_zenith(num_bin=num_bin,nw=nw)


def max_ift_shift(arr):
  ret = np.zeros_like(arr)
  arr_len = len(arr);
  arr_len2 = arr_len/2;

  for i in range(arr_len):
    for j in range(arr_len):
      idx = (i+arr_len2)%arr_len;
      idy = (j+arr_len2)%arr_len;
      ret[idy,idx] = arr[j,i]

  return ret


ift = np.fft.fftshift(np.fft.ifft2(np.fft.ifftshift(uv_plane)))
ift_max = max_ift_shift(np.fft.ifft2(max_ift_shift(uv_plane)))

maxang = 1./(2*(nw*2.)/num_bin)*(180./np.pi)
extent = [maxang, -maxang, -maxang, maxang]
ift, extent

import matplotlib.pyplot as plt

plt.figure()
plt.imshow(np.abs(ift),extent=extent,origin='upper')


plt.figure()
plt.imshow(np.abs(ift_max),extent=extent, origin='upper')

plt.show()

