from tart.imaging import uvfitsgenerator
from tart.imaging import radio_source
from tart.simulation import antennas
from tart.util import skyloc
from tart.util import constants
from tart.util import angle

import numpy as np
import pyfftw.interfaces.numpy_fft as fft

import os
import copy

import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable

cc = np.concatenate

# def get_difmap(fits_file, o=0):
#   difmap = "! Basic imaging instructions by Tim Molteno\n\
#   debug = False\n\
# observe %s\n\
# select I, 1,5\n\
# mapcolor color\n\
# device uv/uvplot%04d.png/png\n\
# uvplot\n\
# mapsize 1024, 1.25e6\n\
# device beam/beam%04d.png/png\n\
# mappl beam\n\
# device map/map%04d.png/png\n\
# mappl\n\
# exit\n" % (fits_file, o, o, o)
#   return difmap

#mapsize 1024, 1.25e6\n\

def add_subplot_axes(ax,rect,axisbg='w'):
    fig = plt.gcf()
    box = ax.get_position()
    width = box.width
    height = box.height
    inax_position  = ax.transAxes.transform(rect[0:2])
    transFigure = fig.transFigure.inverted()
    infig_position = transFigure.transform(inax_position)    
    x = infig_position[0]
    y = infig_position[1]
    width *= rect[2]
    height *= rect[3]
    subax = fig.add_axes([x,y,width,height],axisbg=axisbg)
    x_labelsize = subax.get_xticklabels()[0].get_size()
    y_labelsize = subax.get_yticklabels()[0].get_size()
    x_labelsize *= rect[2]**0.5
    y_labelsize *= rect[3]**0.5
    subax.xaxis.set_tick_params(labelsize=x_labelsize)
    subax.yaxis.set_tick_params(labelsize=y_labelsize)
    return subax

def get_difmap(fits_file, o=0):
  difmap = "! Basic imaging instructions by Tim Molteno\n\
  debug = False\n\
observe %s\n\
select I, 1,5\n\
mapcolor color\n\
device uv/uvplot%04d.png/png\n\
uvplot\n\
mapsize 2048, 1.25e5\n\
device beam/beam%04d.png/png\n\
mappl beam\n\
device map/map%04d.png/png\n\
mappl\n\
exit\n" % (fits_file, o, o, o)
  return difmap

class Synthesis_Imaging(object):
  def __init__(self, vis_list):
    self.vis_list = vis_list
    # vt = self.vis_list[int(len(self.vis_list)/2)]
    vt = self.vis_list[0]
    print vt.config
    ra, dec = vt.config.get_loc().horizontal_to_equatorial(vt.timestamp, angle.from_dms(90.), angle.from_dms(0.))
    # ra, dec = vt.config.get_loc().horizontal_to_equatorial(vt.timestamp, angle.from_dms(90.), angle.from_dms(0.))
    # dec = angle.from_dms(-90.00)
    # print 'phasecenter:', ra, dec
    self.phase_center = radio_source.CosmicSource(ra, dec)

  def get_uvfits(self):
    os.system("rm out.uvfits")
    fits_name = "out.uvfits" #self.fname + ".uvfits"
    gen = uvfitsgenerator.UVFitsGenerator(copy.deepcopy(self.vis_list), self.phase_center)
    gen.write(fits_name)
    difcmd = get_difmap(fits_name)
    f = open('difmap_cmds', 'w')
    f.write(difcmd)
    f.close()
    os.system("difmap < difmap_cmds")


  def get_difmap_movie(self, base_index, frames):
    os.system("rm out.uvfits")
    fits_name = "out.uvfits" #self.fname + ".uvfits"
    aaa = self.vis_list
    for i_frame in frames:
      v_list = copy.deepcopy(aaa)
      vis_indexes = base_index + i_frame
      vis_part = [v_list[ni] for ni in vis_indexes]
      
      # ra, dec = vis_part[0].config.get_loc().horizontal_to_equatorial(vis_part[0].timestamp, angle.from_dms(90.), angle.from_dms(0.))
      # self.phase_center = radio_source.CosmicSource(ra, dec)
      # print ra, dec

      uvgen = uvfitsgenerator.UVFitsGenerator(vis_part, self.phase_center)
      uvgen.write(fits_name)
      difcmd = get_difmap(fits_name, i_frame)
      f = open('difmap_cmds', 'w')
      f.write(difcmd)
      f.close()
      os.system("difmap < difmap_cmds")
      os.system("rm out.uvfits")

  def get_uvplane(self, vis_list, num_bin = 1600, nw = 36):
    uu_l = []
    vv_l = []
    ww_l = []
    vis_l = []
    for v in copy.deepcopy(vis_list):
      # print 'above horizon?', self.phase_center.to_horizontal(v.config.get_loc(),v.timestamp)
      ra, dec = self.phase_center.radec(v.timestamp)
      v.rotate(skyloc.Skyloc(ra, dec))
      for i in range(v.config.num_baselines):
        a0 = antennas.Antenna(v.config.get_loc(), v.config.ant_positions[v.baselines[i][0]])
        a1 = antennas.Antenna(v.config.get_loc(), v.config.ant_positions[v.baselines[i][1]])
        uu, vv, ww = antennas.get_UVW(a0, a1, v.timestamp, ra, dec)
        uu_l.append(uu/constants.L1_WAVELENGTH)
        vv_l.append(vv/constants.L1_WAVELENGTH)
        ww_l.append(ww/constants.L1_WAVELENGTH)
        vis_l.append(v.v[i])

    uu_a = np.array(uu_l)
    vv_a = np.array(vv_l)
    ww_a = np.array(ww_l)
    vis_l = np.array(vis_l)

    outest_point = max(uu_a.max(), vv_a.max(), -vv_a.min(), -uu_a.min())
    # xedges = np.linspace(-outest_point, outest_point, num_bin+1)
    # yedges = np.linspace(-outest_point, outest_point, num_bin+1)
    if outest_point>nw:
      'nw is number of wavelengths and describes the size of the UV plane'
      raise

    xedges = np.linspace(-nw, nw, num_bin+1)
    yedges = np.linspace(-nw, nw, num_bin+1)

    arr = np.zeros((num_bin, num_bin, 2), dtype=complex)
    # place complex visibilities in the UV grid and prepare averaging by counting entries.
    for uu, vv, v_l in zip(uu_a, vv_a, vis_l):
      i = xedges.__lt__(uu).sum()-1
      j = yedges.__lt__(vv).sum()-1
      arr[j, i, 0] += v_l
      arr[j, i, 1] += 1.
      i = xedges.__lt__(-uu).sum()-1
      j = yedges.__lt__(-vv).sum()-1
      arr[j, i, 0] += np.conjugate(v_l)
      arr[j, i, 1] += 1.
    # apply the masked array and divide by number of entries
    n_arr = np.ma.masked_array(arr[:, :, 0], arr[:, :, 1].real.__lt__(1.))
    n_arr = n_arr/arr[:, :, 1].real

    return (n_arr, xedges, yedges) #uv_plane

  def get_image(self, num_bin=400, pax=0):

    num_bin = 2*800 #2*800
    nw = 120*2

    uv_plane, xedges, yedges = self.get_uvplane(self.vis_list, num_bin, nw)
    maxang = 1./(2*(nw*2.)/num_bin)*(180./np.pi)

    ift = np.fft.fftshift(np.fft.ifft2(np.fft.ifftshift(uv_plane)))
    if pax!=0:
      if len(pax)==2:
        pax[0].imshow(np.abs(ift), extent=[maxang, -maxang, -maxang, maxang], interpolation='nearest')
        pax[1].imshow(uv_plane.real, extent=[xedges[-1], xedges[0], yedges[0], yedges[-1]], interpolation='nearest')
        k = np.power(uv_plane.real.sum(axis=1),0).sum()*1.05/2
        pax[1].set_xlim(xedges[len(xedges)/2-k], xedges[len(xedges)/2+k])
        pax[1].set_ylim(xedges[len(xedges)/2-k], xedges[len(xedges)/2+k])
      else:
        cropped = np.abs(ift)
        x,y = cropped.shape
        cropped = cropped[x*3/8: - x*3/8, y*3/8: -y*3/8]
        dx = 35 
        dy = -35
        return pax[0].imshow(cropped, extent=[maxang/4 + dx, -maxang/4 + dx, -maxang/4+ dy, maxang/4+ dy], interpolation='nearest')


    else:
      plt.imshow(np.abs(ift), extent=[maxang, -maxang, -maxang, maxang], interpolation='nearest')
      plt.imshow(uv_plane.real, extent=[xedges[-1], xedges[0], yedges[0], yedges[-1]], interpolation='nearest')

