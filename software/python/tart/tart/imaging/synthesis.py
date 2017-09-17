from tart.imaging import uvfitsgenerator
from tart.imaging import radio_source
from tart.imaging import location

from tart.simulation import antennas
from tart.util import skyloc
from tart.util import constants
from tart.util import angle

import numpy as np
#import pyfftw.interfaces.numpy_fft as fft
import numpy.fft as fft
import time

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

def get_difmap(fits_file):
  difmap = """! Basic imaging instructions by Tim Molteno
debug = False
observe {}
select I, 1,5
mapcolor color
device uvplot{}.png/png
uvplot
mapsize 1024, 1265624.0
device beam{}.png/png
mappl beam
i = 0
docont = false
repeat
    clean 100,0.05
    selfcal
    i = i+1
until(i > 35)
device map{}.png/png
mappl cln
exit""".format(fits_file, fits_file, fits_file, fits_file)
  return difmap


def get_max_ang(nw, num_bin):
  ret = np.degrees(num_bin/(4.*nw))
  return ret

class Synthesis_Imaging(object):
  def __init__(self, cal_vis_list, fixed_zenith=True):
    self.cal_vis_list = cal_vis_list
    self.fixed_zenith = fixed_zenith
    if self.fixed_zenith:
      self.phase_center = None
    else:
      vt = self.cal_vis_list[0]
      ra, dec = location.get_loc(vt.get_config()).horizontal_to_equatorial(vt.get_timestamp(), angle.from_dms(90), angle.from_dms(0))
      self.phase_center = radio_source.CosmicSource(ra, dec, 1e10)
    self.grid_file = 'grid.idx'
    self.grid_idx = None
    #print 'debug:' , self.phase_center.to_horizontal(vt.config.get_loc(),vt.timestamp)

  def set_grid_file(self,fpath):
    self.grid_file = fpath

  def get_uvfits(self, fname = "out.uvfits"):
    #os.system("rm out.uvfits")
    fits_name = fname
    gen = uvfitsgenerator.UVFitsGenerator(copy.deepcopy(self.cal_vis_list), self.phase_center)
    gen.write(fits_name)
    difcmd = get_difmap(fits_name)
    return difcmd, fits_name
    #f = open('difmap_cmds', 'w')
    #f.write(difcmd)
    #f.close()
    #os.system("difmap < difmap_cmds")


  def get_difmap_movie(self, base_index, frames):
    os.system("rm out.uvfits")
    fits_name = "out.uvfits" #self.fname + ".uvfits"
    for i_frame in frames:
      v_list = copy.deepcopy(self.cal_vis_list)
      vis_indexes = base_index + i_frame
      vis_part = [v_list[ni] for ni in vis_indexes]

      # ra, dec = vis_part[0].config.get_loc().horizontal_to_equatorial(vis_part[0].timestamp, angle.from_dms(90.), angle.from_dms(0.))
      # self.phase_center = radio_source.CosmicSource(ra, dec)
      # print ra, dec

      uvgen = uvfitsgenerator.UVFitsGenerator(vis_part, self.phase_center) # FIXME
      uvgen.write(fits_name)
      difcmd = get_difmap(fits_name, i_frame)
      f = open('difmap_cmds', 'w')
      f.write(difcmd)
      f.close()
      os.system("difmap < difmap_cmds")
      os.system("rm out.uvfits")

  def get_uuvvwwvis_zenith(self):
    vis_l = []
    #for cal_vis in copy.deepcopy(self.cal_vis_list[:1]):
    for cal_vis in self.cal_vis_list[:1]:
      ant_p = np.array(cal_vis.get_config().get_antenna_positions())
      bls = cal_vis.get_baselines()
      pos_pairs = ant_p[np.array(bls)]
      uu_a, vv_a, ww_a = (pos_pairs[:,0] - pos_pairs[:,1]).T/constants.L1_WAVELENGTH
      for bl in bls:
        ant_i, ant_j = bl
        vis_l.append(cal_vis.get_visibility(ant_i,ant_j))
    return uu_a, vv_a, ww_a, np.array(vis_l)

  def get_grid_idxs(self,uu_a, vv_a, num_bin, nw):
    try:
      if self.grid_idx is None:
        import cPickle
        self.grid_idx = cPickle.load(open(self.grid_file, 'rb'))
        #print 'finished loading ' + self.grid_file
    except:
      print 'generating...'
      uu_edges = np.linspace(-nw, nw, num_bin+1)
      vv_edges = np.linspace(-nw, nw, num_bin+1)
      grid_idx = []
      for uu, vv in zip(uu_a, vv_a):
        i = uu_edges.__lt__(uu).sum()-1
        j = vv_edges.__lt__(vv).sum()-1
        i2 = uu_edges.__lt__(-uu).sum()-1
        j2 = vv_edges.__lt__(-vv).sum()-1
        grid_idx.append([i,j,i2,j2])
      self.grid_idx = np.array(grid_idx)
      save_ptr = open(self.grid_file, 'wb')
      cPickle.dump(self.grid_idx, save_ptr, cPickle.HIGHEST_PROTOCOL)
      save_ptr.close()
    return self.grid_idx


  def get_uvplane_zenith(self, num_bin = 1600, nw = 36,):
    uu_a, vv_a, ww_a, vis_l = self.get_uuvvwwvis_zenith()
    arr = np.zeros((num_bin, num_bin), dtype=np.complex64)
    # place complex visibilities in the UV grid and prepare averaging by counting entries.
    grid_idxs = self.get_grid_idxs(uu_a, vv_a, num_bin, nw)
    count_arr = np.zeros((num_bin, num_bin), dtype=np.int16)
    #for k, v_l in enumerate(vis_l):
    #  i,j,i2,j2 = grid_idxs[k]
    #  count_arr[j, i] += 1
    #  count_arr[j2, i2] += 1
    if count_arr.max()>1:
      # apply the masked array and divide by number of entries
      for k, v_l in enumerate(vis_l):
        i,j,i2,j2 = grid_idxs[k]
        arr[j, i] += v_l
        arr[j2, i2] += np.conjugate(v_l)
      n_arr = n_arr/(count_arr)
    else:
      arr[grid_idxs[:,1],grid_idxs[:,0]] = vis_l
      arr[grid_idxs[:,3],grid_idxs[:,2]] = np.conjugate(vis_l)
    n_arr = np.ma.masked_array(arr[:, :], count_arr.__lt__(1.))
    return n_arr

  def get_uvplane(self, num_bin = 1600, nw = 36, grid_kernel_r_pixels=0.5, use_kernel=False):

    for cal_vis in (self.cal_vis_list):
      if self.fixed_zenith:
        c = cal_vis.get_config()
        ant_p = np.asarray(c.get_antenna_positions())

        bls = np.asarray(cal_vis.get_baselines())
        bl_pos = ant_p[bls]
        uu_a, vv_a, ww_a = (bl_pos[:,0] - bl_pos[:,1]).T/constants.L1_WAVELENGTH
        vis_l = cal_vis.get_all_visibility()

      else:
        uu_l = []
        vv_l = []
        ww_l = []
        vis_l = []

        ts = cal_vis.get_timestamp()
        ra, dec = self.phase_center.radec(ts)
        c = cal_vis.get_config()
        ant_p = np.asarray(c.get_antenna_positions())
        loc = location.get_loc(c)
        bls = cal_vis.get_baselines()

        for bl in bls:
          ant_i, ant_j = bl
          a0 = antennas.Antenna(loc, ant_p[ant_i])
          a1 = antennas.Antenna(loc, ant_p[ant_j])
          uu, vv, ww = antennas.get_UVW(a0, a1, ts, ra, dec)
          uu_l.append(uu/constants.L1_WAVELENGTH)
          vv_l.append(vv/constants.L1_WAVELENGTH)
          ww_l.append(ww/constants.L1_WAVELENGTH)
          vis_l.append(cal_vis.get_visibility(ant_i,ant_j))
        uu_a = np.asarray(uu_l)
        vv_a = np.asarray(vv_l)
        ww_a = np.asarray(ww_l)
        vis_l = np.asarray(vis_l)

    uu_edges = np.linspace(-nw, nw, num_bin+1)
    vv_edges = np.linspace(-nw, nw, num_bin+1)

    if use_kernel==False:

      n_arr = np.zeros((num_bin, num_bin), dtype=np.complex64)
      uu_comb = np.concatenate((uu_a, -uu_a))
      vv_comb = np.concatenate((vv_a, -vv_a))
      all_v   = np.concatenate((vis_l, np.conjugate(vis_l)))
      h_real,_,_ = np.histogram2d(vv_comb, uu_comb, weights = all_v.real, bins=[vv_edges, uu_edges])
      h_imag,_,_ = np.histogram2d(vv_comb, uu_comb, weights = all_v.imag, bins=[vv_edges, uu_edges])
      num_entries,_,_ = np.histogram2d(vv_comb, uu_comb, bins=[vv_edges, uu_edges])
      n_arr[:,:] = (h_real+(1j*h_imag))
      pos = np.where(num_entries.__gt__(1))
      n_arr[pos] /= num_entries[pos]

    else:
      pixels_per_wavelength = num_bin/(nw*2.)
      halfbin = float(nw)/(num_bin)
      mid_points_uv = np.mgrid[-nw+halfbin:nw-halfbin:num_bin*1j, -nw+halfbin:nw-halfbin:num_bin*1j]
      n_arr = np.zeros((num_bin, num_bin), dtype=complex)

      r_noise_wavelengths = 0.1
      grid_kernel_r_wavelength = r_noise_wavelengths + grid_kernel_r_pixels / pixels_per_wavelength
      offset_px = np.ceil(grid_kernel_r_wavelength * pixels_per_wavelength)
      offsets = np.arange(-offset_px, offset_px+1)


      vis_max_abs = np.max(np.abs(vis_l))
      for uu, vv, v_l in zip(uu_a, vv_a, vis_l):
        i = uu_edges.__lt__(uu).sum()-1
        j = vv_edges.__lt__(vv).sum()-1
        # print 'u', mid_points_uv[0][i-1,0], mid_points_uv[0][i,0], mid_points_uv[0][i+1,0], uu
        # print 'v', mid_points_uv[1][0,j], vv
        for i_offset in offsets:
          for j_offset in offsets:
            r = np.sqrt(np.power(uu-mid_points_uv[0][i+i_offset,0],2) + np.power(vv-mid_points_uv[1][0,j+j_offset],2))
            # print 'u', uu- mid_points_uv[0][i+i_offset,0], r , grid_kernel_r_pixels/pixels_per_wavelength
            # print 'v', vv- mid_points_uv[1][0,j+j_offset], r , grid_kernel_r_pixels/pixels_per_wavelength
            n_arr[j+j_offset, i+i_offset] += v_l * np.exp(-(r**2. / (grid_kernel_r_wavelength)**2.))
            # print i,j, i_offset, j_offset, r, v_l * np.exp(-(r**2. / (grid_kernel_r_pixels/pixels_per_wavelength)**2.))
        i = uu_edges.__lt__(-uu).sum()-1
        j = vv_edges.__lt__(-vv).sum()-1
        for i_offset in offsets:
          for j_offset in offsets:
            r = np.sqrt(np.power(-uu-mid_points_uv[0][i+i_offset,0],2) + np.power(-vv-mid_points_uv[1][0,j+j_offset],2))
            n_arr[j+j_offset, i+i_offset] += np.conjugate(v_l) * np.exp(-(r**2 / (grid_kernel_r_wavelength)**2))
      # apply the masked array and divide by number of entries
        mask = np.abs(n_arr).__gt__(0.)
        #mask = np.abs(n_arr).__gt__(vis_max_abs)
        n_arr[mask] = n_arr[mask]/np.abs(n_arr[mask])
    return (n_arr, uu_edges, vv_edges)


  def get_ift_simp(self, nw = 30, num_bin = 2**7):
    uv_plane = self.get_uvplane_zenith(num_bin=num_bin, nw=nw)
    ift = np.fft.fftshift(fft.ifft2(np.fft.ifftshift(uv_plane)))
    maxang = get_max_ang(nw, num_bin)
    extent = [maxang, -maxang, -maxang, maxang]
    return [ift, extent]


  def get_ift(self, nw = 30, num_bin = 2**7, use_kernel=False):
    uv_plane, uu_edges, vv_edges = self.get_uvplane(num_bin=num_bin, nw=nw, use_kernel=use_kernel)
    ift = np.fft.fftshift(fft.ifft2(np.fft.ifftshift(uv_plane)))
    maxang = get_max_ang(nw, num_bin)
    extent = [maxang, -maxang, -maxang, maxang]
    return [ift, extent]


  def get_image(self, nw = 30, num_bin = 2**7, pax=0, ex_ax=0):

    ift, extent = self.get_ift(nw = nw, num_bin = num_bin)
    absift = np.abs(ift)

    if pax!=0:
      if len(pax)==4:
        def convert_to_polar(x, y):
          theta = np.arctan2(x, y)
          r = np.sqrt(x**2 + y**2)
          return theta, r
        maxang = get_max_ang(nw, num_bin)
        grid_l, grid_m = np.mgrid[-maxang:maxang:num_bin*1j, -maxang:maxang:num_bin*1j]
        grid_phi, grid_r = convert_to_polar(grid_l,grid_m)
        idx = np.unravel_index(absift.argmax(), absift.shape)
        print (grid_phi[idx]*180/np.pi, 90.-grid_r[idx])

        uv_plane, uu_edges, vv_edges = self.get_uvplane(num_bin=num_bin, nw=nw, use_kernel=True)
        beam_ift = np.fft.fftshift(np.fft.ifft2(np.fft.ifftshift(1-uv_plane.__ne__(0))))
        pax[0].pcolormesh(grid_phi, grid_r, absift)
        pax[1].imshow(absift, origin='lower', extent=extent, interpolation='nearest')
        pax[2].imshow(np.abs(beam_ift), origin='lower', extent=extent, interpolation='nearest')
        pax[3].imshow(np.abs(uv_plane), origin='lower', extent=[uu_edges[-1], uu_edges[0], vv_edges[0], vv_edges[-1]], interpolation='nearest')
      else:
        return pax[1].imshow(absift, origin='lower', extent=extent, interpolation='nearest')

    else:
      if ex_ax==0:
        plt.imshow(absift, origin='lower', extent=extent, interpolation='nearest')
        return ift
      else:
        ex_ax.imshow(absift, origin='lower', extent=extent, interpolation='nearest')
        return ift
