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
    # print vt.config
    ra, dec = vt.config.get_loc().horizontal_to_equatorial(vt.timestamp, angle.from_dms(90.), angle.from_dms(90.))
    # ra, dec = vt.config.get_loc().horizontal_to_equatorial(vt.timestamp, angle.from_dms(90.), angle.from_dms(0.))
    # dec = angle.from_dms(-90.00)
    # print 'phasecenter:', ra, dec
    self.phase_center = radio_source.CosmicSource(ra, dec)
    #print 'debug:' , self.phase_center.to_horizontal(vt.config.get_loc(),vt.timestamp)

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

  def get_uvplane(self, vis_list, num_bin = 1600, nw = 36, grid_kernel_r_pixels=0.5, use_kernel=True):
    pixels_per_wavelength = num_bin/(nw*2.)

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

    if use_kernel==False:
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
      n_arr = n_arr/(arr[:, :, 1].real)

    else:
      
      halfbin = float(nw)/(num_bin)
      mid_points_uv = np.mgrid[-nw+halfbin:nw-halfbin:num_bin*1j, -nw+halfbin:nw-halfbin:num_bin*1j]
      n_arr = np.zeros((num_bin, num_bin), dtype=complex)

      r_noise_wavelengths = 0.1
      grid_kernel_r_wavelength = r_noise_wavelengths + grid_kernel_r_pixels / pixels_per_wavelength
      offset_px = np.ceil(grid_kernel_r_wavelength * pixels_per_wavelength)
      offsets = np.arange(-offset_px,offset_px+1)


      vis_max_abs = np.max(np.abs(vis_l))
      for uu, vv, v_l in zip(uu_a, vv_a, vis_l):
        i = xedges.__lt__(uu).sum()-1
        j = yedges.__lt__(vv).sum()-1

        # print 'u', mid_points_uv[0][i-1,0], mid_points_uv[0][i,0], mid_points_uv[0][i+1,0], uu
        # print 'v', mid_points_uv[1][0,j], vv

        for i_offset in offsets:
          for j_offset in offsets:
            r = np.sqrt(np.power(uu-mid_points_uv[0][i+i_offset,0],2) + np.power(vv-mid_points_uv[1][0,j+j_offset],2))
            # print 'u', uu- mid_points_uv[0][i+i_offset,0], r , grid_kernel_r_pixels/pixels_per_wavelength
            # print 'v', vv- mid_points_uv[1][0,j+j_offset], r , grid_kernel_r_pixels/pixels_per_wavelength
            n_arr[j+j_offset, i+i_offset] += v_l * np.exp(-(r**2. / (grid_kernel_r_wavelength)**2.))
            # print i,j, i_offset, j_offset, r, v_l * np.exp(-(r**2. / (grid_kernel_r_pixels/pixels_per_wavelength)**2.))

        i = xedges.__lt__(-uu).sum()-1
        j = yedges.__lt__(-vv).sum()-1
        for i_offset in offsets:
          for j_offset in offsets:
            r = np.sqrt(np.power(-uu-mid_points_uv[0][i+i_offset,0],2) + np.power(-vv-mid_points_uv[1][0,j+j_offset],2))
            n_arr[j+j_offset, i+i_offset] += np.conjugate(v_l) * np.exp(-(r**2 / (grid_kernel_r_wavelength)**2))
      # apply the masked array and divide by number of entries

        mask = np.abs(n_arr).__gt__(vis_max_abs)
        n_arr[mask] = n_arr[mask]/np.abs(n_arr[mask])


      # for uu, vv, v_l in zip(uu_a, vv_a, vis_l):
      #   r1 = np.sqrt(np.power(uu-mid_points_uv[0],2) + np.power(vv-mid_points_uv[1],2))
      #   r2 = np.sqrt(np.power(-uu-mid_points_uv[0],2) + np.power(-vv-mid_points_uv[1],2)) 
      #   n_arr += v_l*np.exp(-(r1**2 / (grid_kernel_r_pixels/pixels_per_wavelength)**2))
      #   n_arr += np.conjugate(v_l)*np.exp(-(r2**2 / (grid_kernel_r_pixels/pixels_per_wavelength)**2))
    # import matplotlib.pyplot as plt
    # plt.figure()
    # plt.imshow(n_arr.real, interpolation='none')
    # plt.savefig('uvplane.png')
    # plt.show()
    return (n_arr, xedges, yedges) #uv_plane

  def get_image(self, nw = 30, num_bin = 2**7, pax=0):

    uv_plane, xedges, yedges = self.get_uvplane(self.vis_list, num_bin, nw, use_kernel=True)
    plt.imshow(uv_plane.real, extent=[xedges[-1], xedges[0], yedges[0], yedges[-1]], interpolation='nearest')
    plt.show()
    maxang = 1./(2*(nw*2.)/num_bin)*(180./np.pi)
    # print 'maxang', maxang
    # beam_ift = np.fft.fftshift(np.fft.ifft2(np.fft.ifftshift(1-uv_plane.mask)))
    ift = np.fft.fftshift(np.fft.ifft2(np.fft.ifftshift(uv_plane)))
    
    def convert_to_polar(x, y):
      theta = np.arctan2(x, y)
      r = np.sqrt(x**2 + y**2)
      return theta, r

    grid_l, grid_m = np.mgrid[-maxang:maxang:num_bin*1j, -maxang:maxang:num_bin*1j]
    # grid_phi, grid_r = convert_to_polar(np.arcsin(grid_l*np.pi/180.),np.arcsin(grid_m*np.pi/180.))
    grid_phi, grid_r = convert_to_polar(grid_l,grid_m)
    
    absift = np.abs(ift)
    idx = np.unravel_index(absift.argmax(), absift.shape)
    print (grid_phi[idx]*180/np.pi, 90.-grid_r[idx])
    if pax!=0:
      if len(pax)==4:
        # pax[0].pcolormesh(grid_phi, grid_r*180./np.pi, np.flipud(np.fliplr(np.abs(ift.T))))
        pax[0].pcolormesh(grid_phi, grid_r, np.abs(ift))
        pax[1].imshow(np.abs(ift), extent=[maxang, -maxang, -maxang, maxang], interpolation='nearest')
        pax[2].imshow(np.abs(beam_ift), extent=[maxang, -maxang, -maxang, maxang], interpolation='nearest')
        pax[3].imshow(np.abs(uv_plane), extent=[xedges[-1], xedges[0], yedges[0], yedges[-1]], interpolation='nearest')
      else:
        return pax[1].imshow(np.abs(ift), extent=[maxang, -maxang, -maxang, maxang], interpolation='nearest')


    else:
      # plt.imshow(np.abs(ift), extent=[maxang, -maxang, -maxang, maxang], interpolation='nearest')
      plt.imshow(np.abs(ift), extent=[maxang, -maxang, -maxang, maxang], interpolation='nearest')
      plt.imshow(uv_plane.real, extent=[xedges[-1], xedges[0], yedges[0], yedges[-1]], interpolation='nearest')

