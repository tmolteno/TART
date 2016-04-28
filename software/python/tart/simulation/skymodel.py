"""Copyright (C) Max Scheel 2013. All rights reserved"""

from tart.util import angle

from tart.imaging import sun
from tart.imaging import radio_source
from tart.imaging import gps_satellite

from tart.simulation import simulation_source

import numpy as np
import healpy as hp
import matplotlib.pyplot as plt

class Skymodel(object):
  """Ensemble of sources and their visibilities"""
  def __init__(self, n_sources, location, sun_str=2.e5, sat_str=5.01e6, gps=True, thesun=False, known_cosmic=True):
    self.n_sources = n_sources
    self.source_list = []
    self.known_objects = []
    self.gps_ants = []
    self.location = location

    self.sun_str=sun_str
    self.sat_str=sat_str
    self.gps = gps
    self.thesun = thesun
    self.known_cosmic= known_cosmic

    if self.thesun:
      self.add_src(sun.Sun(jy=sun_str))
    if self.gps:
      for i in range(32):
        sv = gps_satellite.GpsSatellite(i+1, location=self.location,jy=sat_str)
        self.add_src(sv)
    if self.known_cosmic:
      for src in radio_source.BrightSources:
        self.add_src(src)
    for _ in range(self.n_sources):
      ra = angle.from_dms(np.random.uniform(0., 360.))
      dec = angle.asin(np.random.uniform(-1., 1))
      cs = radio_source.CosmicSource(ra, dec)
      self.add_src(cs)

  def gen_beam(self, utc_date_init, utc_date_obs, config, radio,  az_deg = 20., el_deg = 80.):
    ''' Generate point source with constant at RA and DEC according to given az and el at time utc_date_init'''
    sources = []
    ra, dec = config.get_loc().horizontal_to_equatorial(utc_date_init, angle.from_dms(el_deg), angle.from_dms(az_deg))
    src = radio_source.CosmicSource(ra, dec)
    el, az = src.to_horizontal(config.get_loc(), utc_date_obs)
    sources.append(simulation_source.SimulationSource(amplitude = 1., azimuth = az, elevation = el, \
      sample_duration = radio.sample_duration))
    return sources

  def get_cum_src_flux(self, utc_date):
    '''Return cumulative flux as list over all sources'''
    cumulativ_src_flux = []
    for k in range(len(self.known_objects)+1):
      cumulativ_src_flux.append(np.array([src.jansky(utc_date) for src in self.known_objects[:k] ]).sum())
    return cumulativ_src_flux

  def get_int_src_flux(self, utc_date):
    '''Return cumulative flux'''
    return self.get_cum_src_flux(utc_date)[-1]

  def gen_photons_per_src(self, utc_date, radio, config, n_samp=1):
    ''' Generate n_samp photons per source'''
    sources = []
    for src in self.known_objects:
      ra, declination = src.radec(utc_date)
      dx, dy = np.random.multivariate_normal([0., 0.], np.identity(2)*np.power(src.width, 2.), n_samp).T

      for j in range(n_samp):
        el, az = config.get_loc().equatorial_to_horizontal(utc_date, \
          ra + angle.from_dms(dx[j]), declination + angle.from_dms(dy[j]))
        sources.append(simulation_source.SimulationSource(\
          amplitude = src.jansky(utc_date)/self.get_int_src_flux(utc_date)*1./n_samp, \
          azimuth = az, elevation = el, sample_duration = radio.sample_duration))
    return sources


  def gen_n_photons(self, config, utc_date, radio, n=10):
    ''' Generate a total of n photons. Sources with more jansky will contribute more photons'''
    cumulativ_src_flux = self.get_cum_src_flux(utc_date)
    int_src_flux = cumulativ_src_flux[-1]
    rel_noise_flux = 0.0
    tot_flux = int_src_flux * (1. + rel_noise_flux)
    src_identifier = np.random.uniform(0., tot_flux, n)
    hi, _ = np.histogram(src_identifier, bins=cumulativ_src_flux)
    ret = []
    for src_num, count in enumerate(hi):
      src = self.known_objects[src_num]
      # Assume the sky is flat.
      # this will also cause problems at problems at boundaries of dec.
      dx, dy = np.random.multivariate_normal([0., 0.], np.identity(2)*np.power(src.width, 2.), count).T
      ra, declination = src.radec(utc_date)
      for j in range(count):
        el, az = config.get_loc().equatorial_to_horizontal(utc_date, ra\
          + angle.from_dms(dx[j]), declination + angle.from_dms(dy[j]))
        ret.append(simulation_source.SimulationSource(amplitude = 1./n, \
            azimuth = az, elevation = el, sample_duration = radio.sample_duration))
    return ret

  def add_src(self, src):
    '''Add source to known_objects'''
    self.known_objects.append(src)

  def get_state_vector(self):
    '''Return state vector'''
    state_vector = np.zeros(self.n_sources*4)
    for i, source in enumerate(self.source_list):
      state_vector[i+(0*self.n_sources)] = source.skyloc.ra.to_degrees()
      if source.skyloc.dec.to_degrees() > 90.:
        state_vector[i+(1*self.n_sources)] = source.skyloc.dec.to_degrees() - 360.
      else:
        state_vector[i+(1*self.n_sources)] = source.skyloc.dec.to_degrees()
      state_vector[i+(2*self.n_sources)] = source.flux
      state_vector[i+(3*self.n_sources)] = source.width
    return [self.sun_str, self.sat_str, self.gps, self.thesun, self.known_cosmic, self.location, state_vector]

  def get_src_positions(self, location, utc_date):
    ''' Save way of getting lists of el and az for all known_objects'''
    l_el = []
    l_az = []
    l_name = []
    for src in self.known_objects:
      try:
        el, az = src.to_horizontal(location, utc_date)
        if (el.to_rad()>0):
          l_el.append(el.to_rad())
          l_az.append(az.to_rad())
          l_name.append(str(src))
          # l_name.append('%s %2.2f %1.1e' % (str(src), el.to_degrees(), src.jansky(utc_date)))
      except:
        print 'no position', src
    return l_el, l_az, l_name

  def true_image(self, location, utc_date, nside):
    '''Plot current sky.'''

    l_el, l_az, l_name = self.get_src_positions(location, utc_date)

    m = np.zeros(hp.nside2npix(nside))*hp.UNSEEN
    th = -np.array(l_el) + np.pi/2.
    l_phi = -np.array(l_az)
    pix = hp.pixelfunc.ang2pix(nside, th, l_phi)
    m[pix] = 1 #[src.jansky(utc_date) for src in self.known_objects]
    m = hp.sphtfunc.smoothing(m,fwhm=3.*np.pi/180.)
    # plt.figure()
    # hp.visufunc.orthview(m, title="", rot=(0,90,0), )
    return m


  def true_image_overlay(self, location, utc_date):
    '''Plot current sky.
    Theta is colatitude and measured from North pole. 0 .. pi
     (straight up) |  el:  pi/2   | theta 0
     (horizon)     |  el:  0      | theta pi/2
    Th = pi/2 - el

    flip : {'astro', 'geo''}, optional
    Defines the convention of projection : 'astro'' (default, east towards left, west towards right) or 'geo' (east towards right, west towards left)
    '''

    l_el, l_az, l_name = self.get_src_positions(location, utc_date)

    th = np.pi/2. - np.array(l_el)
    l_phi = -np.array(l_az)
    _ = [hp.projscatter(i, j, rot=(0,90,0)) for i, j, n in zip(th, l_phi, l_name)]
    _ = [hp.projtext(i, j, n, rot=(0,90,0)) for i, j, n in zip(th, l_phi, l_name)]

    hp.projtext(np.pi/2, 0., 'N', rot=(0,90,0))
    hp.projtext(np.pi/2, -np.pi/2., 'E', rot=(0,90,0))
    hp.projtext(np.pi/2, -np.pi, 'S', rot=(0,90,0))
    hp.projtext(np.pi/2, -np.pi*3./2., 'W', rot=(0,90,0))

def from_state_vector(state):
  '''Generate skymodel from state vector'''
  sun_str, sat_str, gps, thesun, known_cosmic, location, state_vector = state
  psky = Skymodel(0, location=location, sun_str=sun_str, sat_str=sat_str,gps=gps,known_cosmic=known_cosmic)
  psky.n_sources = len(state_vector)/4
  psky.source_list = []

  for i in range(psky.n_sources):
    ra = angle.from_dms(state_vector[i+(0*psky.n_sources)])
    dec = angle.from_dms(state_vector[i+(1*psky.n_sources)])
    gs = radio_source.CosmicSource(ra, dec, jy=state_vector[i+(2*psky.n_sources)], width=state_vector[i+(3*psky.n_sources)])
    psky.source_list.append(gs)
  return psky
