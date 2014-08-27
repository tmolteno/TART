"""Copyright (C) Max Scheel 2013. All rights reserved"""

from tart.imaging import sun
from tart.imaging import radio_source
from tart.imaging import gps_satellite
from tart.imaging import location

from tart.util import angle

from tart.simulation import simulation_source


import numpy as np

class Skymodel(object):
  """Ensemble of sources and their visibilities"""
  def __init__(self, n_sources, sun_str=2.e5, sat_str=5.01e6, gps=True, thesun=False, known_cosmic=True):
    self.n_sources = n_sources
    self.source_list = []
    self.known_objects = []
    self.gps_ants = []
    if thesun:
      self.add_src(sun.Sun(jy=sun_str))
    if gps:
      for i in range(32):
        self.add_src(gps_satellite.GpsSatellite(i+1, location.Dunedin, jy=sat_str))
    if known_cosmic:
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
    return state_vector

  def true_image(self, settings, utc_date):
    '''Plot current sky.'''
    import healpy as hp
    import matplotlib.pyplot as plt

    l_el = []
    l_az = []
    l_name = []

    for src in self.known_objects:
      el, az = src.to_horizontal(settings.get_loc(), utc_date)
      l_el.append(el.to_rad())
      l_az.append(az.to_rad())
      l_name.append('%s %1.1e' % (str(src), src.jansky(utc_date)))

    nside = np.power(2, 5)
    m = np.zeros(hp.nside2npix(nside))*hp.UNSEEN
    th = -np.array(l_el) + np.pi/2.
    l_az = np.array(l_az)
    pix = hp.pixelfunc.ang2pix(nside, th, l_az)
    m[pix] = [src.jansky(utc_date) for src in self.known_objects]
    plt.figure()
    hp.mollview(m)
    _ = [hp.projtext(i, j, n, lonlat=False) for i, j, n in zip(th, l_az, l_name)]
    plt.show()
    plt.figure()
    plt.plot(l_el)
    plt.show()

def from_state_vector(state_vector):
  '''Generate skymodel from state vector'''
  psky = Skymodel(0)
  psky.n_sources = len(state_vector)/4
  psky.source_list = []

  for i in range(psky.n_sources):
    ra = angle.from_dms(state_vector[i+(0*psky.n_sources)])
    dec = angle.from_dms(state_vector[i+(1*psky.n_sources)])
    gs = radio_source.CosmicSource(ra, dec, jy=state_vector[i+(2*psky.n_sources)], width=state_vector[i+(3*psky.n_sources)])
    psky.source_list.append(gs)
  return psky
