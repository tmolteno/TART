'''
RadioSource Radio Source

Some bright cosmic radio sources
'''
#
# Copyright (c) Tim Molteno 2013. tim@elec.ac.nz
#
import datetime

from tart.util import angle
from tart.imaging import location
from tart.util import skyloc

import numpy as np


class RadioSource:
  '''This is the base class for all possible radio sources'''
  def __init__(self, jy=1000.0, width=0.01):
    self.flux = jy
    self.width = width

  def radec(self, utc_date): # Get the RA and Declination
    raise "Use a class derived from RadioSource"
  
  def jansky(self, utc_date):
    ''' Spectral flux density S. This is the spectral radiance B(theta, phi) integrated over the solid angle of the source. '''
    return self.flux

  def to_horizontal(self, location, utc_date):
    RA, declination = self.radec(utc_date)
    el, az = location.equatorial_to_horizontal(utc_date, RA, declination)
    return el, az


class CosmicSource(RadioSource):
  '''
  A cosmic radio source at fixed equatorial coordinates.
  '''
  
  def __init__(self, ra, dec, jy=1000.0, width=0.001):
    RadioSource.__init__(self, jy, width)
    self.skyloc = skyloc.Skyloc(ra, dec)

  def __repr__(self):
    return 'COSMIC'+str(self.skyloc)

    
  def radec(self, utc_date): # Get the RA and Declination
    return self.skyloc.ra, self.skyloc.dec
  
C3_461 = CosmicSource(angle.from_hours(23,23,24), angle.from_dms(58,48,54), 2477.0) # SNR-Cassiopeia A
CTA_59 = CosmicSource(angle.from_hours(13,22,28), angle.from_dms(-42,46,0), 2010.0) # Cent A NGC5128
CTB_42 = CosmicSource(angle.from_hours(17,42,9), angle.from_dms(-28,50,0), 1800.0) # Sag A Galactic Nucleus
C3_405 = CosmicSource(angle.from_hours(19,59,28), angle.from_dms(40,44,2), 1495.0) # D Galaxy - Cygnus A
C3_144 = CosmicSource(angle.from_hours(5,34,32), angle.from_dms(22,0,52), 875.0) # SNR Crab Nebula

'''Convenient list of bright sources'''
BrightSources = [C3_461, CTA_59, CTB_42, C3_405, C3_144]

# Use data from the NED database of objects http://ned.ipac.caltech.edu/
#class NEDRadioSources:
  
  #def __init__

if __name__ == '__main__':
  date = datetime.datetime.utcnow()
  # Do some testing
  print "Cassiopeia A ", 
  print C3_461.to_horizontal(location.Dunedin, date)

  print "Centaurus A ", 
  print CTA_59.to_horizontal(location.Dunedin, date)
  print CTA_59.radec(date)

  print "Saggitarius A ", 
  print CTB_42.to_horizontal(location.Dunedin, date)

  print "Cygnus A ", 
  print C3_405.to_horizontal(location.Dunedin, date)

  print "Crab Nebula SNR ", 
  print C3_144.to_horizontal(location.Dunedin, date)

