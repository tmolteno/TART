#
# Settings for the telescope. This provides an accessor object for the data stored in
# The telescope_config.json file.
#
# Tim Molteno 2013 tim@elec.ac.nz
#
import json
import numpy as np
from tart.util import angle
from tart.imaging import location
import matplotlib.pyplot as plt

'''Antenna positions are in 3D ENU coordinates in meters'''
def rotate_location(array_orientation, localcoord):
  array_orientation = angle.from_dms(array_orientation)
  c = array_orientation.cos()
  s = array_orientation.sin()
  e = localcoord[0]*c - localcoord[1]*s
  n = localcoord[0]*s + localcoord[1]*c
  u = localcoord[2]
  return [e, n, u]

def from_dict(configdict):
  ret = Settings()
  ret.name = configdict['name']
  ret.array_orientation = configdict['array_orientation']
  ret.locations = configdict['locations']
  ret.num_antennas = configdict['num_antennas']
  ret.num_baselines = configdict['num_baselines']
  ret.geo = configdict['geo']
  ret.ant_positions = configdict['ant_positions']
  ret.frequency  = configdict['frequency']
  ret.bandwidth  = configdict['bandwidth']
  return ret

class Settings:

  def __init__(self, filename = 0):
    if filename != 0:
      f = open(filename, 'r')
      data = f.read()
      f.close()
      self.from_json(data)

  def from_json(self, data):
    settings = json.loads(data)
    self.name = settings['name']
    self.array_orientation = settings['orientation']
    self.locations = np.array(settings['locations'])
    self.num_antennas = settings['num_antenna']
    self.num_baselines = self.num_antennas*(self.num_antennas-1)/2
    self.geo = [settings['lat'], settings['lon'], settings['alt']]
    self.ant_positions = self.calc_antenna_ENU()
    self.frequency = settings['frequency']
    self.bandwidth = settings['bandwidth']

  def to_json(self):
    configdict = {}
    configdict['name'] = self.name
    configdict['orientation'] = self.array_orientation
    configdict['locations'] = self.locations.tolist()
    configdict['num_antenna'] = self.num_antennas
    # configdict['num_baselines'] = self.num_baselines
    configdict['lat'] = self.geo[0]
    configdict['lon'] = self.geo[1]
    configdict['alt'] = self.geo[2]
    # configdict['ant_positions'] = self.ant_positions
    configdict['frequency'] = self.frequency
    configdict['bandwidth'] = self.bandwidth
    json_str = json.dumps(configdict)
    return json_str

  def save(self, fname):
    f=open(fname,"w")
    f.write(self.to_json())
    f.close()

  def set_geo(self, lat, lon, alt):
    self.geo[0] = lat
    self.geo[1] = lon
    self.geo[2] = alt

  def get_lat(self):
    return self.geo[0]

  def get_lon(self):
    return self.geo[1]

  def get_alt(self):
    return self.geo[2]

  def get_loc(self):
    return location.Location(angle.from_dms(self.get_lat()), angle.from_dms(self.get_lon()), self.get_alt())

  def calc_antenna_ENU(self):
    ant_positions = []
    for x in self.locations:
      ant_positions.append(rotate_location(-self.array_orientation, x))
    return ant_positions

  def plot_antenna_positions(self, c='blue', label=''):
    labels = ['Ant{0}'.format(i) for i in range(self.num_antennas)]
    ante = np.array([a[0] for a in self.ant_positions])
    antn = np.array([a[1] for a in self.ant_positions])
    plt.scatter(ante, antn, marker = 'o', s = 50., color=c, label=label)
    plt.xlim(-2,2)
    plt.ylim(-2,2)
    plt.xlabel('East [m]')
    plt.ylabel('North [m]')
    plt.tight_layout()
    for label, x, y in zip(labels, ante, antn):
      plt.annotate(label, xy = (x+0.02, y-0.03), fontsize=20)
    # if args.png:
    #   plt.savefig('antenna_positions.png')
    # plt.show()
