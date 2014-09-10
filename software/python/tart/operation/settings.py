#
# Settings for the telescope. This provides an accessor object for the data stored in
# The telescope_config.json file.
#
# Tim Molteno 2013 tim@elec.ac.nz
#
import json

from tart.util import angle
from tart.imaging import location

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
      settings = json.loads(data)
      f.close()
      self.name = settings['name']
      self.array_orientation = settings['orientation']
      self.locations = settings['locations']
      self.num_antennas = settings['num_antenna']
      self.num_baselines = self.num_antennas*(self.num_antennas-1)/2
      self.geo = [settings['lat'], settings['lon'], settings['alt']]
      self.ant_positions = self.calc_antenna_ENU()
      self.frequency = settings['frequency']
      self.bandwidth = settings['bandwidth']

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

  def plot_antenna_positions(self):
    import matplotlib.pyplot as plt
    labels = ['Ant{0}'.format(i+1) for i in range(self.num_antennas)]
    ante = np.array([a[0] for a in self.ant_positions])
    antn = np.array([a[1] for a in self.ant_positions])
    plt.scatter(ante, antn, marker = 'o', s = 20.)
    # plt.xlim(-4, 4)
    # plt.ylim(-4, 4)
    plt.xlabel('East [m]')
    plt.ylabel('North [m]')

    for label, x, y in zip(labels, ante, antn):
      plt.annotate(label, xy = (x, y*1.05), fontsize=6)
    # if args.png:
    #   plt.savefig('antenna_positions.png')
    # plt.show()
