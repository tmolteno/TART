import numpy as np
import datetime
import cPickle
import math
import gzip

from tart.imaging import tart_util  #FIXME
from tart.imaging import location

from tart.operation import settings

class Observation:

  '''Antenna positions are going to be in meters from the array reference position.
  They will be in 3D ENU co-ordinates'''
  def __init__(self, timestamp, config, data=None, savedata=None):
    self.timestamp = timestamp
    self.config = config
    self.data = data
    self.savedata = savedata

  def save(self, filename):

    configdict = {}
    configdict['name'] = self.config.name
    configdict['array_orientation'] = self.config.array_orientation
    configdict['locations'] = self.config.locations
    configdict['num_antennas'] = self.config.num_antennas
    configdict['num_baselines'] = self.config.num_baselines
    configdict['geo'] = [self.config.get_lat(), self.config.get_lon(), self.config.get_alt()]
    configdict['ant_positions'] = self.config.ant_positions
    configdict['frequency'] = self.config.frequency
    configdict['bandwidth'] = self.config.bandwidth

    d = {}
    d['config'] = configdict
    d['timestamp']= self.timestamp
    
    if (self.savedata is None):
      self.savedata = np.array((self.data+1)/2,dtype=np.uint8)

    t = []
    for ant in self.savedata:
      t.append(np.packbits(ant))
    
    d['data'] = t
    
    save_ptr = gzip.open(filename, 'wb')
    cPickle.dump(d, save_ptr, cPickle.HIGHEST_PROTOCOL)
    save_ptr.close()

  def get_antenna_old(self, ant_num):
    if (ant_num > self.config.num_antennas):
      raise "Antenna %d doesn't exist" % ant_num
    mask = (1 << (ant_num-1))
    ant_data = ((self.data & mask) / mask)
    ret_data = np.array(ant_data, dtype=np.int)
    ret = ret_data*2.0 - 1.0 # Convert to bipolar binary
    return ret

  def get_antenna(self, ant_num):
    if (ant_num > self.config.num_antennas):
      raise "Antenna %d doesn't exist" % ant_num
    return self.data[ant_num-1] # Return to bipolar binary

  def get_sampling_rate(self):
    ref_freq = 16.368e6  # See the Max 2769 data sheet. We operate in one of the predefined modes
    return ref_freq

  def get_julian_date(self):
    return tart_util.get_julian_date(self.timestamp)

  def get_mjd(self):
    return tart_util.get_mjd(self.timestamp)

def Observation_Load(filename):
    try:
      load_data = gzip.open(filename, 'rb')
      d = cPickle.load(load_data)
    except:
      print 'not gzipped'
      load_data = open(filename, 'rb')
      d = cPickle.load(load_data)
    finally:
      load_data.close()
    bipolar_data = []
    for i in d['data']:
      bipolar_data.append(np.unpackbits(i)*2.-1)
    # this is an array of bipolar radio signals.
    ret = Observation(d['timestamp'], settings.from_dict(d['config']), data=bipolar_data)
    return ret
