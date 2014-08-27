import numpy as np
import datetime
import cPickle
import math

from tart.imaging import tart_util  #FIXME
from tart.imaging import location

from tart.operation import settings

# Antenna numbers are 1,2,3......

def int2bit(array_ints):
  ret = np.zeros(8*len(array_ints),dtype=np.uint8)
  for i in range(8):
    ret[i::8] = (array_ints << i)>0
  print ret
  return ret

def bit2int(array_bits):
  # numpy array as input! dtype=np.uint8
  arr_len = len(array_bits)
  if (arr_len%8) != 0:
    array_bits = array_bits[:-(arr_len%8)]
    print 'length must be a multiple of 8. Input array got cropped'

  ret = (array_bits[0::8] << 7) + \
        (array_bits[1::8] << 6) + \
        (array_bits[2::8] << 5) + \
        (array_bits[3::8] << 4) + \
        (array_bits[4::8] << 3) + \
        (array_bits[5::8] << 2) + \
        (array_bits[6::8] << 1) + \
        (array_bits[7::8])
  ret = ret.astype(np.uint8)
  print ret
  return ret

#TEST BENCH:

# int_input = np.array([240,240,0,240], dtype=np.uint8)
# bit_output = int2bit(int_input)
# int_output = bit2int(bit_output)

# print int_input
# print bit_output
# print int_output

# print (int_input == int_output).all()

# bit_input = np.array([1, 1, 1, 1, 0, 0, 0, 0], dtype=np.uint8)
# int_output = bit2int(bit_input)
# bit_output = int2bit(int_output)


# print bit_input
# print int_output
# print bit_output

# print (bit_input == bit_output).all()

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
    for i,ant in enumerate(self.savedata):
      ints = bit2int(ant)
      print 'antenna', i, ints
      t.append(ints)
    
    d['data'] = t
    
    save_ptr = open(filename, 'wb')
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
    load_data = open(filename, 'rb')
    d = cPickle.load(load_data)
    load_data.close()
    bipolar_data = []
    for i in d['data']:
      bipolar_data.append(int2bit(i)*2.-1)
    # this is an array of bipolar radio signals.
    ret = Observation(d['timestamp'], settings.from_dict(d['config']), data=bipolar_data)
    return ret
