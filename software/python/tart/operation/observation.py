import numpy as np
import datetime
import cPickle
import math

from tart.imaging import tart_util  #FIXME
from tart.imaging import location

from tart.operation import settings

# Antenna numbers are 1,2,3......

def str2bits(st):
  length = len(st)
  ret = np.zeros(length, dtype='uint8')
  for i, el in enumerate(st):
    ret[i] = int(el)
  return ret, length

def bit2int(bits):
  return np.array([int(bits2str(bits[i:i+8]),2) for i in range(0,len(bits),8)])

def bits2str(arr):
  ret = ''
  for i in arr:
    ret += str(int(i))
  return ret

def int2bin_str_with_n_leading_zeros(n, i):
  return ('{0:0%db}'%(n)).format(i)

def int2bit(ints, length=0):
  if length==0:
    length = len(ints)*8
  ret = np.zeros(length, dtype='uint8')
  for i, el in enumerate(ints):
    if i == len(ints)-1:
      for j, val in enumerate(int2bin_str_with_n_leading_zeros(length-i*8, el)):
        ret[i*8+j] = int(val)
    else:
      for j, val in enumerate('{0:08b}'.format(el)):
        ret[i*8+j] = int(val)
  return np.array(ret)

class Observation:

  '''Antenna positions are going to be in meters from the array reference position.
  They will be in 3D ENU co-ordinates'''
  def __init__(self, timestamp, data, config):
    self.timestamp = timestamp
    self.data = data # array of bipolar arrays [[-1,1...],[1,-1...],...] with each length 2**16-1
    self.config = config

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
    d['len'] = len(self.data[0])
    t = []
    for i in self.data:
      t.append(np.array(bit2int((i+1.)/2.),dtype='uint8'))
    d['data'] = np.array(t, dtype='uint8')

    save_data = open(filename, 'wb')
    cPickle.dump(d, save_data, cPickle.HIGHEST_PROTOCOL)
    save_data.close()

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
      bipolar_data.append(int2bit(i, d['len'])*2.-1)
    # this is an array of bipolar radio signals.
    ret = Observation(d['timestamp'], np.array(bipolar_data, dtype='float64'), settings.from_dict(d['config']))
    return ret
