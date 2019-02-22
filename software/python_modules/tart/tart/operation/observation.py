import numpy as np
import datetime
try:
   import cPickle as pickle
except:
   import pickle
import math
import gzip

from tart.imaging import tart_util

from tart.operation import settings

def boolean_mean(x_bool_arr):
    '''Return mean of boolean array where -1 -> 0    and 1 -> 1'''
    ret = x_bool_arr.sum()/float(len(x_bool_arr))*2.-1
    return ret


class Observation(object):

    '''Antenna positions are going to be in meters from the array reference position.
    They will be in 3D ENU co-ordinates'''
    def __init__(self, timestamp, config, data=None, savedata=None):
        self.timestamp = timestamp
        self.config = config
        self.data = np.asarray(data)
        self.savedata = savedata

    def save(self, filename):

        d = {}
        d['config'] = self.config.Dict
        d['timestamp'] = self.timestamp

        if self.savedata is None:
            #self.savedata = np.array((self.data+1)/2,dtype=np.uint8)
            self.savedata = np.array(self.data, dtype=np.uint8)

        t = []
        for ant in self.savedata:
            t.append(np.packbits(ant))

        d['data'] = t

        save_ptr = gzip.open(filename, 'wb')
        pickle.dump(d, save_ptr, pickle.HIGHEST_PROTOCOL)
        save_ptr.close()

    def get_means(self):
        '''Calculate and return means of antenna data'''
        ret = []
        for i in range(self.config.get_num_antenna()):
            ret.append(boolean_mean(self.data[i]))
        return np.array(ret)

    def get_antenna(self, ant_num):
        if ant_num >= self.config.get_num_antenna():
            raise "Antenna %d doesn't exist" % ant_num
        return self.data[ant_num]*2-1. # Return to bipolar binary

    def get_sampling_rate(self):
        return self.config.get_sampling_frequency()    # See the Max 2769 data sheet. We operate in one of the predefined modes

    def get_julian_date(self):
        return tart_util.get_julian_date(self.timestamp)

    def get_mjd(self):
        return tart_util.get_mjd(self.timestamp)

def Observation_Load(filename):
    try:
        load_data = gzip.open(filename, 'rb')
        d = pickle.load(load_data)
    except:
        print('not gzipped')
        load_data = open(filename, 'rb')
        d = pickle.load(load_data)
    finally:
        load_data.close()
    bipolar_data = []
    for i in d['data']:
        unpacked_ints = np.asarray(np.unpackbits(i), dtype=np.uint8)
        bipolar_data.append(unpacked_ints)
    # this is an array of bipolar radio signals.
    ret = Observation(timestamp=d['timestamp'], config=settings.from_dict(d['config']), data=bipolar_data)
    return ret
