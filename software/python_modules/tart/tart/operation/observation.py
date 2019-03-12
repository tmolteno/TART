import numpy as np
import datetime

import os

try:
   import cPickle as pickle
except:
   import pickle
import math
import gzip
import h5py
import dateutil.parser

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
        '''
            Create an observation from binary (boolean 0...1) data
            
        '''
        self.timestamp = timestamp
        self.config = config
        self.data = np.asarray(data)
        self.savedata = savedata

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

    def save(self, filename):
        ''' Save the Observation object,
            Data is saved as one bit
        '''
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


    def to_hdf5(self, filename):
        ''' Save the Observation object,
            in a portable HDF5 format
            
            obs = observation.Observation(t_stmp, config, savedata=ant_data)
            obs.to_hdf5(filename)

        '''
        with h5py.File(filename, "w") as h5f:
            dt = h5py.special_dtype(vlen=bytes)
            
            conf_dset = h5f.create_dataset('config', (1,), dtype=dt)
            conf_dset[0] = self.config.to_json()

            ts_dset = h5f.create_dataset('timestamp', (1,), dtype=dt)
            ts_dset[0] = self.timestamp.isoformat()

            if self.savedata is None:
                #self.savedata = np.array((self.data+1)/2,dtype=np.uint8)
                self.savedata = np.array(self.data, dtype=np.uint8)

            t = []
            for ant in self.savedata:
                t.append(np.packbits(ant))

            h5f.create_dataset('data', data=t)

    @classmethod
    def from_hdf5(self, filename):
        ''' Load the Observation object,
            in a portable HDF5 format
        '''
        with h5py.File(filename, "r") as h5f:
            config_json = np.string_(h5f['config'][0])
            config = settings.from_json(config_json)
            timestamp = dateutil.parser.parse(h5f['timestamp'][0])
            
            hdf_data = h5f['data'][:]
            unipolar_data = []
            for i in hdf_data:
                unpacked_ints = np.asarray(np.unpackbits(i), dtype=np.uint8)
                unipolar_data.append(unpacked_ints)
                # this is an array of unipolar 0,1 radio signals.

        ret = Observation(timestamp=timestamp, config=config, data=unipolar_data)
        return ret

def Observation_Load(filename):
    _, file_extension = os.path.splitext(filename)

    if ('.pkl' == file_extension):
        try:
            load_data = gzip.open(filename, 'rb')
            d = pickle.load(load_data)
        except:
            print('not gzipped')
            load_data = open(filename, 'rb')
            d = pickle.load(load_data)
        finally:
            load_data.close()

        unipolar_data = []
        for i in d['data']:
            unpacked_ints = np.asarray(np.unpackbits(i), dtype=np.uint8)
            unipolar_data.append(unpacked_ints)
            # this is an array of unipolar 0,1 radio signals.

        return Observation(timestamp=d['timestamp'], config=settings.from_dict(d['config']), data=unipolar_data)
    if ('.hdf' == file_extension):
        return Observation.from_hdf5(filename)
    
    raise RuntimeError("Unknown file extension {}".format(file_extension))
