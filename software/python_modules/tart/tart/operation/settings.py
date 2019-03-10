#
# Settings for the telescope. This provides an accessor object for the data stored in
# The telescope_config.json file.
#
# Tim Molteno 2013-2017 tim@elec.ac.nz
# Max Scheel 2017 max@max.ac.nz : do no derive any prober
import json
import numpy as np
from tart.util import angle
from tart.imaging import location

'''Antenna positions are in 3D ENU coordinates in meters'''
def rotate_location(array_orientation, localcoord):
    array_orientation = angle.from_dms(array_orientation)
    _e, _n, _u = localcoord
    c = array_orientation.cos()
    s = array_orientation.sin()
    e = _e*c - _n*s
    n = _e*s + _n*c
    u = _u
    return [e, n, u]

def from_dict(configdict):
    ret = Settings()
    ret.Dict = configdict
    return ret

def from_file(filename):
    f = open(filename, 'r')
    ret = f.read()
    f.close()
    return from_json(ret)

def from_json(data):
    dic = json.loads(data)
    ret = from_dict(dic)
    return ret

def from_api_json(config_json, ant_pos_json):
    ret = Settings()
    ret.Dict['num_antenna'] = config_json['num_antenna']
    ret.Dict['sampling_frequency'] = config_json['sampling_frequency']
    ret.Dict['frequency'] = config_json['operating_frequency']
    ret.Dict['bandwidth'] = config_json['bandwidth']
    ret.set_antenna_positions(ant_pos_json)
    loc = config_json['location']
    ret.set_geo(loc['lat'], loc['lon'], loc['alt'])
    return ret

class Settings:
    def __init__(self):
        self.Dict = {}

    def set_antenna_positions(self, ant_pos):
        self.Dict['antenna_positions'] = {'calibrated':ant_pos}
        assert (self.Dict['num_antenna'] == len(ant_pos))

    def load_antenna_positions(self,
                cal_ant_positions_file='calibrated_antenna_positions.json',
                design_antenna_positions_file = None):
        ant_pos = {}
        try:
            with open(cal_ant_positions_file, 'r') as fr:
                ant_pos['calibrated'] = json.loads(fr.read())
                fr.close()
        except:
                print('could not load ' + cal_ant_positions_file)
        try:
            if design_antenna_positions_file is not None:
                with open(design_antenna_positions_file, 'r') as fr:
                    ant_pos['design'] = json.loads(fr.read())
                    fr.close()
        except:
            print('could not load ' + design_antenna_positions_file)
        self.set_antenna_positions(ant_pos['calibrated'])

    def get_antenna_positions(self, key='calibrated'):
        if 'antenna_positions' in self.Dict:
            if key in self.Dict['antenna_positions']:
                return self.Dict['antenna_positions'][key]

    def to_json(self):
        from copy import deepcopy
        configdict = deepcopy(self.Dict)
        if 'antenna_positions' in configdict:
            configdict.pop('antenna_positions')
        json_str = json.dumps(configdict)
        return json_str

    def save(self, fname):
        f=open(fname, "w")
        f.write(self.to_json())
        f.close()

    def set_geo(self, lat, lon, alt):
        self.Dict['lat'] = lat
        self.Dict['lon'] = lon
        self.Dict['alt'] = alt

    def get_loc(self):
        return location.Location(angle.from_dms(self.Dict['lat']),
                                 angle.from_dms(self.Dict['lon']),
                                 self.Dict['alt'])

    def get_lat(self):
        return self.Dict['lat']

    def get_lon(self):
        return self.Dict['lon']

    def get_alt(self):
        return self.Dict['alt']

    def get_sampling_frequency(self):
        return self.Dict['sampling_frequency']

    def get_operating_frequency(self):
        return self.Dict['frequency']

    def get_bandwidth(self):
        return self.Dict['bandwidth']

    def get_name(self):
        return self.Dict['name']

    def get_num_antenna(self):
        return self.Dict['num_antenna']

    def calc_antenna_ENU(self, antenna_pos):
        ant_positions = []
        for x in antenna_pos:
            ant_positions.append(rotate_location(-self.Dict['array_orientation'], x))
        return ant_positions

    def plot_antenna_positions(self, c='blue', label=''):
        import matplotlib.pyplot as plt
        num_antennas = self.Dict['num_antenna']
        for key in self.Dict['antenna_positions']:
            ant_pos = self.Dict['antenna_positions'][key]
            labels = ['{0}'.format(i) for i in range(num_antennas)]
            ante = np.array([a[0] for a in ant_pos])
            antn = np.array([a[1] for a in ant_pos])
            plt.scatter(ante, antn, marker='o', s = 50.0, color=c, label=label)
            plt.xlim(-1.5,1.5)
            plt.ylim(-1.5,1.5)
            plt.xlabel('East [m]')
            plt.ylabel('North [m]')
            plt.tight_layout()
            for label, x, y in zip(labels, ante, antn):
                plt.annotate(label, xy=(x+0.02, y-0.03), fontsize=15)
