import numpy as np
import json

try:
   import cPickle as pickle
except:
   import pickle

from tart.util import angle
from tart.util import skyloc
from tart.util import constants

from tart.operation import observation

class Visibility:
    """
    A container class for visibilities from a single observation.
    """
    def __init__(self, obs, phase_el, phase_az):
        self.phase_el = phase_el
        self.phase_az = phase_az
        self.config = obs.config
        self.timestamp = obs.timestamp

    def __init__(self, config, timestamp):
        self.phase_el = angle.from_dms(90.)
        self.phase_az = angle.from_dms(0.)
        self.config = config
        self.timestamp = timestamp

    def set_visibilities(self, v, b):
        self.baselines = b
        self.v = v

    '''Rotated one, aimed at ra, decl

    Justification:

    Let
        s_1(t) = N(t) e^{j \omega t}

    t_g^{01) = t_g^1 - t_g^0 (arrival at a1 - arrival at a0)
    if t_g^{01} < 0, then s_0 arrives later than s_1 (defined by antennas.get_geo_delay_horizontal)
    therefore:

        s_0(t) = s_1(t - t_g^{01})

    v(0,1) = <s_0(t) s_1^{*}(t)>
                 = <s_1(t - t_g^{01}) s_1^{*}(t)>
                 = <N(t) e^{j \omega t} e^{-j \omega t_g^{01}} N^*(t) e^{-j \omega t}>
                 = e^{-j \omega t_g^{01}} <N(t) N^*(t)>

    So, after rotation

     <N(t) N^*(t)> = v(0,1) e^{+j \omega t_g^{01}}
    '''
    def rotate(self, sloc):
        from tart.simulation import antennas
        stopped_vis = []
        omega = self.config.get_operating_frequency()*2.0*np.pi
        # Now we must do fringe stopping
        loc = self.config.get_loc()
        el, az = loc.equatorial_to_horizontal(self.timestamp, sloc.ra, sloc.dec)

        ant_pos = self.config.get_antenna_positions()
        for v, b in zip(self.v, self.baselines):
            a0 = antennas.Antenna(loc, ant_pos[b[0]])
            a1 = antennas.Antenna(loc, ant_pos[b[1]])

            tg = antennas.get_geo_delay_horizontal(a0, a1, sloc)
            # tg is t_a1 - t_a0
            # (negative if a1 is closer to source than a0)

            # print(b, omega*tg)
            v = v * np.exp(-1.0j * omega * tg)
            stopped_vis.append(v)

        self.phase_el = el
        self.phase_az = az
        self.v = stopped_vis

    def vis(self, i, j):
        if (j == i):
            raise "Baseline [%d,%d] is invalid" % (i,j)
        if (j < i): # The first index should be before the second
            return np.conjugate(self.vis(j,i))
        for k, b in enumerate(self.baselines):
            if (b == [i,j]):
                return self.v[k]
        raise "Baseline [%d,%d] is invalid" % (i,j)

    def get_closure_phase(self, i, j, k):
        return np.angle(self.vis(i,j)) + np.angle(self.vis(j,k)) - np.angle(self.vis(i,k))


    def toString(self):
        ret = ""
        for i,b in enumerate(self.baselines):
            ret += " V(%s)=%g, I%g" % (str(b), np.abs(self.v[i]), np.angle(self.v[i]))
        return ret

def Visibility_From_Conf(config, timestamp, phase_el, phase_az):
    obs = observation.Observation(timestamp=timestamp, config=config)
    vis = Visibility(obs, phase_el, phase_az)
    return vis

def Visibility_Save(vis, filename):
    save_data = open(filename, 'wb')
    pickle.dump(vis, save_data, pickle.HIGHEST_PROTOCOL)
    save_data.close()


def Visibility_Save_JSON(vis, filename):
    json_data = {}
    json_data['timestamp'] = vis.timestamp
    json_data['phase_el'] = vis.phase_el.to_degrees()
    json_data['phase_az'] = vis.phase_az.to_degrees()
    json_data['config'] = vis.config.Dict
    json_data['baselines'] = vis.baselines
    json_data['vis'] = vis.v
    with open(filename, 'w') as outfile:
        # The default=str handles datetime objects as strings
        json.dump(json_data, outfile, default=str, \
            sort_keys=True, indent=4, separators=(',', ': '))

def Visibility_Load(filename):
        load_data = open(filename, 'rb')
        vis_list = pickle.load(load_data, encoding='latin1')
        load_data.close()
        err_count = 0
        ret = []
        for v in vis_list:
            if type(v) is tuple:
                err_count += 1
            else:
                ret.append(v)
        if err_count>0:
            print('Warning. Visibility file: %s had %i visibilities missing' % (filename, err_count))
        return ret

def Visibility_Lsq(vis1, vis2):
    """ Return least square based on the phases of 2 visibilities """
    if vis1.config.get_num_antenna() == vis2.config.get_num_antenna():
        difflist = []
        for v1, v2 in zip(vis1.v, vis2.v):
            diff = np.abs(np.angle(v1) - np.angle(v2))
            if diff > np.pi:
                diff = 2.*np.pi-diff
            difflist.append(diff)
        diffarr = np.array(difflist)
        return np.power(diffarr,2).sum()

