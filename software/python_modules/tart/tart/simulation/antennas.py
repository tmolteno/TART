"""Models of Antennas"""

import numpy as np
from tart.util import angle
from tart.util import constants
from tart.operation import observation
from tart.imaging import location
from tart.imaging import visibility
from tart.simulation.simulation_source import HorizontalSource


def antennas_signal(antennas, ant_models, sources, timebase):
    """ TODO think about introducing uncorrelated system noise at each antenna."""
    ret = np.zeros((len(antennas), len(timebase)))
    # An array to hold the signal seen by each antenna (in rows)
    # One row for each antenna

    for i, ant in enumerate(antennas):
        # A vector to hold the working value as signals are added
        working = np.zeros(len(timebase))
        for src in sources:
            delta = ant.get_geo_delay_horizontal(src)
            # print("Delay %s: %g" % (ant, dt))
            gain = ant_models[i].get_gain(src.elevation, src.azimuth)
            # print('Antenna: %i Gain: %1.1f el: %1.1f az: %1.1f' % (i, gain, src.elevation.to_degrees(), src.azimuth.to_degrees()))
            working += src.s(timebase + delta) * gain
        ret[i, :] = working
    return ret


def antennas_simplified_signal(antennas, ant_models, sources, timebase, fc0, seed=None):
    """ Retun array of simplified (at baseband) antenna signals ."""
    np.random.seed(seed=seed)
    debug = False
    # create an array to hold the signal seen by each antenna (in rows)
    ant_sigs = []

    for i, ant in enumerate(antennas):
        s_bb = np.zeros(len(timebase), dtype=complex)
        for src in sources:  # Cycle through each signal source in turn
            gain = ant_models[i].get_gain(src.elevation, src.azimuth)
            if gain > 0.0:  # Gain theshold
                dt = ant.get_geo_delay_horizontal(src)
                s_bb += (
                    src.s_baseband(timebase + dt) * gain * np.exp(1j * src.omega * dt)
                )
        ant_sigs.append(s_bb)
    return np.asarray(ant_sigs)  # signal in baseband


def antennas_simp_vis(antennas, ant_models, sources, utc_date, config, noise_lvl):
    """ Return visibility object without generating timeseries or filtering."""
    vis = []
    baselines = []
    num_ant = len(antennas)
    # noise = np.random.uniform(0.,np.sqrt(noise_lvl),config.get_num_antenna()) * np.exp(2.0j*np.pi*np.random.uniform(-1.,1.,config.get_num_antenna()))
    if noise_lvl.__gt__(0.0).all():
        noise = np.random.normal(0.0, noise_lvl) * np.exp(
            2.0j * np.pi * np.random.uniform(-1.0, 1.0, num_ant)
        )
    else:
        noise = np.zeros(num_ant)
    for i in range(0, num_ant):
        for j in range(i + 1, num_ant):
            vi = noise[i] + noise[j]
            # print(vi)
            for src in sources:
                gain0 = ant_models[i].get_gain(src.elevation, src.azimuth)
                gain1 = ant_models[j].get_gain(src.elevation, src.azimuth)
                if gain0 <= 0.0 or gain1 <= 0.0:
                    vi += 0.0j
                else:
                    dt = get_geo_delay_horizontal(antennas[i], antennas[j], src)
                    vi += (
                        gain0
                        * gain1
                        * 1.0
                        * np.exp(1.0j * dt * constants.L1_OMEGA)
                        * src.amplitude
                    )
            if np.abs(vi) >= 1.0:
                # otherwise in case we have a signal that is almost 1. noise could cause an overflow
                vi = 1.0 * vi / np.abs(vi)
            vis.append(vi)
            baselines.append([i, j])
    vis_o = visibility.Visibility(config, utc_date)
    vis_o.set_visibilities(vis, baselines)
    return vis_o


class Antenna(object):
    """Holds position of the Antenna and can calculate the geometric
    delay for given horizontal (el, az) or equatorial (time, RA, DEC) coordinates"""

    def __init__(self, loc, enu):
        self.loc = loc
        self.enu = enu
        self.dxdydz = self.loc.get_ecef_delta_from_enu(enu[0], enu[1], enu[2])

    def __repr__(self):
        return "[%s]" % self.enu

    def get_geo_delay(self, sloc, utc_time):
        """
        Calculate and return geometric delay for each antenna
        Negative value means the wavefront hits the antenna earlier (than the zero ENU location)
        """
        el, az = self.loc.equatorial_to_horizontal(utc_time, sloc.ra, sloc.dec)
        return self.get_geo_delay_horizontal(el, az)

    def get_geo_delay_horizontal(self, src_0):
        """
        Calculate and return geometric delay for each antenna
        Negative value means the wavefront hits the antenna earlier (than the zero ENU location)
        t_antenna - t_origin
        """
        el_0, az_0, r_0 = src_0.elevation, src_0.azimuth, src_0.r
        if src_0.r > 1e4:
            src_0.r = 1.0e4

        # el_0 and az_0 from the PoV of the antenna, corrected for the distance of the source.
        # if source is far away: el = el_0 and az = az_0
        object_vector = np.zeros(3)
        object_vector[0] = az_0.sin() * el_0.cos()
        object_vector[1] = az_0.cos() * el_0.cos()
        object_vector[2] = el_0.sin()

        object_vector_ant_pov = r_0 * object_vector - self.enu

        r = np.linalg.norm(object_vector_ant_pov)
        path_diff = r - r_0
        delay = path_diff / constants.V_LIGHT
        # print(delay)
        return delay

    def calcUVW(self, utc_time, ra, dec):
        """u is towards the east, v is towards the north,
        w points to the phase centre"""

        x, y, z = self.dxdydz
        ha = self.loc.GHA(utc_time, ra)
        u = ha.sin() * x + ha.cos() * y
        v = -dec.sin() * ha.cos() * x + dec.sin() * ha.sin() * y + dec.cos() * z
        w = dec.cos() * ha.cos() * x - dec.cos() * ha.sin() * y + dec.sin() * z
        return np.array([u, v, w])


def get_geo_delay_horizontal(a0, a1, src):
    assert isinstance(src, HorizontalSource)
    """
    The delay is negative if a1 is closer to the source than a0

    t_a1 - t_a0 = (t_a1 - t_origin) - (t_a0 - t_origin)
    """
    d0 = a0.get_geo_delay_horizontal(src)
    d1 = a1.get_geo_delay_horizontal(src)
    return d1 - d0


def get_UVW(a0, a1, utc_time, ra, dec):
    uvw0 = a0.calcUVW(utc_time, ra, dec)
    uvw1 = a1.calcUVW(utc_time, ra, dec)
    # print(uvw0, uvw1, uvw1 - uvw0)
    return uvw0 - uvw1
