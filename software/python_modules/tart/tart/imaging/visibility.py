import json
import h5py
import dateutil
import os

import numpy as np

try:
    import cPickle as pickle
except:
    import pickle

from tart.util import angle
from tart.util import skyloc
from tart.util import constants

from tart.operation import observation
from tart.operation import settings

from tart.simulation.simulation_source import HorizontalSource


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
        self.phase_el = angle.from_dms(90.0)
        self.phase_az = angle.from_dms(0.0)
        self.config = config
        self.timestamp = timestamp

    def set_visibilities(self, v, b):
        if not isinstance(b, list):
            raise RuntimeError(
                f"Baselines (type={type(b)}) must be passed as a list object"
            )
        self.baselines = b
        self.v = v

    r"""Rotated one, aimed at ra, decl

    Justification:

    Let
        s_1(t) = N(t) e^{j omega t}

    t_g^{01) = t_g^1 - t_g^0 (arrival at a1 - arrival at a0)
    if t_g^{01} < 0, then s_0 arrives later than s_1 (defined by antennas.get_geo_delay_horizontal)
    therefore:

        s_0(t) = s_1(t - t_g^{01})

    v(0,1) = <s_0(t) s_1^{*}(t)>
                 = <s_1(t - t_g^{01}) s_1^{*}(t)>
                 = <N(t) e^{j omega t} e^{-j omega t_g^{01}} N^*(t) e^{-j omega t}>
                 = e^{-j omega t_g^{01}} <N(t) N^*(t)>

    So, after rotation

     <N(t) N^*(t)> = v(0,1) e^{+j omega t_g^{01}}
    """

    def rotate(self, sky_location):
        from tart.simulation import antennas

        stopped_vis = []
        omega = self.config.get_operating_frequency() * 2.0 * np.pi
        # Now we must do fringe stopping

        loc = self.config.get_loc()
        el, az = loc.equatorial_to_horizontal(
            self.timestamp, sky_location.ra, sky_location.dec
        )
        hsource = HorizontalSource(r=9.0e99, azimuth=az, elevation=el)

        ant_pos = self.config.get_antenna_positions()
        for v, b in zip(self.v, self.baselines):
            a0 = antennas.Antenna(loc, ant_pos[b[0]])
            a1 = antennas.Antenna(loc, ant_pos[b[1]])

            tg = antennas.get_geo_delay_horizontal(a0, a1, hsource)
            # tg is t_a1 - t_a0
            # (negative if a1 is closer to source than a0)

            # print(b, omega*tg)
            v = v * np.exp(-1.0j * omega * tg)
            stopped_vis.append(v)

        self.phase_el = el
        self.phase_az = az
        self.v = stopped_vis

    def vis(self, i, j):
        if j == i:
            raise "Baseline [%d,%d] is invalid" % (i, j)
        if j < i:  # The first index should be before the second
            return np.conjugate(self.vis(j, i))
        for k, b in enumerate(self.baselines):
            if b == [i, j]:
                return self.v[k]
        raise "Baseline [%d,%d] is invalid" % (i, j)

    def get_closure_phase(self, i, j, k):
        return (
            np.angle(self.vis(i, j))
            + np.angle(self.vis(j, k))
            - np.angle(self.vis(i, k))
        )

    def toString(self):
        ret = ""
        for i, b in enumerate(self.baselines):
            ret += " V(%s)=%g, I%g" % (str(b), np.abs(self.v[i]), np.angle(self.v[i]))
        return ret

    def __repr__(self):
        return "vis(ts={})".format(self.timestamp)


def Visibility_Lsq(vis1, vis2):
    """ Return least square based on the phases of 2 visibilities """
    if vis1.config.get_num_antenna() == vis2.config.get_num_antenna():
        difflist = []
        for v1, v2 in zip(vis1.v, vis2.v):
            diff = np.abs(np.angle(v1) - np.angle(v2))
            if diff > np.pi:
                diff = 2.0 * np.pi - diff
            difflist.append(diff)
        diffarr = np.array(difflist)
        return np.power(diffarr, 2).sum()


def Visibility_From_Conf(config, timestamp, phase_el, phase_az):
    obs = observation.Observation(timestamp=timestamp, config=config)
    vis = Visibility(obs, phase_el, phase_az)
    return vis


def Visibility_Save_JSON(vis, filename):
    json_data = {}
    json_data["timestamp"] = vis.timestamp
    json_data["phase_el"] = vis.phase_el.to_degrees()
    json_data["phase_az"] = vis.phase_az.to_degrees()
    json_data["config"] = vis.config.Dict
    json_data["baselines"] = vis.baselines
    json_data["vis"] = vis.v
    with open(filename, "w") as outfile:
        # The default=str handles datetime objects as strings
        json.dump(
            json_data,
            outfile,
            default=str,
            sort_keys=True,
            indent=4,
            separators=(",", ": "),
        )


"""
    Load and save lists of visibilities. Use filename extensions to deal with the deprecated .pkl files (or .vis files)
"""


def list_save(vis_list, ant_pos, cal_gain, cal_ph, filename):
    _, file_extension = os.path.splitext(filename)

    if ".pkl" == file_extension:
        to_pkl(vis_list, filename)
    elif ".vis" == file_extension:
        to_pkl(vis_list, filename)
    elif ".hdf" == file_extension:
        to_hdf5(vis_list, ant_pos, cal_gain, cal_ph, filename)
    else:
        raise RuntimeError(
            "Unknown visibility file extension {}".format(file_extension)
        )


def list_load(filename):
    _, file_extension = os.path.splitext(filename)

    if ".pkl" == file_extension:
        vis_list = from_pkl(filename)
        err_count = 0
        ret = []
        for v in vis_list:
            if isinstance(v, tuple):
                err_count += 1
            else:
                ret.append(v)
        if err_count > 0:
            print(
                (
                    "Warning. Visibility file: %s had %i visibilities missing"
                    % (filename, err_count)
                )
            )
        return ret
    elif ".vis" == file_extension:
        vis_list = from_pkl(filename)
        return vis_list
    elif ".hdf" == file_extension:
        vis_list = from_hdf5(filename)
        return vis_list
    else:
        raise RuntimeError("Unknown file extension {}".format(file_extension))


"""
    Deal with Pickle files (Deprecated in favour of HDF5 files)
"""


def to_pkl(vis, filename):
    save_data = open(filename, "wb")
    pickle.dump(vis, save_data, pickle.HIGHEST_PROTOCOL)
    save_data.close()


def from_pkl(filename):
    with open(filename, "rb") as load_data:
        ret = pickle.load(load_data, encoding="latin1")
    return ret


"""
    Deal with HDF5 Files
"""


def to_hdf5(vis_list, ant_pos, cal_gain, cal_ph, filename):
    """Save the Observation object,
    to a portable HDF5 format
    """
    if not isinstance(vis_list, list):
        raise RuntimeError("vis_list must be a list of visibility objects")

    vis0 = vis_list[0]

    vis_data = [vis.v for vis in vis_list]
    vis_ts = [vis.timestamp.isoformat() for vis in vis_list]

    with h5py.File(filename, "w") as h5f:
        dt = h5py.special_dtype(vlen=str)
        conftype = h5py.special_dtype(vlen=bytes)

        conf_dset = h5f.create_dataset("config", (1,), dtype=conftype)
        conf_dset[0] = vis0.config.to_json()
        h5f.create_dataset(
            "phase_elaz", data=[vis0.phase_el.to_degrees(), vis0.phase_az.to_degrees()]
        )
        h5f.create_dataset("baselines", data=vis0.baselines)

        h5f.create_dataset("vis", data=np.array(vis_data))
        h5f.create_dataset("gains", data=np.array(cal_gain))
        h5f.create_dataset("phases", data=np.array(cal_ph))

        h5f.create_dataset("antenna_positions", data=np.array(ant_pos))

        h5f.create_dataset("timestamp", data=np.array(vis_ts, dtype=object), dtype=dt)

        # ts_dset = h5f.create_dataset('timestamp', (len(vis_ts),), dtype=dt)
        # for i,ts in enumerate(vis_ts):
        # print(ts)
        # ts_dset[i] = ts


def from_hdf5(filename):
    """Load the Visibility object,
    from a portable HDF5 format
    """

    vis_list = []
    ret = {}

    with h5py.File(filename, "r") as h5f:
        config_json = np.string_(h5f["config"][0])
        config = settings.from_json(config_json)
        hdf_baselines = h5f["baselines"][:].tolist()
        ant_pos = h5f["antenna_positions"][:]
        hdf_phase_elaz = h5f["phase_elaz"][:]

        hdf_timestamps = h5f["timestamp"]
        timestamps = [dateutil.parser.parse(x) for x in hdf_timestamps]

        hdf_vis = h5f["vis"][:]
        config.set_antenna_positions(ant_pos)
        for ts, v in zip(timestamps, hdf_vis):
            vis = Visibility(config=config, timestamp=ts)
            vis.set_visibilities(v=v, b=hdf_baselines)
            vis.phase_el = hdf_phase_elaz[0]
            vis.phase_az = hdf_phase_elaz[1]
            vis_list.append(vis)

        ret["vis_list"] = vis_list
        ret["config_json"] = config_json
        ret["config"] = config
        ret["ant_pos"] = ant_pos
        ret["timestamps"] = timestamps
        ret["baselines"] = hdf_baselines
        ret["gain"] = h5f["gains"][:]
        ret["phase_offset"] = h5f["phases"][:]

    return ret
