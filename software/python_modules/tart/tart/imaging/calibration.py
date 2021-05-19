#
# A class the encapsulates a CalibratedVisibility object.
#
# Copyright (c) Tim Molteno 2013-2020. tim@elec.ac.nz
#

import json

import numpy as np
from tart.util import constants


class CalibratedVisibility(object):
    def __init__(self, vis):
        self.vis = vis
        self.flagged_baselines = []
        self.phase_offset = np.zeros(self.get_config().get_num_antenna())
        self.tile_phase_offset = np.zeros(4)
        self.gain = np.ones(self.get_config().get_num_antenna())

    def set_config(self, config):
        self.vis.config = config

    def get_config(self):
        return self.vis.config

    def get_timestamp(self):
        return self.vis.timestamp

    def __get_vis(self, bl):
        return self.vis.v[self.vis.baselines.index(bl)]

    def flag_baseline(self, i, j):
        bl = [i, j]
        if bl not in self.flagged_baselines:
            # vis_idx = self.vis.baselines.index(bl)
            self.flagged_baselines.append(bl)
            # print('flagged', vis_idx, bl)

    def flag_antenna(self, i):
        for bl in self.vis.baselines:
            if i in bl:
                [a, b] = bl
                self.flag_baseline(a, b)

    def get_visibility(self, i, j):
        if j < i:
            return np.conjugate(self.get_visibility(j, i))

        bl = [i, j]
        if bl in self.flagged_baselines:
            raise
        else:
            v = self.__get_vis(bl)
            return (
                v
                * self.get_gain(i)
                * self.get_gain(j)
                * np.exp(-1j * (self.get_phase_offset(i) - self.get_phase_offset(j)))
            )

    def get_all_visibility(self):
        bl = np.asarray(self.get_baselines())
        v_arr = np.asarray(self.get_unflagged_vis())
        ret = (
            v_arr
            * self.get_gain(bl[:, 0])
            * self.get_gain(bl[:, 1])
            * np.exp(
                -1j
                * (self.get_phase_offset(bl[:, 0]) - self.get_phase_offset(bl[:, 1]))
            )
        )
        return ret, bl

    def get_all_uvw(self):
        c = self.get_config()
        ant_p = np.asarray(c.get_antenna_positions())

        bls = np.asarray(self.get_baselines())
        bl_pos = ant_p[bls]
        uu_a, vv_a, ww_a = (bl_pos[:, 0] - bl_pos[:, 1]).T / constants.L1_WAVELENGTH
        return uu_a, vv_a, ww_a

    def get_unflagged_vis(self):
        return [
            vis
            for bl, vis in zip(self.vis.baselines, self.vis.v)
            if bl not in self.flagged_baselines
        ]

    def get_baselines(self):
        return [bl for bl in self.vis.baselines if bl not in self.flagged_baselines]

    def get_baseline_lengths(self):
        pos = self.get_config().get_antenna_positions()
        # print(pos)
        ret = []
        for bln in self.get_baselines():
            [i, j] = bln
            ret.append([pos[j][0] - pos[i][0], pos[j][1] - pos[i][1]])
        return np.abs(ret)

    def set_flagged_baselines(self, flaged_list):
        self.flagged_baselines = flaged_list

    def flag_tile(self, tile_idx):
        for bln in self.get_baselines():
            [i, j] = bln
            if ((i // 6) == tile_idx) or ((j // 6) == tile_idx):
                self.flag_baseline(i, j)

    def leave_parallel_baselines(self, ew_threshold=10, ns_threshold=10):
        ant_positions = np.array(self.get_config().get_antenna_positions())
        for bln in self.get_baselines():
            [i, j] = bln
            diff = ant_positions[i] - ant_positions[j]
            if np.abs(diff[0]) > ew_threshold or np.abs(diff[1]) > ns_threshold:
                self.flag_baseline(i, j)

    def leave_intra_tile_baselines(self):
        ant_positions = np.array(self.get_config().get_antenna_positions())
        for bln in self.get_baselines():
            [i, j] = bln
            if (i // 6) != (j // 6):
                self.flag_baseline(i, j)

    def set_phase_offset(self, i, val):
        self.phase_offset[i] = np.asarray(val) % (2 * np.pi)

    def get_phase_offset(self, i):
        return self.phase_offset[i] + self.get_tile_phase_offset_for_antenna(i)

    def set_tile_phase_offset(self, i, val):
        self.tile_phase_offset[i] = val

    def get_tile_offset(self, i):
        return self.tile_phase_offset[i]

    def get_tile_phase_offset_for_antenna(self, ant_idx):
        return self.tile_phase_offset[np.asarray(ant_idx) // 6]

    def set_gain(self, i, val):
        self.gain[i] = val

    def get_gain(self, i):
        return self.gain[i]

    def to_json(self, filename="gain_calibration.json"):
        calib_dict = {}
        calib_dict["gain"] = [
            int(1e5 * self.get_gain(i)) / float(1e5) for i in range(len(self.gain))
        ]
        calib_dict["phase_offset"] = [
            int(1e5 * self.get_phase_offset(i)) / float(1e5)
            for i in range(len(self.gain))
        ]
        calib_dict["flagged_baselines"] = self.flagged_baselines
        with open(filename, "w") as handle:
            json.dump(calib_dict, handle, sort_keys=True, indent=4, ensure_ascii=False)
        json_str = json.dumps(calib_dict)
        return json_str


def from_dict(vis, calib_dict):
    ret = CalibratedVisibility(vis)
    ret.set_phase_offset(
        list(range(ret.get_config().get_num_antenna())),
        np.array(calib_dict["phase_offset"]),
    )
    ret.set_gain(
        list(range(ret.get_config().get_num_antenna())), np.array(calib_dict["gain"])
    )
    ret.set_flagged_baselines(calib_dict["flagged_baselines"])
    return ret


def from_JSON(vis, json_str):
    calib_dict = json.loads(json_str)
    return from_dict(vis, calib_dict)


def from_JSON_file(vis, filename="gain_calibration.json"):
    with open(filename) as data_file:
        calib_dict = json.load(data_file)
    return from_dict(vis, calib_dict)


import argparse

from tart.imaging import visibility


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Generate visibilities for a skymodel in parallel."
    )
    parser.add_argument(
        "--vis",
        default="/freenas2/tart/visibility/vis_2015_08_10.vis",
        required=False,
        help="",
    )
    args = parser.parse_args()

    vis = visibility.list_load(args.vis)  # [:20]

    cal_vis = CalibratedVisibility(vis[0])
    # print(cal_vis.get_visibility(0,1))
    # print(cal_vis.get_baselines())
    # print(len(cal_vis.get_baselines()))

    cal_vis.flag_baseline(0, 1)

    # print(cal_vis.get_baselines())
    # print(len(cal_vis.get_baselines()))

    def gen_tile_vis_list(tile_no, vis):
        for v in vis:
            cal_vis_list = []
            cv = calibration.CalibratedVisibility(v)
            for i in [0, 1, 2, 3]:
                if i != tile_no:
                    cv.flag_tile(i)

        # cv.flag_tile(1)
        # cv.flag_tile(2)
        # cv.flag_tile(3)
        # cv.leave_parallel_baselines(ns_threshold=0.1)
        # cv.flag_baseline(0,5)
        cal_vis_list.append(cv)
        return cal_vis_list

    # t0_vis_list = gen_tile_vis_list(0,vis)
    # t1_vis_list = gen_tile_vis_list(1,vis)
    # t2_vis_list = gen_tile_vis_list(2,vis)
    # t3_vis_list = gen_tile_vis_list(3,vis)
    # fig, axarr = plt.subplots(2,2,sharex=True,sharey=True)
    # t_list = [t0_vis_list, t1_vis_list, t2_vis_list, t3_vis_list]
    # ift_arr = []
    # for i, ax in enumerate(axarr.ravel()):
    #   syn_tile = synthesis.Synthesis_Imaging(t_list[i])
    #   ift = syn_tile.get_image(nw=50,num_bin=2**9,ex_ax=ax)
    #   ift_arr.append(ift)

    # ift_arr = np.array(ift_arr)
    # print(ift_arr)
    # plt.figure()
    # plt.imshow(np.prod(np.abs(ift_arr),axis=0))

    # print(cal_vis.get_visibility(0,1))
