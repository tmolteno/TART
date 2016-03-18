#calibration.py

import json

import numpy as np

class CalibratedVisibility(object):
    def __init__(self, vis):
        self.vis = vis
        self.flagged_baselines = []
        self.phase_offset = np.zeros(self.get_config().num_antennas)

    def get_config(self):
        return self.vis.config

    def get_timestamp(self):
        return self.vis.timestamp

    def __get_vis(self,bl):
        return self.vis.v[self.vis.baselines.index(bl)]

    def flag_baseline(self,i,j):
        bl = [i,j]
        if bl not in self.flagged_baselines:
            vis_idx = self.vis.baselines.index(bl)
            self.flagged_baselines.append(bl)
            # print 'flagged', vis_idx, bl

    def get_visibility(self,i,j):
        bl = [i,j]
        if bl in self.flagged_baselines:
            raise
        else:
            return self.__get_vis(bl) * np.exp(-1j*(self.get_phase_offset(i)-self.get_phase_offset(j)))

    def get_baselines(self):
        return [bl for bl in self.vis.baselines if bl not in self.flagged_baselines]

    def set_flagged_baselines(self, flaged_list):
        self.flagged_baselines = flaged_list


    def flag_tile(self, tile_idx):
        for i in range(0,self.get_config().num_antennas):
            for j in range(i+1,self.get_config().num_antennas):
                if ((i/6) == tile_idx) or ((j/6) == tile_idx):
                    self.flag_baseline(i,j)

    def leave_parallel_baselines(self,ew_threshold=10,ns_threshold=10):
        ant_positions = np.array(self.get_config().ant_positions)
        for i in range(0,self.get_config().num_antennas):
            for j in range(i+1,self.get_config().num_antennas):
                diff = ant_positions[i]-ant_positions[j]
                if np.abs(diff[0])>ew_threshold or np.abs(diff[1])>ns_threshold:
                    self.flag_baseline(i,j)

    def set_phase_offset(self,i, val):
        self.phase_offset[i] = val

    def get_phase_offset(self,i):
        return self.phase_offset[i]

    def to_json(self):
        calib_dict = {}
        calib_dict['phase_offset'] = self.phase_offset.tolist()
        calib_dict['flagged_baselines'] = self.flagged_baselines
        json_str = json.dumps(calib_dict)
        return json_str

def from_JSON(vis, json_str):
    calib_dict = json.loads(json_str)    
    ret = CalibratedVisibility(vis)
    ret.set_phase_offset(range(ret.get_config().num_antennas),np.array(calib_dict['phase_offset']))
    ret.set_flagged_baselines(calib_dict['flagged_baselines'])
    return ret

import argparse

from tart.imaging import visibility



if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Generate visibilities for a skymodel in parallel.')
  parser.add_argument('--vis', default="/freenas2/tart/visibility/vis_2015_08_10.vis",required=False, help="")
  args = parser.parse_args()

  vis = visibility.Visibility_Load(args.vis) #[:20]

  cal_vis = CalibratedVisibility(vis[0])
  print cal_vis.get_visibility(0,1)
  print cal_vis.get_baselines()
  print len(cal_vis.get_baselines())

  cal_vis.flag_baseline(0,1)

  print cal_vis.get_baselines()
  print len(cal_vis.get_baselines())

  # print cal_vis.get_visibility(0,1)