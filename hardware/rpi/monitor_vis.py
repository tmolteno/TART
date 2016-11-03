import argparse
import numpy as np
import matplotlib.pyplot as plt

from tart.operation import settings

from tart.imaging import correlator
from tart.imaging import visibility
from tart.imaging import calibration
from tart.imaging import synthesis
from tart.util import angle
import datetime

def get_corr(xor_sum, n_samples):
  return 1. - 2*xor_sum/float(n_samples)

def get_vis_object(data, n_samples, config):
  num_ant = config.num_antennas
  timestamp = datetime.datetime.utcnow()
  vis = visibility.Visibility_From_Conf(config, timestamp, angle.from_dms(90.), angle.from_dms(0.))
  v = []
  baselines = []
  xor_cos = data[0:-24:2]
  xor_sin = data[1:-24:2]
  n_samples = 2**23
  corr_cos = get_corr(xor_cos, n_samples)
  corr_sin = get_corr(xor_sin, n_samples)
  means = (data[-24:])/float(n_samples)*2.-1
  
  for i in range(0, num_ant):
    for j in range(i+1, num_ant):
      idx = len(baselines)
      baselines.append([i,j])
      v_real = correlator.van_vleck_correction( -means[i]*means[j] + corr_cos[idx] )
      v_imag = correlator.van_vleck_correction( -means[i]*means[j] + corr_sin[idx] )
      v_com = v_real-1.j*v_imag
      v.append(v_com)
  vis.set_visibilities(v, baselines)
  return vis

import time

def gen_fake_data():
  n_ant = 24
  return np.random.randint(0,2**23,n_ant**2)

if __name__ == '__main__':
  PARSER = argparse.ArgumentParser(description='Create an TART Image.')
  PARSER.add_argument('--vis', required=False, help="Visibilities data file.")
  PARSER.add_argument('--config', required=False, help="Config file.")
  ARGS = PARSER.parse_args()
  
  gfx = None
  plt.ion()
  fig,ax = plt.subplots(1,1)
  
  if ARGS.vis != None:
    VIS_LIST = visibility.Visibility_Load(ARGS.vis)
  else: 
    config = settings.Settings(ARGS.config)
    n_samples = 2**23
  
  for i in range(20):
    data = gen_fake_data()
    vis = get_vis_object(data, n_samples, config)
    CAL_MEASURE_VIS_LIST = []
    CAL_MEASURE_VIS_LIST.append(calibration.CalibratedVisibility(vis))
    CV = CAL_MEASURE_VIS_LIST[0:1]
    CAL_SYN = synthesis.Synthesis_Imaging(CV)
    CAL_IFT, CAL_EXTENT = CAL_SYN.get_ift(nw=50, num_bin=2**8, use_kernel=False)
    abs_v = CAL_IFT.real
    print gfx
    if gfx is None:
      gfx = ax.imshow(abs_v, extent=CAL_EXTENT, cmap=plt.cm.rainbow)
    else:
      gfx.set_data(abs_v)
      fig.canvas.draw()

