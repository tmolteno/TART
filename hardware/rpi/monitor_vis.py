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

import time
from tartdsp import TartSPI

def gen_calib_image(vislist, gfx, fig, ax, cb):
  CAL_MEASURE_VIS_LIST = []
  for vis in vislist:
    cv = calibration.CalibratedVisibility(vis)
    for i in range(6,24):
      cv.flag_antenna(i)
    CAL_MEASURE_VIS_LIST.append(cv)

  CV = CAL_MEASURE_VIS_LIST #[0:1]
  CAL_SYN = synthesis.Synthesis_Imaging(CV)
  CAL_IFT, CAL_EXTENT = CAL_SYN.get_ift(nw=50, num_bin=2**8, use_kernel=False)
  abs_v = CAL_IFT.real
  if gfx is None:
    gfx = ax.imshow(abs_v, extent=CAL_EXTENT, cmap=plt.cm.rainbow)
    cb = fig.colorbar(gfx, orientation='horizontal')
  else:
    gfx.set_data(abs_v)
    cb.set_clim(vmin=np.min(abs_v), vmax=np.max(abs_v))
    cb.draw_all()
    fig.canvas.draw()
  return gfx, cb

if __name__ == '__main__':
  PARSER = argparse.ArgumentParser(description='Create an TART Image.')
  PARSER.add_argument('--vis', required=False, help="Visibilities data file.")
  PARSER.add_argument('--config', required=False, help="Config file.")
  PARSER.add_argument('--synthesis', required=False, action='store_true', help="generate telescope synthesis image")
  PARSER.add_argument('--absang', required=False, action='store_true', help="generate abs and angle for vis")
  PARSER.add_argument('--calib', required=False, action='store_true', help="generate abs and angle for vis")
  PARSER.add_argument('--save_vis', required=False, action='store_true', help="generate abs and angle for vis")
  PARSER.add_argument('--vis_prefix', required=False, type=str, default='vis', help="generate abs and angle for vis")
  PARSER.add_argument('--blocksize', default=23, type=int, help='exponent of correlator block-size')

  ARGS = PARSER.parse_args()
  
  plt.ion()
  fig,ax = plt.subplots(1,1)
  gfx = None
  cb = None
  vislist = []
  cnt = 0
  res = []
  num_vis = 100
  
  if ARGS.vis != None:
    VIS_LIST = visibility.Visibility_Load(ARGS.vis)
  else: 
    config = settings.Settings(ARGS.config)
    blocksize = ARGS.blocksize
    n_samples = 2**blocksize
    tart_instance = TartSPI()
    tart_instance.reset()
    tart_instance.read_status(True)
    permutation = tart_instance.load_permute()
    tart_instance.debug(on=False, shift=False, count=False, noisy=True)
    tart_instance.read_status(True)
    tart_instance.start(blocksize, True)

  while True:
    print len(vislist)
    data = get_data(tart_instance)
    vis = get_vis_object(data, n_samples, config)
    vislist.insert(0,vis)
    if ARGS.synthesis:
      if len(vislist)>5:
        gfx, cb = gen_calib_image(vislist, gfx, fig, ax, cb)
        vislist = []
    if ARGS.absang:
      if gfx is None:
        gfx, = ax.plot([],[],c='blue')
        ax2 = ax.twinx()
        gfx2, = ax2.plot([],[], c='red')
      max_len = 100
      update_len = 20
      if len(vislist)>max_len:
        vislist.pop()
      vis_list_01 = [np.angle(i.v[4]) for i in vislist]
      vis_list_01_m = [np.abs(i.v[4]) for i in vislist]
      cnt += 1
      if cnt == update_len:
        plt.xlim(0,max_len)
        ax.set_ylim(-4,4)
        ax2.set_ylim(0.05,0.3)
        gfx.set_data(np.arange(len(vis_list_01)),vis_list_01)
        gfx2.set_data(np.arange(len(vis_list_01_m)),vis_list_01_m)
        fig.canvas.draw()
        cnt = 0
    if ARGS.calib:
      cnt +=1
      print cnt
      if cnt>10:
        res = []
        labels = []
        for i in range(1):
          for j in range(i+1,6):
            temp = []
            temp_abs = []
            labels.append('%i_%i_ang' % (i,j))
            labels.append('%i_%i_abs' % (i,j))
            for vi in vislist:
              cv = calibration.CalibratedVisibility(vi)
              cv.set_phase_offset(0, 0.00)
              cv.set_phase_offset(1, -2.31)
              cv.set_phase_offset(2, 1.32)
              cv.set_phase_offset(3, 1.39)
              cv.set_phase_offset(4, 0.24)
              cv.set_phase_offset(5, 2.92)
              cv.set_gain(1,1/0.122974691125) 
              cv.set_gain(2,1/0.1385) 
              cv.set_gain(3,1/0.132) 
              cv.set_gain(4,1/0.09) 
              cv.set_gain(5,1/0.089) 
              temp.append(np.angle(cv.get_visibility(i,j)))
              temp_abs.append(np.abs(cv.get_visibility(i,j)))
            res.append(temp)
            print 'p', i, j, np.mean(temp)
            print 'abs', i, j, np.mean(temp_abs)
            res.append(temp_abs)
        res = np.array(res)
        print res
        for r,label in zip(res, labels):
          plt.plot(r,label=label)
        plt.legend()
        plt.waitforbuttonpress()
 
