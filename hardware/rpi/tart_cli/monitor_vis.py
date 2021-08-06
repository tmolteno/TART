import traceback
from tart.imaging import calibration
from tart.imaging import synthesis

import numpy as np
import time
import yaml
import logging
import logging.config
import os.path
import os

# BUG: resuQ piles up.

def gen_calib_image(vislist, calibration_dir):
    CAL_MEASURE_VIS_LIST = []
    cal_file = calibration_dir + 'monitor_vis_calibration.json'
    MIN = True
    for vis in vislist[:1]:
        if not os.path.exists(cal_file):
          cv = calibration.CalibratedVisibility(vis)
          flagged_bl = []
          #dead = [0,2,4,7,9,14,20,22]
          for i in range(0,24-1):
              for j in range(i+1,24):
		  if (i > 5) or (j > 5):
                  #if (i in dead) or (j in dead):
                      flagged_bl.append([i,j])
          cv.set_flagged_baselines(flagged_bl)
          if MIN:
              print('fmin')
          else:
              for i in range(1,6):
                  a_i = np.abs(cv.get_visibility(0,i))
                  ang_i = np.angle(cv.get_visibility(0,i))
                  cv.set_gain(i, 1./a_i)
                  cv.set_phase_offset(i, ang_i)
          cv.to_json(cal_file)
        else:
            #print 'loading.. calibration'
            cv = calibration.from_JSON_file(vis, cal_file)
            CAL_MEASURE_VIS_LIST.append(cv)
    CAL_SYN = synthesis.Synthesis_Imaging(CAL_MEASURE_VIS_LIST)
    CAL_SYN.set_grid_file(calibration_dir + 'monitor_vis_grid.idx')
    #CAL_IFT, CAL_EXTENT = CAL_SYN.get_ift(nw=20, num_bin=2**7, use_kernel=False)
    CAL_IFT, CAL_EXTENT = CAL_SYN.get_ift_simp(nw=20, num_bin=2**7)
    return CAL_IFT, CAL_EXTENT

def gen_qt_image(vislist, plotQ, calibration_dir):
    res, ex = gen_calib_image(vislist, calibration_dir)
    #a = np.random.random(size=(2**7,2**7))*np.sin(np.arange(2**7))
    #res = np.fft.fft2(a)
    #plotQ.put(np.abs(res).T.astype(np.float16))
    plotQ.put(np.abs(res).astype(np.float16))
    #plotQ.put(np.abs(res).T.astype(np.int8))

def gen_qt_vis_abs_ang(vislist,meanslist, plotQ):
    #graphs = [[np.angle(v.v[i]) for v in vislist] for i in range(5)]
    graphs = np.array(meanslist)[:,:6].T
    #print meanslist
    plotQ.put(np.array(graphs))

def gen_mpl_image(vislist, gfx, fig, ax , cb, calibration_dir):
    CAL_IFT, CAL_EXTENT = gen_calib_image(vislist, calibration_dir)
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

def result_loop(result_queue, chunk_size, mode='syn', plotQ=None, calibration_dir=None):
    logger = logging.getLogger(__name__)

    #if not ('qt' in mode):
    ##if ARGS.mode!= 'qt':
        #import matplotlib.pyplot as plt
        #plt.ion()
        #fig, ax = plt.subplots(1,1)
        #gfx = None
        #cb = None
    cnt = 0
    max_len = 200
    res = []
    vislist = []
    meanslist = []
    while (True):
        if (False == result_queue.empty()):
            try:
                #print 'ResuQ size', result_queue.qsize()
                while result_queue.qsize()>0:
                    vis, means = result_queue.get()
                    vislist.insert(0,vis)
                    meanslist.insert(0,means)
                gen_qt_image(vislist, plotQ, calibration_dir)
                vislist = []

                #if mode=='syn':
                    #if len(vislist)>=chunk_size:
                        #gfx, cb = gen_mpl_image(vislist, gfx, fig, ax, cb, calibration_dir)
                        #vislist = []
                #elif mode =='qt':
                    #if len(vislist)>=chunk_size:
                #elif mode =='qt_vis':
                    #cnt += 1
                    #if cnt >= chunk_size:
                        #cnt = 0
                        #gen_qt_vis_abs_ang(vislist,meanslist,plotQ)
                    #if len(vislist)>max_len:
                        #vislist.pop()
                        #meanslist.pop()
                #elif mode=='absang':
                    #if gfx is None:
                        #gfx, = ax.plot([],[],c='blue')
                        #ax2 = ax.twinx()
                        #gfx2, = ax2.plot([],[], c='red')
                        #ax.set_ylim(-4,4)
                        #ax2.set_ylim(0.05,0.3)
                        #plt.xlim(0, max_len)
                    #if len(vislist)>max_len:
                        #vislist.pop()
                    #cnt += 1
                    #if result_queue.qsize()< 5:
                        #if cnt >= chunk_size:
                            #vis_list_01 = [np.angle(i.v[4]) for i in vislist]
                            #vis_list_01_m = [np.abs(i.v[4]) for i in vislist]
                            #gfx.set_data(np.arange(len(vis_list_01)),vis_list_01)
                            #gfx2.set_data(np.arange(len(vis_list_01_m)),vis_list_01_m)
                            #fig.canvas.draw()
                            #cnt = 0
                #elif mode=='calib':
                    #cnt +=1
                    #print cnt
                    #if cnt>10:
                        #res = []
                        #labels = []
                        #for i in range(1):
                            #for j in range(i+1,6):
                                #temp = []
                                #temp_abs = []
                                #labels.append('%i_%i_ang' % (i,j))
                                #labels.append('%i_%i_abs' % (i,j))
                                #for vi in vislist:
                                    #cv = calibration.CalibratedVisibility(vi)
                                    #temp.append(np.angle(cv.get_visibility(i,j)))
                                    #temp_abs.append(np.abs(cv.get_visibility(i,j)))
                                #res.append(temp)
                                #print 'p', i, j, np.mean(temp)
                                #print 'abs', i, j, np.mean(temp_abs)
                                #res.append(temp_abs)
                        #res = np.array(res)
                        #print res
                        #for r,label in zip(res, labels):
                            #plt.plot(r,label=label)
                            #plt.legend()
            except Exception as e:
                logger.error( "Measurement Processing Error %s" % str(e))
                logger.error(traceback.format_exc())
        else:
            time.sleep(0.0001)


