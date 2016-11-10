import traceback
from tart.imaging import calibration
from tart.imaging import synthesis
import numpy as np
import time
import yaml
import logging
import logging.config

def gen_calib_image(vislist):
    CAL_MEASURE_VIS_LIST = []
    t_cal = time.time()
    for vis in vislist:
        cv = calibration.CalibratedVisibility(vis)
        for i in range(6,24):
            cv.flag_antenna(i)
        cv.set_gain(0, 1)
        cv.set_gain(1, 1/0.694)
        cv.set_gain(2, 1/0.687)
        cv.set_gain(3, 1/0.698)
        cv.set_gain(4, 1/0.602)
        cv.set_gain(5, 1/0.641)
        cv.set_phase_offset(1, 1.766)
        cv.set_phase_offset(2, 0.25)
        cv.set_phase_offset(3, -1.83)
        cv.set_phase_offset(4, -1.138)
        cv.set_phase_offset(5, 1.13)
        for i in range(1,6):
          print 'a',i, np.abs(cv.get_visibility(0,i))
          print 'p',i, np.angle(cv.get_visibility(0,i))
        CAL_MEASURE_VIS_LIST.append(cv)
    print 't_cal', time.time()- t_cal
    t_obj = time.time()
    CAL_SYN = synthesis.Synthesis_Imaging(CAL_MEASURE_VIS_LIST)
    print 't_obj', time.time()- t_obj
    t_ift = time.time()
    #CAL_IFT, CAL_EXTENT = CAL_SYN.get_ift(nw=20, num_bin=2**7, use_kernel=False)
    CAL_IFT, CAL_EXTENT = CAL_SYN.get_ift_simp(nw=20, num_bin=2**7)
    print 't_uv,ift', time.time()- t_ift
    return CAL_IFT, CAL_EXTENT

def gen_qt_image(vislist, plotQ):
    res, ex = gen_calib_image(vislist)
    #a = np.random.random(size=(2**7,2**7))*np.sin(np.arange(2**7))
    #res = np.fft.fft2(a)
    plotQ.put(np.abs(res).T.astype(np.float16))

def gen_qt_vis_abs_ang(vislist,meanslist, plotQ):
    #graphs = [[np.angle(v.v[i]) for v in vislist] for i in range(5)]
    graphs = np.array(meanslist)[:,:6].T
    #print meanslist
    plotQ.put(np.array(graphs))

def gen_mpl_image(vislist, gfx, fig, ax , cb):
    CAL_IFT, CAL_EXTENT = gen_calib_image(vislist)
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

def result_loop(result_queue, chunk_size, mode='syn', plotQ=None):
    logger = logging.getLogger(__name__)

    if not ('qt' in mode):
    #if ARGS.mode!= 'qt':
        import matplotlib.pyplot as plt
        plt.ion()
        fig, ax = plt.subplots(1,1)
        gfx = None
        cb = None
    cnt = 0
    max_len = 200
    res = []
    vislist = []
    meanslist = []
    while (True):
        if (False == result_queue.empty()):
            try:
                vis, means = result_queue.get() 
                vislist.insert(0,vis)
                meanslist.insert(0,means)
                if mode=='syn':
                    if len(vislist)>=chunk_size:
                        gfx, cb = gen_mpl_image(vislist, gfx, fig, ax, cb)
                        vislist = []
                elif mode =='qt':
                    if len(vislist)>=chunk_size:
                        gen_qt_image(vislist, plotQ)
                        vislist = []
                elif mode =='qt_vis':
                    cnt += 1
                    if cnt >= chunk_size:
                        cnt = 0
                        gen_qt_vis_abs_ang(vislist,meanslist,plotQ)
                    if len(vislist)>max_len:
                        vislist.pop()
                        meanslist.pop()
                elif mode=='absang':
                    if gfx is None:
                        gfx, = ax.plot([],[],c='blue')
                        ax2 = ax.twinx()
                        gfx2, = ax2.plot([],[], c='red')
                        ax.set_ylim(-4,4)
                        ax2.set_ylim(0.05,0.3)
                        plt.xlim(0, max_len)
                    if len(vislist)>max_len:
                        vislist.pop()
                    cnt += 1
                    if result_queue.qsize()< 5:
                        if cnt >= chunk_size:
                            vis_list_01 = [np.angle(i.v[4]) for i in vislist]
                            vis_list_01_m = [np.abs(i.v[4]) for i in vislist]
                            gfx.set_data(np.arange(len(vis_list_01)),vis_list_01)
                            gfx2.set_data(np.arange(len(vis_list_01_m)),vis_list_01_m)
                            fig.canvas.draw()
                            cnt = 0
                elif mode=='calib':
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
            except Exception, e:
                logger.error( "Measurement Processing Error %s" % str(e))
                logger.error(traceback.format_exc())
        else:
            time.sleep(0.0001)


