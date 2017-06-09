import traceback
from tart.imaging import calibration
from tart.imaging import synthesis
import numpy as np
import time
import yaml
import logging
import logging.config


def gen_qt_vis_abs_ang(vislist, plotQ):
    print vislist[0].v[0]

    #plotQ.put(res.real.T)

def result_loop(result_queue, chunk_size, mode='syn', plotQ=None):
    logger = logging.getLogger(__name__)

    if mode!='qt':
        import matplotlib.pyplot as plt
        plt.ion()
        fig, ax = plt.subplots(1,1)
        gfx = None
        cb = None
    cnt = 0
    max_len = 1000
    res = []
    vislist = []
    while (True):
        if (False == result_queue.empty()):
            try:
                vis = result_queue.get() 
                vislist.insert(0,vis)
                if mode=='syn':
                    if len(vislist)>=chunk_size:
                        gfx, cb = gen_mpl_image(vislist, gfx, fig, ax, cb)
                        vislist = []
                elif mode =='qt':
                    if len(vislist)>=chunk_size:
                        gen_qt_image(vislist, plotQ)
                        vislist = []

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
            time.sleep(0.001)


