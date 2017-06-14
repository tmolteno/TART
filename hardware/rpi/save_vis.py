import time, os
import traceback
from tart.imaging import visibility

def result_loop(result_queue, chunk_size, vis_prefix, logger):
    vislist = []
    while(True):
        if (False == result_queue.empty()):
            try:
                vis, means = result_queue.get()
                vislist.append(vis)
                if len(vislist)>chunk_size:
                    fname = vis_prefix + "_" + vis.timestamp.strftime('%Y-%m-%d_%H_%M_%S.%f')+".vis"
                    #fname =  "%s_%02i_%02i_%02i.vis" %(vis_prefix, vis.timestamp.hour, vis.timestamp.minute, vis.timestamp.second)
                    print 'saved ', vis, ' to', fname
                    visibility.Visibility_Save(vislist, fname)
                    #import signal, os
                    #os.kill(os.getpid(), signal.SIGUSR1)

                    vislist = []
            except Exception, e:
                logger.error( "PostProcessing Error %s" % str(e))
                logger.error(traceback.format_exc())
        else:
            time.sleep(0.00001)
