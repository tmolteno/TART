import argparse
import numpy as np
import datetime
import time
import sys
import os, stat

import multiprocessing
import traceback
import logging.config
import yaml
logger = logging.getLogger(__name__)

from tartdsp import TartSPI

from tart.operation import settings
from tart.imaging import visibility
from tart.imaging import calibration
from tart.util import angle

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
    corr_cos = get_corr(xor_cos, n_samples)
    corr_sin = get_corr(xor_sin, n_samples)
    means = (data[-24:])/float(n_samples)*2.-1
    #print means
    for i in range(0, num_ant):
        for j in range(i+1, num_ant):
            idx = len(baselines)
            baselines.append([i,j])
            #v_real = correlator.van_vleck_correction( -means[i]*means[j] + corr_cos[idx] )
            #v_imag = correlator.van_vleck_correction( -means[i]*means[j] + corr_sin[idx] )
            v_real = -means[i]*means[j] + corr_cos[idx]
            v_imag = -means[i]*means[j] + corr_sin[idx]
            v_com = v_real-1.j*v_imag
            v.append(v_com)
    vis.set_visibilities(v, baselines)
    return vis, means

def get_data(tart):
    viz = tart.vis_read(False)
    return viz[tart.perm]

'''
    Grab data and push to queue
'''
def capture_loop(process_queue, blocksize, chunksize=None):
    tart_instance = TartSPI()
    tart_instance.reset()
    tart_instance.read_status(True)
    permutation = tart_instance.load_permute()
    tart_instance.debug(on=False, shift=False, count=False, noisy=True)
    tart_instance.read_status(True)
    tart_instance.capture(on=True, noisy=False)
    tart_instance.start(blocksize, True)

    if chunksize is not None:
        num_vis=0
        while num_vis < chunksize:
            try:
                # Add the data to the process queue
                data = get_data(tart_instance)
                process_queue.put(data)
                num_vis += 1
            except Exception, e:
                logger.error( "Capture Loop Error %s" % str(e))
                logger.error(traceback.format_exc())

            finally:
                print  'Acquired', data[0]
    else:
        while True:
            try:
                # Add the data to the process queue
                data = get_data(tart_instance)
                process_queue.put(data)
                num_vis += 1
            except Exception, e:
                logger.error( "Capture Loop Error %s" % str(e))
                logger.error(traceback.format_exc())

            finally:
                print  'Acquired', data[0]
    print 'Done acquisition. Closing Capture.'
    tart_instance.close()
    return 1
''' This is a separate procees that just
    waits for data to appear on the queue, and processes
    the files.
'''
def process_loop(process_queue, result_queue, config, blocksize, chunksize=None):
    p_bool = 0
    n_samples = 2**blocksize

    if chunksize is not None:
        num_vis=0
        while num_vis < chunksize:
            if (False == process_queue.empty()):
                try:
                    data = process_queue.get()
                    vis, means = get_vis_object(data, n_samples, config)
                    result_queue.put((vis, means))
                    num_vis += 1
                except Exception, e:
                    logger.error( "Processing Error %s" % str(e))
                    logger.error(traceback.format_exc())

    else:
        while (True):
            if (False == process_queue.empty()):
                try:
                    p_bool += 1
                    if p_bool >10:
                        p_bool = 0
                        print 'Status: ProcessQ: %i ResultQ: %i' % (process_queue.qsize(), result_queue.qsize())
                        #print '!!!!!!!!!!!!!!!!!!!!!! dropping frames when displaying  !!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                    data = process_queue.get()
                    vis, means = get_vis_object(data, n_samples, config)
                    result_queue.put((vis, means))
                except Exception, e:
                    logger.error( "Processing Error %s" % str(e))
                    logger.error(traceback.format_exc())
        #else:
            #time.sleep(0.00001)

if __name__=="__main__":
    PARSER = argparse.ArgumentParser()
    PARSER.add_argument('--config', required=False, help="Config file.")
    PARSER.add_argument('--calibration_dir', required=False, type=str, help="Config file.", default='6_ant_setup/')
    PARSER.add_argument('--vis_prefix', required=False, type=str, default='vis', help="generate abs and angle for vis")
    PARSER.add_argument('--blocksize', default=23, type=int, help='exponent of correlator block-size')
    PARSER.add_argument('--chunksize', default=10, type=int, help='number of vis objects per file')
    PARSER.add_argument('--mode', required=False, type=str, default='', help="save, save_single, qt, #syn, absang, calib")

    ARGS = PARSER.parse_args()

    # Setup up multiprocessing logging
    path = 'logging.yaml'
    if os.path.exists(path):
        with open(path, 'rt') as f:
            log_config = yaml.load(f.read())
        logging.config.dictConfig(log_config)

    # Setup up queues
    proc_queue = multiprocessing.Queue()
    result_queue = multiprocessing.Queue()


    # Import result_loop

    if ARGS.mode == 'save':
        vis_calc_process = multiprocessing.Process(target=process_loop, args=(proc_queue, result_queue, settings.Settings(ARGS.config), ARGS.blocksize,))
        from save_vis import result_loop
        post_process = multiprocessing.Process(target=result_loop, args=(result_queue, ARGS.chunksize,ARGS.vis_prefix,logger))
        capture_process = multiprocessing.Process(target=capture_loop, args=(proc_queue, ARGS.blocksize))


    elif ARGS.mode == 'save_single':
        vis_calc_process = multiprocessing.Process(target=process_loop, args=(proc_queue, result_queue, settings.Settings(ARGS.config), ARGS.blocksize, ARGS.chunksize))
        from save_vis import result_loop
        post_process = multiprocessing.Process(target=result_loop, args=(result_queue, ARGS.chunksize,ARGS.vis_prefix,logger,True))
        capture_process = multiprocessing.Process(target=capture_loop, args=(proc_queue, ARGS.blocksize, ARGS.chunksize))


    elif 'qt' in ARGS.mode:
        vis_calc_process = multiprocessing.Process(target=process_loop, args=(proc_queue, result_queue, settings.Settings(ARGS.config), ARGS.blocksize,))
        from pyqtgraph.Qt import QtGui, QtCore
        import qt_view
        import qt_graph
        print 'importing alternative result_loop'
        from monitor_vis import result_loop
        app = QtGui.QApplication([])
        if ARGS.mode == 'qt':
            plotter = qt_view.QtPlotter(app)
        else:
            plotter = qt_graph.QtPlotter(app)
        plotQ = plotter.getPort()
        post_process = multiprocessing.Process(target=result_loop, args=(result_queue, ARGS.chunksize, ARGS.mode, plotQ, ARGS.calibration_dir))
        capture_process = multiprocessing.Process(target=capture_loop, args=(proc_queue, ARGS.blocksize))



    # Start processes in reverse order of chain
    post_process.start()
    vis_calc_process.start()
    capture_process.start()
    print 'Process chain started.'

    post_process.join()
    vis_calc_process.join()
    capture_process.join()
    print 'Process chain finished.'


    if ARGS.mode == 'qt':
        QtGui.QApplication.instance().exec_()

