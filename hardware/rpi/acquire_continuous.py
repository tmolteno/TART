import argparse
#import numpy as np
#import datetime

import os, stat

import multiprocessing
#import traceback
import logging.config
import yaml
logger = logging.getLogger(__name__)

from tart_dsp.tartspi import TartSPI
from tart_dsp.highlevel_modes_api import *
from tart_dsp.stream_vis import *

from tart.operation import settings
#from tart.imaging import visibility
#from tart.imaging import calibration
#from tart.util import angle


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


    # Import result_loop


    #from monitor_vis import gen_calib_image


    runtime_config = {}
    #runtime_config['mode'] = ARGS.mode
    runtime_config['mode'] = 'qt'
    runtime_config['blocksize'] = ARGS.blocksize
    runtime_config['telescope_config_path'] = ARGS.config
    runtime_config['calibration_dir'] = ARGS.calibration_dir
    runtime_config['chunksize'] = 2

    tart_instance = TartSPI()

#    post_process = multiprocessing.Process(target=vis_to_latest_image, args=(vis_queue,))
 #   post_process.start()

    #vis_calc_process.start()
    #capture_process.start()



    #if ARGS.mode == 'save':
        #vis_calc_process = multiprocessing.Process(target=process_loop, args=(proc_queue, result_queue, settings.Settings(ARGS.config), ARGS.blocksize, None))
        #from save_vis import result_loop
        #post_process = multiprocessing.Process(target=result_loop, args=(result_queue, ARGS.chunksize,ARGS.vis_prefix,logger))
        #capture_process = multiprocessing.Process(target=capture_loop, args=(tart_instance, proc_queue, ARGS.blocksize))


    #elif ARGS.mode == 'save_single':
        #vis_calc_process = multiprocessing.Process(target=process_loop, args=(proc_queue, result_queue, settings.Settings(ARGS.config), ARGS.blocksize, ARGS.chunksize))
        #from save_vis import result_loop
        #post_process = multiprocessing.Process(target=result_loop, args=(result_queue, ARGS.chunksize,ARGS.vis_prefix,logger,True))
        #capture_process = multiprocessing.Process(target=capture_loop, args=(tart_instance, proc_queue, ARGS.blocksize, ARGS.chunksize))

    if ARGS.mode == 'rt_syn_img':
        vis_to_latest_image(tart_instance, runtime_config)

    elif ARGS.mode == 'qt':
        vis_queue, vis_calc_process, capture_process vis_calc_cmd_q, capture_cmd_q = stream_vis_to_queue(tart_instance, runtime_config)
        from pyqtgraph.Qt import QtGui, QtCore
        import qt_view
        print 'importing alternative result_loop'
        from monitor_vis import result_loop

        app = QtGui.QApplication([])
        plotter = qt_view.QtPlotter(app)
        plotQ = plotter.getPort()
        post_process = multiprocessing.Process(target=result_loop, args=(vis_queue, runtime_config['chunksize'], runtime_config['mode'], plotQ, runtime_config['calibration_dir']))
        post_process.start()
        QtGui.QApplication.instance().exec_()


    # Start processes in reverse order of chain
    #vis_calc_process.start()
    #capture_process.start()
    #print 'Process chain started.'

    #if ARGS.mode == 'save_single':
        #post_process.join()
        #vis_calc_process.join()
        #capture_process.join()
        #print 'Process chain finished.'


    #if ARGS.mode == 'qt':
        #QtGui.QApplication.instance().exec_()

