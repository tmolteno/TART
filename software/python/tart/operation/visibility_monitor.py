'''
    Main loop for visibility capture.
'''
import io
import numpy as np
import time

import multiprocessing
import logging
import traceback

import sys
import os, stat
import datetime


'''
    Grab data and push to queue
'''
def capture_loop(process_queue, stack_N, sec, res, loc):
    while True:
        try:
            # Add the data to the process queue
            data = 'foo'
            READ DATA HERE...
            process_queue.put(data)
            
        except Exception, e:
            logger.error( "Capture Loop Error %s" % str(e))
            logger.error(traceback.format_exc())
            
        finally:
            camera.close()
        


''' This is a separate procees that just
    waits for data to appear on the queue, and processes
    the files.
'''
def process_loop(process_queue):

    while (True):
        if (False == process_queue.empty()):
            try:
                stack_filename = process_queue.get()
                detect_meteor.analyse(stack_filename)
            except Exception, e:
                logger.error( "Measurement Processing Error %s" % str(e))
                logger.error(traceback.format_exc())
        time.sleep(0.5)




import logging.config
import yaml
logger = logging.getLogger(__name__)

if __name__=="__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--num_frames", type=int, default=2, help="Number of frames to stack")    
    parser.add_argument("--exp", type=float, default=2.0, help="Number of seconds exposure per frame")    
    parser.add_argument("--debug", action="store_true", help="Run regardless of sun angle")
    parser.add_argument("--v2", action="store_true", help="Use a version 2.0 camera")
    parser.add_argument("--data-dir", default='.', help="Root directory to store data")

    args = parser.parse_args()


    path = 'logging.yaml'
    if os.path.exists(path):
        with open(path, 'rt') as f:
            config = yaml.load(f.read())
        logging.config.dictConfig(config)

    proc_queue = multiprocessing.Queue()
    
    detect_process = multiprocessing.Process(target=process_loop, \
                                             args=(proc_queue,))
    detect_process.start()
   
    sec = args.exp
    n_frames = args.num_frames

    capture_loop(proc_queue, n_frames, sec, resolution, location.Dunedin, args.debug)
