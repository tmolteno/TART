import argparse
import numpy as np
import datetime
import time
import sys
import os, stat

import multiprocessing
import logging
import traceback

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
  return vis

def get_data(tart):
  viz = tart.vis_read(False)
  return viz[tart.perm]

'''
    Grab data and push to queue
'''
def capture_loop(process_queue, tart_instance, ):
    while True:
        try:
            # Add the data to the process queue
            data = get_data(tart_instance)

            #data = 'foo'
            #READ DATA HERE...
            process_queue.put(data)
        except Exception, e:
            logger.error( "Capture Loop Error %s" % str(e))
            logger.error(traceback.format_exc())

        finally:
            print data[0], 'ok'
            #tart_instance.close()



''' This is a separate procees that just
    waits for data to appear on the queue, and processes
    the files.
'''
def process_loop(process_queue, config, n_samples, chunck_size):
    vislist = []
    while (True):
        if (False == process_queue.empty()):
            try:
                data = process_queue.get()
                vis = get_vis_object(data, n_samples, config)
                vislist.append(vis)
                if len(vislist)>chunck_size:
                  print vis
                  visibility.Visibility_Save(vislist, "%s_%02i_%02i_%02i.vis" %(ARGS.vis_prefix, vis.timestamp.hour, vis.timestamp.minute, vis.timestamp.second))
                  vislist = []
            except Exception, e:
                logger.error( "Measurement Processing Error %s" % str(e))
                logger.error(traceback.format_exc())
        time.sleep(0.01)

import logging.config
import yaml
logger = logging.getLogger(__name__)

if __name__=="__main__":
    import argparse
    PARSER = argparse.ArgumentParser()
    PARSER.add_argument('--config', required=False, help="Config file.")
    PARSER.add_argument('--save_vis', required=False, action='store_true', help="generate abs and angle for vis")
    PARSER.add_argument('--vis_prefix', required=False, type=str, default='vis', help="generate abs and angle for vis")
    PARSER.add_argument('--blocksize', default=23, type=int, help='exponent of correlator block-size')
    PARSER.add_argument('--chuncksize', default=10, type=int, help='number of vis objects per file')

    ARGS = PARSER.parse_args()

    config = settings.Settings(ARGS.config)
    blocksize = ARGS.blocksize
    n_samples = 2**blocksize
    chunck_size = ARGS.chuncksize

    path = 'logging.yaml'
    if os.path.exists(path):
        with open(path, 'rt') as f:
            log_config = yaml.load(f.read())
        logging.config.dictConfig(log_config)

    proc_queue = multiprocessing.Queue()

    detect_process = multiprocessing.Process(target=process_loop, args=(proc_queue, config, n_samples, chunck_size))
    detect_process.start()

    tart_instance = TartSPI()
    tart_instance.reset()
    tart_instance.read_status(True)
    permutation = tart_instance.load_permute()
    tart_instance.debug(on=False, shift=False, count=False, noisy=True)
    tart_instance.read_status(True)
    tart_instance.start(blocksize, True)

    capture_loop(proc_queue, tart_instance,)


