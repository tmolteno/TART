import numpy as np
import datetime

import os, stat

import multiprocessing
import traceback
import logging.config
import yaml
logger = logging.getLogger(__name__)

from tart.operation import settings
from tart.imaging import visibility
from tart.imaging import correlator
from tart.util import angle

from highlevel_modes_api import get_status_json

def get_corr(xnor_sum, n_samples):
    return 2*xnor_sum/float(n_samples)-1

def get_vis_object(data, runtime_config):
    n_samples = 2**runtime_config['vis']['N_samples_exp']
    timestamp = datetime.datetime.utcnow()
    config = settings.from_file(runtime_config['telescope_config_path'])
    num_ant = config.get_num_antenna()
    v = []
    baselines = []
    xnor_cos = data[0:-24:2]
    xnor_sin = data[1:-24:2]
    corr_cos_i_cos_j = get_corr(xnor_cos, n_samples)
    corr_cos_i_sin_j = get_corr(xnor_sin, n_samples)
    means = (data[-24:])/float(n_samples)*2.-1
    #print(means)
    for i in range(0, num_ant):
        for j in range(i+1, num_ant):
            idx = len(baselines)
            baselines.append([i,j])
            v_real = correlator.van_vleck_correction((-means[i]*means[j]) + corr_cos_i_cos_j[idx])
            v_imag = correlator.van_vleck_correction((-means[i]*means[j]) + corr_cos_i_sin_j[idx])
            #v_real = (-means[i]*means[j]) + corr_cos_i_cos_j[idx]
            #v_imag = (-means[i]*means[j]) + corr_cos_i_sin_j[idx]
            v_com = correlator.combine_real_imag(v_real,v_imag)
            v.append(v_com)
    vis = visibility.Visibility(config, timestamp)
    vis.set_visibilities(v, baselines)
    return vis, means, timestamp

def get_data(tart):
    viz = tart.vis_read(False)
    return viz[tart.perm]

def capture_loop(tart, process_queue, cmd_queue, runtime_config, logger=None,):
    tart.reset()
    tart.read_status(True)
    permutation = tart.load_permute()
    tart.debug(on=False, shift=False, count=False, noisy=True)
    tart.read_status(True)
    tart.capture(on=True, noisy=False)
    tart.set_sample_delay(runtime_config['sample_delay'])
    tart.start(runtime_config['vis']['N_samples_exp'], True)
    active = 1
    while active:
        try:
            if (cmd_queue.empty() == False):
                cmd = cmd_queue.get()
                if cmd == 'stop':
                    active = 0
            # Add the data to the process queue
            data = get_data(tart)
            d, d_json = get_status_json(tart)
            runtime_config['status'] = d
            process_queue.put(data)
            print( 'Capture Loop: Acquired', data[0])
        except Exception, e:
            logger.error( "Capture Loop Error %s" % str(e))
            logger.error(traceback.format_exc())
    print('Done acquisition. Closing Capture Loop.')
    return 1

def update_means(means,ts,runtime_config):
    channels = []
    for i in range(runtime_config['telescope_config']['num_antenna']):
        channel = {}
        for key in runtime_config['channels'][i]:
            channel[key] = runtime_config['channels'][i][key]
        channel['radio_mean'] = means[i]
        channels.append(channel)
    runtime_config['channels'] = channels
    runtime_config['channels_timestamp'] = ts

def process_loop(process_queue, vis_queue, cmd_queue, runtime_config, logger=None):
    active = 1
    while active:
        try:
            time.sleep(0.01)
            if (cmd_queue.empty() == False):
                cmd = cmd_queue.get()
                if cmd == 'stop':
                    active = 0
            if (process_queue.empty() == False):
                #print('Status: ProcessQ: %i ResultQ: %i' % (process_queue.qsize(), vis_queue.qsize()))
                data = process_queue.get()
                vis, means, timestamp = get_vis_object(data, runtime_config)
                #print(vis, means, timestamp)
                #update_means(means, timestamp, runtime_config)
                #print(means)
                #print( 'Process Loop:', # vis)
                vis_queue.put((vis, means))
        except Exception, e:
            logger.error( "Processing Error %s" % str(e))
            logger.error(traceback.format_exc())
    print('process_loop finished')
    return 1

def stream_vis_to_queue(tart, runtime_config):
    # tart
    # >> [2x visibility and mean readout]
    # >> raw_data_queue >> [visibility assembly]
    # >> vis_queue

    # Send data to each process
    raw_data_queue = multiprocessing.Queue()
    vis_queue = multiprocessing.Queue()

    # Send commands to each process
    capture_cmd_queue = multiprocessing.Queue()
    vis_calc_cmd_queue = multiprocessing.Queue()

    capture_process = multiprocessing.Process(target=capture_loop,\
      args=(tart, raw_data_queue, capture_cmd_queue, runtime_config, logger))
    vis_calc_process = multiprocessing.Process(target=process_loop,\
      args=(raw_data_queue, vis_queue, vis_calc_cmd_queue, runtime_config, logger))

    vis_calc_process.start()
    capture_process.start()
    return vis_queue, vis_calc_process, capture_process, vis_calc_cmd_queue, capture_cmd_queue


from PIL import Image
import time
from tart.imaging import calibration
from tart.imaging import synthesis

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
            #print('loading.. calibration')
            cv = calibration.from_JSON_file(vis, cal_file)
            CAL_MEASURE_VIS_LIST.append(cv)
    CAL_SYN = synthesis.Synthesis_Imaging(CAL_MEASURE_VIS_LIST)
    CAL_SYN.set_grid_file(calibration_dir + 'monitor_vis_grid.idx')
    #CAL_IFT, CAL_EXTENT = CAL_SYN.get_ift(nw=20, num_bin=2**7, use_kernel=False)
    CAL_IFT, CAL_EXTENT = CAL_SYN.get_ift_simp(nw=20, num_bin=2**7)
    return CAL_IFT, CAL_EXTENT


def vis_to_latest_image(tart_instance, runtime_config,):
    '''take vis off queue and synthesize and image'''
    vis_q, vis_calc_p, capture_p, vis_calc_cmd_q, capture_cmd_q = stream_vis_to_queue(tart_instance, runtime_config)
    vis = None
    time.sleep(0.05)
    while vis_q.qsize()>0:
        vis, means = vis_q.get()
        #print('here', vis)
    if vis is not None:
        res, ex = gen_calib_image([vis,], runtime_config['calibration_dir'])
        res = np.abs(res)
        rescaled = (255.0 / res.max() * (res - res.min())).astype(np.uint8)
        print(rescaled, 'shape', np.shape(rescaled))
        im = Image.fromarray(rescaled)
        im.save(runtime_config['realtime_image_path'])
        print('saved file')
