
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

def capture_loop(tart, process_queue, cmd_queue, runtime_config, logger=None,):

    tart.reset()
    tart.read_status(True)
    permutation = tart.load_permute()
    tart.debug(on=False, shift=False, count=False, noisy=True)
    tart.read_status(True)
    tart.capture(on=True, noisy=False)
    tart.start(runtime_config['blocksize'], True)

    active = 1
    while active:
        try:
            time.sleep(0.01)
            if (cmd_queue.empty() == False):
                cmd = cmd_queue.get()
                if cmd == 'stop':
                    active = 0
                    #break
            # Add the data to the process queue
            data = get_data(tart)
            process_queue.put(data)
            #if chunksize is not None:
                #num_vis += 1
                #if num_vis == chunksize:
                    #active = False
            print  'Capture Loop: Acquired', data[0]
        except Exception, e:
            logger.error( "Capture Loop Error %s" % str(e))
            logger.error(traceback.format_exc())

        #finally:
    #print 'Done acquisition. Closing Capture.'
    print 'capture_loop finished'
    return 1


def process_loop(process_queue, vis_queue, cmd_queue, runtime_config, logger=None):
    print_int = 0
    n_samples = 2**runtime_config['blocksize']
    config = settings.Settings(runtime_config['telescope_config_path'])
    active = 1
    while active:
        try:
            time.sleep(0.01)
            if (cmd_queue.empty() == False):
                cmd = cmd_queue.get()
                if cmd == 'stop':
                    active = 0
                    #break
            if (process_queue.empty() == False):
                    print_int += 1
                    if print_int >10:
                        print_int = 0
                        print 'Status: ProcessQ: %i ResultQ: %i' % (process_queue.qsize(), vis_queue.qsize())
                        #print '!!!!!!!!!!!!!!!!!!!!!! dropping frames when displaying  !!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                    data = process_queue.get()
                    vis, means = get_vis_object(data, n_samples, config)
                    #print means
                    vis_queue.put((vis, means))
                    print  'Process Loop:', vis
        except Exception, e:
            logger.error( "Processing Error %s" % str(e))
            logger.error(traceback.format_exc())
            #finally:

    print 'process_loop finished'
    return 1


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
              print 'fmin'
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


def vis_to_latest_image(tart_instance, runtime_config,):
    '''take vis off queue and synthesize and image'''
    vis_q, vis_calc_p, capture_p, vis_calc_cmd_q, capture_cmd_q = stream_vis_to_queue(tart_instance, runtime_config)
    while (runtime_config['mode'] =='rt_syn_img'):
        vis = None
        time.sleep(0.05)
        while vis_q.qsize()>0:
            vis, means = vis_q.get()
            #print 'here', vis
        if vis is not None:
            res, ex = gen_calib_image([vis,], runtime_config['calibration_dir'])
            res = np.abs(res)
            rescaled = (255.0 / res.max() * (res - res.min())).astype(np.uint8)
            print rescaled, 'shape', np.shape(rescaled)
            im = Image.fromarray(rescaled)
            im.save(runtime_config['realtime_image_path'])
            print 'saved file'

            if runtime_config.has_key('loop_mode'):
                if runtime_config['loop_mode']=='loop_n':
                    runtime_config['loop_idx'] += 1
                    print runtime_config['loop_idx']
                    if runtime_config['loop_idx'] == runtime_config['loop_n']:
                        runtime_config['loop_idx'] = 0
                        runtime_config['mode'] = 'off'

        else:
          time.sleep(0.05)
    print 'stopping'
    capture_cmd_q.put('stop')
    vis_calc_cmd_q.put('stop')
    vis_calc_p.join()
    capture_p.join()
    print 'stopped'



def vis_to_dict(tart_instance, runtime_config,):
    '''take vis off queue and synthesize and image'''
    vis_q, vis_calc_p, capture_p, vis_calc_cmd_q, capture_cmd_q = stream_vis_to_queue(tart_instance, runtime_config)
    while (runtime_config['mode'] =='vis'):
        vis = None
        time.sleep(0.05)
        while vis_q.qsize()>0:
            vis, means = vis_q.get()
            #print 'here', vis
        if vis is not None:
            d = {}
            for (b, v) in zip(vis.baselines, vis.v):
                key = str(b)
                d[key] = (v.real, v.imag)
            print 'here'
            runtime_config['vis_current'] = d
            runtime_config['vis_antenna_positions'] = vis.config.ant_positions
            runtime_config['vis_timestamp'] = vis.timestamp

            if runtime_config.has_key('loop_mode'):
                if runtime_config['loop_mode']=='loop_n':
                    runtime_config['loop_idx'] += 1
                    print runtime_config['loop_idx']
                    if runtime_config['loop_idx'] == runtime_config['loop_n']:
                        runtime_config['loop_idx'] = 0
                        runtime_config['mode'] = 'off'

        else:
          time.sleep(0.05)
    print 'stopping'
    capture_cmd_q.put('stop')
    vis_calc_cmd_q.put('stop')
    vis_calc_p.join()
    capture_p.join()
    print 'stopped'




