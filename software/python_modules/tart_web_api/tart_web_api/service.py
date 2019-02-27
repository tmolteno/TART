'''
    Services for the TART web api. These are background processes that 
    capture visibilities e.t.c.
    
    Author. Max Scheel 2017
            Tim Molteno 2018-2019

'''

import logging
import sys
#logging.basicConfig(level=logging.INFO)

import datetime
import dateutil.parser
import os
import stat
import time
import traceback
import json
import requests

import numpy as np
from numpy import concatenate as cc
from scipy.optimize import minimize

import tart_web_api.database as db

from tart_hardware_interface.tartspi import TartSPI
from tart_web_api.tart_dummy_spi import TartDummySPI

from tart_hardware_interface.highlevel_modes_api import *
from tart_hardware_interface.stream_vis import *

from tart.operation import settings
from tart.imaging import visibility
from tart.imaging import calibration
from tart.imaging import antenna_model
from tart.imaging import radio_source
from tart.imaging import correlator
from tart.imaging import location
from tart.simulation import skymodel
from tart.simulation import antennas
from tart.simulation import radio
from tart.simulation.simulator import get_vis_parallel, get_vis

msd_vis = []
sim_vis = []
N_IT = 0

def mkdirp(directory):
    if not os.path.isdir(directory):
        os.makedirs(directory) 

def optimize_phaseNgain(opt_parameters):
    """ Set phase offsets in list of calibrated visibility objects.
        Calculate and return likelihood
    """
    global msd_vis, sim_vis, N_IT
    gains, phase_offsets = opt_parameters[:23], opt_parameters[23:]
    for key in list(msd_vis.keys()):
        ant_idxs = list(range(1, 24))
        for i, g in enumerate(gains):
            if g < 0:
                gains[i] = -g
                phase_offsets[i] += np.pi
        msd_vis[key].set_phase_offset(ant_idxs, phase_offsets)
        msd_vis[key].set_gain(ant_idxs, gains)
    ret = likelihood_vis_dicts(msd_vis, sim_vis, debug=False)
    N_IT += 1
    if N_IT%20 == 0:
        db.update_calibration_process_state(str(ret))
        print(ret) #, phase_offsets, gains
    return ret

def vis_diff(vis1, vis2, ant_a, ant_b):
    """ Calculate difference of complex visibilities vis1 and vis2 for baseline ant_a and ant_b"""
    ret = np.power(np.abs(vis1.get_visibility(ant_a, ant_b)-vis2.get_visibility(ant_a, ant_b)), 2)
    return ret

def likelihood_vis_dicts(msd_vis, sim_vis, debug=False):
    """Calculate likelihood from lists of measured and simulated visibilities"""
    ret = 0.
    for key in list(msd_vis.keys()):
        meas_v = msd_vis[key]
        sim_v = sim_vis[key]
        bls_meas = meas_v.get_baselines() # flagging based on measured visibilities
        if debug:
            logging.info( 'Number of baselines contributing: {}'.format(len(bls_meas)))
        for bln in bls_meas:
            [ant_i, ant_j] = bln
            ret += vis_diff(meas_v, sim_v, ant_i, ant_j)*np.abs(sim_v.get_visibility(ant_i, ant_j))
    return ret

def vis_object_from_response(vis, ts, SETTINGS):
    ret = visibility.Visibility(SETTINGS, ts)
    v_order = []
    bl_order = []
    for v in vis:
        v_order.append(complex(v['re'], v['im']))
        bl_order.append([v['i'], v['j']])
    ret.set_visibilities(v_order, bl_order)
    return ret


def calibrate_from_vis(cal_measurements, runtime_config):
    MODE = 'mini'
    #simp (use only when noise present)\
    #mini, perfect visibilities.\
    #full

    SETTINGS = settings.from_file(runtime_config['telescope_config_path'])
    loc = location.get_loc(SETTINGS)
    
    ANTS = [antennas.Antenna(loc, pos)
            for pos in runtime_config['antenna_positions']]
    ANT_MODELS = [antenna_model.GpsPatchAntenna() for i in range(SETTINGS.get_num_antenna())]
    NOISE_LVLS = 0.0 * np.ones(SETTINGS.get_num_antenna())
    RAD = radio.Max2769B(n_samples=2**12, noise_level=NOISE_LVLS)
    COR = correlator.Correlator()

    global msd_vis, sim_vis
    sim_vis = {}
    msd_vis = {}
    sim_sky = {}

    for m in cal_measurements:
        timestamp = dateutil.parser.parse(m['data']['timestamp'])

        key = '%f,%f,%s' % (m['el'], m['az'], timestamp)
        # Generate model visibilities according to specified point source positions
        sim_sky[key] = skymodel.Skymodel(0, location=loc,
                                         gps=0, thesun=0, known_cosmic=0)
        sim_sky[key].add_src(radio_source.ArtificialSource(loc, timestamp, r=100.0, el=m['el'], az=m['az']))
        v_sim = get_vis(sim_sky[key], COR, RAD, ANTS, ANT_MODELS, SETTINGS, timestamp, mode=MODE)
        sim_vis[key] = calibration.CalibratedVisibility(v_sim)

        # Construct (un)calibrated visibility objects from received measured visibilities
        vis = vis_object_from_response(m['data']['vis']['data'], timestamp, SETTINGS)
        msd_vis[key] = calibration.CalibratedVisibility(vis)

    # Define initial optimisation paramters

    opt_param = np.ones(46)
    opt_param[:23] = np.ones(23)
    opt_param[23:] = np.zeros(23)

    # Run optimisation
    RES = minimize(optimize_phaseNgain, opt_param)

    utc_date = datetime.datetime.utcnow()
    phases = list(msd_vis.values())[0].get_phase_offset(np.arange(24))
    gains = list(msd_vis.values())[0].get_gain(np.arange(24))

    #content =  msd_vis.values()[0].to_json(filename='/dev/null')
    db.insert_gain(utc_date, gains, phases)
    db.update_calibration_process_state('idle')
    return {}

def cleanup_observation_cache():
    while True:
        resp = db.get_raw_file_handle()
        if len(resp) > 10:
            for entry in resp[10:]:

                try:
                    db.remove_raw_file_handle_by_Id(entry['Id'])
                    print('removed', entry['Id'], entry['filename'], entry['checksum'])
                except:
                    print('couldnt remove handle from database')
                    pass
                try:
                    os.remove(entry['filename'])
                except:
                    print('couldnt remove file')
                    pass
        else:
            db.update_observation_cache_process_state('OK')
        time.sleep(60)

def cleanup_visibility_cache():
    while True:
        resp = db.get_vis_file_handle()
        if len(resp) > 10:
            for entry in resp[10:]:
                try:
                    db.remove_vis_file_handle_by_Id(entry['Id'])
                    print('removed', entry['Id'], entry['filename'], entry['checksum'])
                except:
                    print('couldnt remove handle from database')
                    pass
                try:
                    os.remove(entry['filename'])
                except:
                    print('couldnt remove file')
                    pass

        else:
            db.update_vis_cache_process_state('OK')
        time.sleep(60)

def create_direct_vis_dict(vis):
    vis_dict = {}
    vis_list = []
    for (b, v) in zip(vis.baselines, vis.v):
        i, j = b
        vis_el = {'i': i, 'j': j, 're': v.real, 'im':v.imag}
        vis_list.append(vis_el)
    vis_dict = {'data':vis_list, 'timestamp':vis.timestamp.isoformat()[:-3]+'Z'}
    return vis_dict

class TartControl():
    ''' High Level TART Interface'''
    def __init__(self, runtime_config):
        try:
            self.TartSPI = TartSPI()
        except Exception as e:
            logging.exception(e)
            logging.warn('USING DUMMY SPI MODULE.')

            self.TartSPI = TartDummySPI()

        self.config = runtime_config
        self.state = 'off'
        self.queue_vis = None
        self.process_vis_calc = None
        self.cmd_queue_vis_calc = None
        self.process_capture = None
        self.cmd_queue_capture = None
        self.vislist = []
        mkdirp(self.config['vis']['base_path'])
        mkdirp(self.config['raw']['base_path'])


    def run(self):
        if self.state == 'diag':
            run_diagnostic(self.TartSPI, self.config)
            db.insert_sample_delay(self.config['channels_timestamp'], self.config['sample_delay'])

        elif self.state == 'raw':
            ret = run_acquire_raw(self.TartSPI, self.config)
            if 'filename' in ret:
                db.insert_raw_file_handle(ret['filename'], ret['sha256'])

        elif self.state == 'vis':
            if self.queue_vis is None:
                logging.info("vis_stream_setup")
                self.vis_stream_setup()
            else:
                ret = self.vis_stream_acquire()
                if 'filename' in ret:
                    logging.info("vis_stream_acquire = {}".format(ret))
                    db.insert_vis_file_handle(ret['filename'], ret['sha256'])
                time.sleep(0.005)
        elif self.state == 'off':
            time.sleep(0.5)
        else:
            logging.error('unknown state: {}'.format(self.state))

    def set_state(self, new_state):
        if new_state == self.state:
            return
        else:
            ''' State Transition '''
            if self.state == 'vis':
                '''Cleanup vis acquisition queues and processes'''
                self.vis_stream_finish()
            self.state = new_state

    def vis_stream_setup(self):
        self.queue_vis,\
        self.process_vis_calc,\
        self.process_capture,\
        self.cmd_queue_vis_calc,\
        self.cmd_queue_capture,\
        = stream_vis_to_queue(self.TartSPI, self.config)

    def vis_stream_acquire(self):
        ''' Get all available visibities'''
        ret = {}

        while self.queue_vis.qsize() > 0:
            vis, means = self.queue_vis.get()
            #logging.info('vis = {}, means={}.'.format(vis, means))

            if vis is not None:
                self.config['vis_current'] = create_direct_vis_dict(vis)
                self.vislist.append(vis)
                logging.info('Updated visibilities N={}.'.format(len(self.vislist)))
                
                chunksize = self.config['vis']['chunksize']
                if len(self.vislist) >= chunksize:
                    logging.info('reached chunksize of {}'.format(chunksize))
                    if self.config['vis']['save'] == 1:
                        fname = "{}/fpga_{}.vis".format(self.config['vis']['base_path'], 
                                                   vis.timestamp.strftime('%Y-%m-%d_%H_%M_%S.%f'))
                        #fname = self.config['vis']['base_path'] + "_" + 
                            #vis.timestamp.strftime('%Y-%m-%d_%H_%M_%S.%f')+".vis"
                        visibility.Visibility_Save(self.vislist, fname)
                        logging.info("saved to {}".format(vis, fname))
                        ret['filename'] = fname
                        ret['sha256'] = sha256_checksum(fname)
                    self.vislist = []
        return ret


    def vis_stream_finish(self):
        self.cmd_queue_capture.put('stop')
        self.cmd_queue_vis_calc.put('stop')
        self.process_capture.join()
        self.process_vis_calc.join()
        self.queue_vis = None
        self.vislist = []
        logging.info('Stop visibility acquisition processes.')

    def vis_stream_reconfigure(self):
        self.vis_stream_finish()
