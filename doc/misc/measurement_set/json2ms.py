'''
    A quick attempt to get TART JSON data into a measurement set.
    Author: Tim Molteno, tim@elec.ac.nz
    Copyright (c) 2019.

    Official Documentation is Here.
        https://casa.nrao.edu/Memos/229.html#SECTION00044000000000000000

    License. GPLv3.
'''
import os
import json
import logging
import dask
import argparse

import numpy as np
import dask.array as da
from itertools import product


from tart.operation import settings

from tart_tools import api_handler
from tart_tools import api_imaging
from tart.imaging import elaz
from tart.util import constants


import pyrap.tables as pt
from numpy.testing import assert_array_equal

from daskms import Dataset, xds_to_table

logger = logging.getLogger()

'''
The following from Oleg Smirnov.

>
> * We have a single circular polarization that I've labelled 1 (from FITS scheme I googled somewhere. Could use RR I guess).

Ah, my favourite bit of the MS documentation! According to
https://casa.nrao.edu/Memos/229.html#SECTION000613000000000000000, we
have CORR_TYPE: An integer for each correlation product indicating the
Stokes type as defined in the Stokes class enumeration.

And where does a naive user find this mysterious "Stokes class
enumeration"? Why, it's in the C++ source code itself! Voila:
https://casa.nrao.edu/active/docs/doxygen/html/classcasa_1_1Stokes.html.
And they don't even give you the numbers! You have to use your fingers
to count them off, old school. And how were you supposed to know about
this? I have no idea -- the only reason I know about this is because I
worked with the AIPS++ codebase back in the day...

Anyway, here's a Python list version of the enumeration, if it helps:
'''
MS_STOKES_ENUMS = {
            "Undefined": 0, 
            "I": 1, 
            "Q": 2, 
            "U": 3, 
            "V": 4, 
            "RR": 5, 
            "RL": 6, 
            "LR": 7, 
            "LL": 8,
            "XX": 9, 
            "XY": 10, 
            "YX": 11, 
            "YY": 12, 
            "RX": 13, 
            "RY": 14, 
            "LX": 15, 
            "LY": 16, 
            "XR": 17, 
            "XL": 18, 
            "YR": 19,
            "YL": 20,
            "PP": 21, 
            "PQ": 22, 
            "QP": 23, 
            "QQ": 24, 
            "RCircular": 25, 
            "LCircular": 26, 
            "Linear": 27,
            "Ptotal": 28, 
            "Plinear": 29, 
            "PFtotal": 30, 
            "PFlinear": 31, 
            "Pangle": 32}



def ms_create(ms_table_name, info, ant_pos, cal_vis, timestamps, corr_types, sources):
    '''    "info": {
        "info": {
            "L0_frequency": 1571328000.0,
            "bandwidth": 2500000.0,
            "baseband_frequency": 4092000.0,
            "location": {
                "alt": 270.0,
                "lat": -45.85177,
                "lon": 170.5456
            },
            "name": "Signal Hill - Dunedin",
            "num_antenna": 24,
            "operating_frequency": 1575420000.0,
            "sampling_frequency": 16368000.0
        }
    },
    '''
    
    num_chans = [1]

    rs = np.random.RandomState(42)

    ant_table_name = "::".join((ms_table_name, "ANTENNA"))
    feed_table_name = "::".join((ms_table_name, "FEED"))
    field_table_name = "::".join((ms_table_name, "FIELD"))
    obs_table_name = "::".join((ms_table_name, "OBSERVATION"))
    ddid_table_name = "::".join((ms_table_name, "DATA_DESCRIPTION"))
    pol_table_name = "::".join((ms_table_name, "POLARIZATION"))
    spw_table_name = "::".join((ms_table_name, "SPECTRAL_WINDOW"))
    # SOURCE is an optional MS sub-table
    src_table_name = "::".join((ms_table_name, "SOURCE"))

    ms_datasets = []
    ant_datasets = []
    feed_datasets = []
    field_datasets = []
    obs_datasets = []
    ddid_datasets = []
    pol_datasets = []
    spw_datasets = []
    src_datasets = []

    # Create ANTENNA dataset
    # Each column in the ANTENNA has a fixed shape so we
    # can represent all rows with one dataset
    na = len(ant_pos)
    position =  da.asarray(ant_pos) # da.zeros((na, 3))
    diameter = da.ones(na) * 0.025
    offset = da.zeros((na, 3))
    names = np.array(['ANTENNA-%d' % i for i in range(na)], dtype=np.object)
    stations = np.array([info['name'] for i in range(na)], dtype=np.object)
    
    ds = Dataset({
        'POSITION': (("row", "xyz"), position),
        'OFFSET': (("row", "xyz"), offset),
        'DISH_DIAMETER': (("row",), diameter),
        'NAME': (("row",), da.from_array(names, chunks=na)),
        'STATION': (("row",), da.from_array(stations, chunks=na)),
    })
    ant_datasets.append(ds)

    #########################################  Create a FEED dataset. #######################################
    # There is one feed per antenna, so this should be quite similar to the ANTENNA
    antenna_ids = da.asarray(range(na))
    feed_ids = da.zeros(na)
    num_receptors = da.ones(na)
    polarization_types = np.array([['XX'] for i in range(na)], dtype=np.object)
    receptor_angles = np.array([[0.0] for i in range(na)])
    pol_response = np.array([[[0.0 + 1.0j, 1.0 - 1.0j]] for i in range(na)])
    
    beam_offset = np.array([[[0.0, 0.0]] for i in range(na)])
    
    
    ds = Dataset({
        'ANTENNA_ID': (("row",), antenna_ids),
        'FEED_ID': (("row",), feed_ids),
        'NUM_RECEPTORS': (("row",), num_receptors),
        'POLARIZATION_TYPE': (("row", "receptors",), da.from_array(polarization_types, chunks=na)),
        'RECEPTOR_ANGLE': (("row", "receptors",), da.from_array(receptor_angles, chunks=na)),
        'POL_RESPONSE': (("row", "receptors", "receptors-2"), da.from_array(pol_response, chunks=na)),
        'BEAM_OFFSET': (("row", "receptors", "radec"), da.from_array(beam_offset, chunks=na)),
    })
    feed_datasets.append(ds)


    ########################################### FIELD dataset ################################################
    direction = [ [np.radians(90.0), np.radians(0.0)]]   ## Phase Center in J2000
    field_direction = da.asarray(direction)[None, :]
    field_name = da.asarray(np.asarray(['up'], dtype=np.object))
    field_num_poly = da.zeros(1) # Zero order polynomial in time for phase center.
    
    dir_dims = ("row",'field-poly', 'field-dir',)

    ds = Dataset({
        'PHASE_DIR': (dir_dims, field_direction),
        'DELAY_DIR': (dir_dims, field_direction),
        'REFERENCE_DIR': (dir_dims, field_direction),
        'NUM_POLY': (("row", ), field_num_poly),
        'NAME': (("row", ), field_name),
    })
    field_datasets.append(ds)

   ########################################### OBSERVATION dataset ################################################

    ds = Dataset({
        'TELESCOPE_NAME': (("row",), da.asarray(np.asarray(['TART'], dtype=np.object)) ),
        'OBSERVER': (("row",), da.asarray(np.asarray(['Tim'], dtype=np.object)) ),
    })
    obs_datasets.append(ds)

        
    #################################### Create SOURCE datasets #############################################
    for s, src in enumerate(sources):
        name = src['name']
        rest_freq = [info['operating_frequency']]
        direction = [np.radians(src['el']), np.radians(src['az'])]   ## FIXME these are in elevation and azimuth. Not in J2000.
        
        #logger.info("SOURCE: {}, timestamp: {}".format(name, timestamps))
        dask_num_lines = da.full((1,), len(rest_freq), dtype=np.int32)
        dask_direction = da.asarray(direction)[None, :]
        dask_rest_freq = da.asarray(rest_freq)[None, :]
        dask_name = da.asarray(np.asarray([name], dtype=np.object))
        dask_time = da.asarray(np.asarray([timestamps], dtype=np.object))
        ds = Dataset({
            "NUM_LINES": (("row",), dask_num_lines),
            "NAME": (("row",), dask_name),
            #"TIME": (("row",), dask_time), # FIXME. Causes an error. Need to sort out TIME data fields
            #"REST_FREQUENCY": (("row", "line"), dask_rest_freq),
            "DIRECTION": (("row", "dir"), dask_direction),
            })
        src_datasets.append(ds)

    # Create POLARISATION datasets.
    # Dataset per output row required because column shapes are variable
    for r, corr_type in enumerate(corr_types):
        dask_num_corr = da.full((1,), len(corr_type), dtype=np.int32)
        dask_corr_type = da.from_array(corr_type,
                                       chunks=len(corr_type))[None, :]
        dask_corr_type = da.from_array(corr_type,
                                       chunks=len(corr_type))[None, :]
        ds = Dataset({
            "NUM_CORR": (("row",), dask_num_corr),
            #"CORR_PRODUCT": (("row",), dask_num_corr),
            "CORR_TYPE": (("row", "corr"), dask_corr_type),
        })

        pol_datasets.append(ds)

    # Create multiple SPECTRAL_WINDOW datasets
    # Dataset per output row required because column shapes are variable
    
    for num_chan in num_chans:
        dask_num_chan = da.full((1,), num_chan, dtype=np.int32)
        dask_chan_freq = da.asarray([[info['operating_frequency']]])
        dask_chan_width = da.full((1, num_chan), 2.5e6/num_chan)

        ds = Dataset({
            "NUM_CHAN": (("row",), dask_num_chan),
            "CHAN_FREQ": (("row", "chan"), dask_chan_freq),
            "CHAN_WIDTH": (("row", "chan"), dask_chan_width),
        })

        spw_datasets.append(ds)

    # For each cartesian product of SPECTRAL_WINDOW and POLARIZATION
    # create a corresponding DATA_DESCRIPTION.
    # Each column has fixed shape so we handle all rows at once
    spw_ids, pol_ids = zip(*product(range(len(num_chans)),
                                    range(len(corr_types))))
    dask_spw_ids = da.asarray(np.asarray(spw_ids, dtype=np.int32))
    dask_pol_ids = da.asarray(np.asarray(pol_ids, dtype=np.int32))
    ddid_datasets.append(Dataset({
        "SPECTRAL_WINDOW_ID": (("row",), dask_spw_ids),
        "POLARIZATION_ID": (("row",), dask_pol_ids),
    }))

    # Now create the associated MS dataset
    
    vis_data, baselines = cal_vis.get_all_visibility()
    vis_array = np.array(vis_data, dtype=np.complex64)
    chunks = {
        "row": (vis_array.shape[0],),
    }
    baselines = np.array(baselines)
    bl_pos = np.array(ant_pos)[baselines]
    uu_a, vv_a, ww_a = -(bl_pos[:,1] - bl_pos[:,0]).T/constants.L1_WAVELENGTH  # Use the - sign to get the same orientation as our tart projections.

    uvw_array = np.array([uu_a, vv_a, ww_a]).T

    for ddid, (spw_id, pol_id) in enumerate(zip(spw_ids, pol_ids)):
        # Infer row, chan and correlation shape
        logger.info("ddid:{} ({}, {})".format(ddid, spw_id, pol_id))
        row = sum(chunks['row'])
        chan = spw_datasets[spw_id].CHAN_FREQ.shape[1]
        corr = pol_datasets[pol_id].CORR_TYPE.shape[1]

        # Create some dask vis data
        dims = ("row", "chan", "corr")
        logger.info("Data size {}".format((row, chan, corr)))
        
        np_data = vis_array.reshape((row, chan, corr))
        np_uvw = uvw_array.reshape((row, 3))
        
        data_chunks = tuple((chunks['row'], chan, corr))
        dask_data = da.from_array(np_data, chunks=data_chunks)
        
        uvw_data = da.from_array(np_uvw)
        # Create dask ddid column
        dask_ddid = da.full(row, ddid, chunks=chunks['row'], dtype=np.int32)
        dataset = Dataset({
            'DATA': (dims, dask_data),
            'WEIGHT': (("row", "corr"), da.from_array(0.95*np.ones((row, corr)))),
            'WEIGHT_SPECTRUM': (dims, da.from_array(0.95*np.ones_like(np_data, dtype=np.float64))),
            'SIGMA_SPECTRUM': (dims, da.from_array(np.ones_like(np_data, dtype=np.float64)*0.05)),
            'UVW': (("row", "uvw",), uvw_data),
            'ANTENNA1': (("row",), da.from_array(baselines[:,0])),
            'ANTENNA2': (("row",), da.from_array(baselines[:,1])),
            'FEED1': (("row",), da.from_array(baselines[:,0])),
            'FEED2': (("row",), da.from_array(baselines[:,1])),
            'DATA_DESC_ID': (("row",), dask_ddid)
        })
        ms_datasets.append(dataset)

    ms_writes = xds_to_table(ms_datasets, ms_table_name, columns="ALL")
    ant_writes = xds_to_table(ant_datasets, ant_table_name, columns="ALL")
    feed_writes = xds_to_table(feed_datasets, feed_table_name, columns="ALL")
    field_writes = xds_to_table(field_datasets, field_table_name, columns="ALL")
    obs_writes = xds_to_table(obs_datasets, obs_table_name, columns="ALL")
    pol_writes = xds_to_table(pol_datasets, pol_table_name, columns="ALL")
    spw_writes = xds_to_table(spw_datasets, spw_table_name, columns="ALL")
    ddid_writes = xds_to_table(ddid_datasets, ddid_table_name, columns="ALL")
    source_writes = xds_to_table(src_datasets, src_table_name, columns="ALL")

    dask.compute(ms_writes)
    dask.compute(ant_writes)
    dask.compute(feed_writes)
    dask.compute(field_writes)
    dask.compute(obs_writes)
    dask.compute(pol_writes)
    dask.compute(spw_writes)
    dask.compute(ddid_writes)
    dask.compute(source_writes)


if __name__=="__main__":

    parser = argparse.ArgumentParser(description='Generate measurement set from a JSON file from the TART radio telescope.', 
                                    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--json', required=False, default=None, help="Snapshot observation saved JSON file (visiblities, positions and more).")
    parser.add_argument('--ms', required=False, default='tart.ms', help="Output MS table.")
    parser.add_argument('--api', required=False, default='https://tart.elec.ac.nz/signal', help="Telescope API server URL.")
    parser.add_argument('--catalog', required=False, default='https://tart.elec.ac.nz/catalog', help="Catalog API URL.")
    parser.add_argument('--vis', required=False, default=None, help="Use a local JSON file containing the visibilities to create the image.")

    ARGS = parser.parse_args()

    logger.setLevel(logging.DEBUG)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    
    if os.path.isdir(ARGS.ms):
        raise RuntimeError("Measurement set '{}' exists. Please delete before continuing".format(ARGS.ms))

    if ARGS.json:
        logger.info("Getting Data from file: {}".format(ARGS.json))
        # Load data from a JSON file
        with open(ARGS.json, 'r') as json_file:
            json_data = json.load(json_file)
            
    else:
        logger.info("Getting Data from API: {}".format(ARGS.api))
        api = api_handler.APIhandler(ARGS.api)
        info = api.get('info')
        ant_pos = api.get('imaging/antenna_positions')
        config = settings.from_api_json(info['info'], ant_pos)
        gains_json = api.get('calibration/gain')
        vis_json = api.get('imaging/vis')
        ts = api_imaging.vis_json_timestamp(vis_json)

        logger.info("Download Complete")
       
        src_json = api.get_url(api.catalog_url(lon=config.get_lon(),
                                               lat=config.get_lat(),
                                               datestr=ts.isoformat()))

        json_data = {'info':info,
                'ant_pos':ant_pos,
                'gains': gains_json,
                'data': [[vis_json, src_json]]
                }
    
    info = json_data['info']
    ant_pos = json_data['ant_pos']
    config = settings.from_api_json(info['info'], ant_pos)
    gains = json_data['gains']['gain']
    phases = json_data['gains']['phase_offset']
    
    for d in json_data['data']: # TODO deal with multiple observations in the JSON file later.
        vis_json, source_json = d
        cv, timestamp = api_imaging.vis_calibrated(vis_json, config, gains, phases, [])
        src_list = source_json

    # FIXME. These appear to be weird polarization codes (or ID's). WHY oh WHY are they called corr?
    # We use RR antennas, so this should be either RR or I?. I'm using FITS code 1 for this 'I'
    corr_types = [ [MS_STOKES_ENUMS['I']] ]
    
    ms_create(ms_table_name=ARGS.ms, info = info['info'], ant_pos = ant_pos, cal_vis=cv, timestamps=timestamp,
                   corr_types=corr_types, sources=src_list)
