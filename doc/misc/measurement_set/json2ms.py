import numpy as np

import dask
import dask.array as da
from itertools import product


from tart.operation import settings

from tart_tools import api_handler
from tart_tools import api_imaging
from tart.imaging import elaz

import json
import logging

import pyrap.tables as pt
from numpy.testing import assert_array_equal

from daskms import Dataset, xds_to_table

logger = logging.getLogger()


def ms_create(ms_table_name, info, ant_pos, cal_vis, timestamps, corr_types, sources):
    # Set up
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
    ddid_table_name = "::".join((ms_table_name, "DATA_DESCRIPTION"))
    pol_table_name = "::".join((ms_table_name, "POLARIZATION"))
    spw_table_name = "::".join((ms_table_name, "SPECTRAL_WINDOW"))
    # SOURCE is an optional MS sub-table
    src_table_name = "::".join((ms_table_name, "SOURCE"))

    ms_datasets = []
    ant_datasets = []
    ddid_datasets = []
    pol_datasets = []
    spw_datasets = []
    src_datasets = []

    # For comparison
    all_data_desc_id = []
    all_data = []

    # Create ANTENNA dataset of 64 antennas
    # Each column in the ANTENNA has a fixed shape so we
    # can represent all rows with one dataset
    na = len(ant_pos)
    position = da.asarray(ant_pos)
    offset = da.zeros((na, 3))
    names = np.array(['ANTENNA-%d' % i for i in range(na)], dtype=np.object)
    
    ds = Dataset({
        'POSITION': (("row", "xyz"), position),
        'OFFSET': (("row", "xyz"), offset),
        'NAME': (("row",), da.from_array(names, chunks=na)),
    })
    ant_datasets.append(ds)

    # Create SOURCE datasets
    print(sources)
    for s, src in enumerate(sources):
        name = src['name']
        rest_freq = [info['operating_frequency']]
        direction = [np.radians(src['el']), np.radians(src['az'])]   ## FIXME these are in elevation and azimuth. Not in J2000.
        
        logger.info("SOURCE: {}, timestamp: {}".format(name, timestamps))
        dask_num_lines = da.full((1,), len(rest_freq), dtype=np.int32)
        dask_direction = da.asarray(direction)[None, :]
        dask_rest_freq = da.asarray(rest_freq)[None, :]
        dask_name = da.asarray(np.asarray([name], dtype=np.object))
        dask_time = da.asarray(np.asarray([timestamps], dtype=np.object))
        ds = Dataset({
            "NUM_LINES": (("row",), dask_num_lines),
            "NAME": (("row",), dask_name),
            #"TIME": (("row",), dask_time), # FIXME. Causes an error. Need to sort out TIME data fields
            "REST_FREQUENCY": (("row", "line"), dask_rest_freq),
            "DIRECTION": (("row", "dir"), dask_direction),
            })
        src_datasets.append(ds)

    # Create POLARISATION datasets.
    # Dataset per output row required because column shapes are variable
    for r, corr_type in enumerate(corr_types):
        dask_num_corr = da.full((1,), len(corr_type), dtype=np.int32)
        dask_corr_type = da.from_array(corr_type,
                                       chunks=len(corr_type))[None, :]
        ds = Dataset({
            "NUM_CORR": (("row",), dask_num_corr),
            "CORR_TYPE": (("row", "corr"), dask_corr_type),
        })

        pol_datasets.append(ds)

    # Create multiple MeerKAT L-band SPECTRAL_WINDOW datasets
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
    
    # FIXME. There is some assumed association between row number and baselines.I am ignoring this here.
    # FIXME We have only a single poloarzation 'LL'
    vis_data, baselines = cal_vis.get_all_visibility()
    vis_array = np.array(vis_data, dtype=np.complex64)
    
    # FIXME. Following is arbitrary
    chunks = {
        "row": (vis_array.shape[0],),
    }
    
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

        logger.info("np_data {}".format(np_data.shape))
        data_chunks = tuple((chunks['row'], chan, corr))
        dask_data = da.from_array(np_data, chunks=data_chunks)
        # Create dask ddid column
        dask_ddid = da.full(row, ddid, chunks=chunks['row'], dtype=np.int32)
        dataset = Dataset({
            'DATA': (dims, dask_data),
            'DATA_DESC_ID': (("row",), dask_ddid)
        })
        ms_datasets.append(dataset)
        all_data.append(dask_data)
        all_data_desc_id.append(dask_ddid)

    ms_writes = xds_to_table(ms_datasets, ms_table_name, columns="ALL")
    ant_writes = xds_to_table(ant_datasets, ant_table_name, columns="ALL")
    pol_writes = xds_to_table(pol_datasets, pol_table_name, columns="ALL")
    spw_writes = xds_to_table(spw_datasets, spw_table_name, columns="ALL")
    ddid_writes = xds_to_table(ddid_datasets, ddid_table_name, columns="ALL")
    source_writes = xds_to_table(src_datasets, src_table_name, columns="ALL")

    dask.compute(ms_writes)
    dask.compute(ant_writes)
    dask.compute(pol_writes)
    dask.compute(spw_writes)
    dask.compute(ddid_writes)
    dask.compute(source_writes)

import argparse

if __name__=="__main__":

    parser = argparse.ArgumentParser(description='Generate measurement set from a JSON file from the TART radio telescope.', 
                                    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--json', required=True, default=None, help="Snapshot observation saved JSON file (visiblities, positions and more).")
    parser.add_argument('--ms', required=False, default='tart.ms', help="Output MS table.")

    ARGS = parser.parse_args()

    logger.setLevel(logging.DEBUG)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    

    logger.info("Getting Data from file: {}".format(ARGS.json))
    # Load data from a JSON file
    with open(ARGS.json, 'r') as json_file:
        json_data = json.load(json_file)

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
    # We use LL, so this should be either LL or I?. I'm using FITS code 1 for this 'I'
    corr_types = [ [1] ]
    
    sources = [("PKS-1934", [5.1461782, -1.11199629], [0.9*.856e9, 1.1*.856e9]),
               ("3C286",  [3.53925792, 0.53248541], [0.8*.856e9, .856e9, 1.2*.856e9])]
    
    ms_create(ms_table_name=ARGS.ms, info = info['info'], ant_pos = ant_pos, cal_vis=cv, timestamps=timestamp,
                   corr_types=corr_types, sources=src_list)
