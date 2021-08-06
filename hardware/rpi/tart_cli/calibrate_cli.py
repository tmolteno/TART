import argparse
import os, stat

import time, os
import traceback

import numpy as np
from numpy import concatenate as cc

import requests
from requests.auth import HTTPDigestAuth
import json

api_endpoint = "https://tart.elec.ac.nz/dev/api/v1"


def get_from_api(view):
    myResponse = requests.get(
        api_endpoint + view,
    )
    if myResponse.ok:
        jData = json.loads(myResponse.content)
    else:
        myResponse.raise_for_status()
    return jData


def api_get_timestamp():
    view = "/imaging/timestamp"
    myResponse = requests.get(api_endpoint + view)
    if myResponse.ok:
        date_str = myResponse.content
    return date_str


def api_get_vis():
    res = get_from_api("/imaging/vis")
    return res  # v_complex


def get_calibration_measurements():
    "this will be an API request"
    vis = api_get_vis()
    ts = api_get_timestamp()
    cal_measurements = [
        {
            "el": 90,
            "az": 0,
            "data": {
                "vis": vis,
                "timestamp": ts,
            },
        }
    ]  # up
    # cal_measurements = [{'el':70,'az':  0,'data':{'vis':vis,'timestamp':ts,}}] # north
    # cal_measurements = [{'el':70,'az': 90,'data':{'vis':vis,'timestamp':ts,}}] # east
    # cal_measurements = [{'el':70,'az':180,'data':{'vis':vis,'timestamp':ts,}}] # south
    # cal_measurements = [{'el':70,'az':270,'data':{'vis':vis,'timestamp':ts,}}] # west
    return cal_measurements


if __name__ == "__main__":
    cal_measurements = get_calibration_measurements()
    content = cal_measurements
    # print str(content)
    r = requests.post("http://tart2-pear/api/v1/calibrate", json=content)
    print(r.content)

    # v_complex = {}
    ##print res
    # for key, v in res.iteritems():
    # v_real, v_imag = v
    # v_complex[key] = v_real + 1j*v_imag

    # import dateutil.parser
    # ret = dateutil.parser.parse(date_str)
    # return ret


# r = requests.post('http://tart2-raspberry/api/v1/calibration/gain', json=eval(content))
