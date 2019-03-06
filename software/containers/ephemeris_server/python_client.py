# Test client for the object position server
# Copyright 2013, Tim Molteno. tim@elec.ac.nz
#
# Requires jsonrpc
#     easy_install jsonrpclib

from tart.util import utc
from tart.imaging.ephemerides_proxy import *

import datetime
import requests

if False:
        
    #server = "https://tart.elec.ac.nz/catalog"
    server = "http://localhost:8876"

    payload = {} # {'date': datetime.datetime.utcnow().isoformat()}
    r = requests.get('{}/position'.format(server), params=payload)
    print(r.url)
    print(r.text)

    t = datetime.datetime.utcnow() # utc.utc_datetime(2002, 10, 31, 2, 2, 2)

    #for i in range(0,100):
        #t = t - datetime.timedelta(seconds=100.0)
        #payload = {'date': t, 'sv':1}
        #r = requests.get('{}/position'.format(server), params=payload)
        #print r.text


    #payload = {'date': t.isoformat(), 'sv':1, 'lat':-45.86, 'lon':170.54}
    #r = requests.get('{}/catalog'.format(server), params=payload)
    #print r.url
    #print r.text



ep = EphemeridesProxy.Instance()

t = utc.utc_datetime(2013, 9, 21, 0, 59, 3)
sv = 21
for i in range(0,100):
    t = t + datetime.timedelta(seconds=1.0)
    pos = ep.get_sv_position(t, sv)
    pos_remote = ep.get_remote_position(t, sv)
    diff = np.array(pos) - np.array(pos_remote)
    dr = np.sqrt(diff.dot(diff))
    print(t, dr)
