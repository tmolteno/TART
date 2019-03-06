# Test client for the object position server
# Copyright 2013, Tim Molteno. tim@elec.ac.nz
#
# Requires jsonrpc
#     easy_install jsonrpclib

#from tart.util import utc

import datetime
import requests

#server = "https://tart.elec.ac.nz/catalog"
server = "http://localhost:8876"

payload = {} # {'date': datetime.datetime.utcnow().isoformat()}
r = requests.get('{}/position'.format(server), params=payload)
print r.url
print r.text

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
