import argparse
import os, stat
import requests
from requests.auth import HTTPDigestAuth
import json
import hashlib
import urllib2
import time

def get_from_api(view, api_endpoint):
    myResponse = requests.get(api_endpoint+view,)
    if(myResponse.ok):
        jData = json.loads(myResponse.content)
    else:
        myResponse.raise_for_status()
    return jData

def sha256_checksum(filename, block_size=65536):
    sha256 = hashlib.sha256()
    with open(filename, 'rb') as f:
        for block in iter(lambda: f.read(block_size), b''):
            sha256.update(block)
    return sha256.hexdigest()

def download_file(url, checksum=0):
    file_name = url.split('/')[-1]
    u = urllib2.urlopen(url)
    f = open(file_name, 'wb')
    meta = u.info()
    file_size = int(meta.getheaders("Content-Length")[0])
    print "Downloading: %s Bytes: %s" % (file_name, file_size)

    file_size_dl = 0
    block_sz = 8192
    while True:
        buffer = u.read(block_sz)
        if not buffer:
            break
        file_size_dl += len(buffer)
        f.write(buffer)
        status = r"%10d  [%3.2f%%]" % (file_size_dl, file_size_dl * 100. / file_size)
        status = status + chr(8)*(len(status)+1)
        print status,
    f.close()
    if checksum:
        if (sha256_checksum(file_name) != checksum):
            os.remove(file_name)

if __name__=="__main__":
    tart_endpoint = "http://tart2-raspberry/dev/"
    #tart_endpoint = "http://tart2-pear/signal/"
    #tart_endpoint = "https://tart.elec.ac.nz/dev/"
    api_endpoint = tart_endpoint+"api/v1"
    while True:
        resp_raw = get_from_api('/raw/data', api_endpoint)
        resp_vis = get_from_api('/vis/data', api_endpoint)
        for entry in resp_raw+resp_vis:
            if entry.has_key('filename'):
                f = tart_endpoint+entry['filename']
                file_name = f.split('/')[-1]
                if os.path.isfile(file_name):
                    if (sha256_checksum(file_name) == entry['checksum']):
                        print 'Skipping', file_name
                    else:
                        print 'Corrupted File', file_name
                        os.remove(file_name)
                        download_file(tart_endpoint+entry['filename'],entry['checksum'])
                else:
                    download_file(tart_endpoint+entry['filename'],entry['checksum'])
        time.sleep(5)

