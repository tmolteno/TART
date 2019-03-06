#!/usr/bin/env python
#    Upload antenna positions from local file to remote telescope
#    Tim Molteno 2017 - tim@elec.ac.nz

import argparse
import json


import os, stat
import requests
from requests.auth import HTTPDigestAuth
import hashlib
import urllib.request, urllib.error, urllib.parse
import time

from tart_tools.api_handler import AuthorizedAPIhandler

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
    print("Download_file({}, {})".format(url, checksum))
    file_name = url.split('/')[-1]
    u = urllib.request.urlopen(url)
    f = open(file_name, 'wb')
    meta = u.info()
    file_size = int(meta.getheaders("Content-Length")[0])
    print("Downloading: %s Bytes: %s" % (file_name, file_size))

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
        print(status, end=' ')
    f.close()
    if checksum:
        if (sha256_checksum(file_name) != checksum):
            os.remove(file_name)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Get RAW data from the telescope', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--api', required=False, default='https://tart.elec.ac.nz/signal', help="Telescope API server URL.")
    parser.add_argument('--pw', default='password', type=str, help='API password')
    parser.add_argument('--file', type=str, required=True, help='local file to download')

    ARGS = parser.parse_args()

    api = AuthorizedAPIhandler(ARGS.api, ARGS.pw)
    #resp = api.post_payload_with_token('calibration/antenna_positions', positions_dict["antenna_positions"])
    print("Raw Data Downloader")

    tart_endpoint = ARGS.api + "/"
    ##tart_endpoint = "http://tart2-pear/signal/"
    ##tart_endpoint = "https://tart.elec.ac.nz/dev/"
    #api_endpoint = tart_endpoint+"api/v1"
    while True:
        resp_raw = api.get('raw/data')
        resp_vis = api.get('vis/data')

        for entry in resp_raw+resp_vis:
            if 'filename' in entry:
                data_url = urllib.parse.urljoin(tart_endpoint, entry['filename'])
                file_name = data_url.split('/')[-1]
                if os.path.isfile(file_name):
                    if (sha256_checksum(file_name) == entry['checksum']):
                        print('Skipping', file_name)
                    else:
                        print('Corrupted File', file_name)
                        os.remove(file_name)
                        download_file(data_url, entry['checksum'])
                else:
                    download_file(data_url, entry['checksum'])
        time.sleep(5)

