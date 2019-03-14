#!/usr/bin/env python
#    Download Raw Data from a TART telescope
#    The telescope should be in RAW mode if you want it to capture raw data every minute.
#
#    Tim Molteno 2017-2019 - tim@elec.ac.nz

import argparse
import json

import os
import hashlib
import shutil
import urllib.request, urllib.error, urllib.parse
import time

from tart_tools.api_handler import AuthorizedAPIhandler

def sha256_checksum(filename, block_size=65536):
    sha256 = hashlib.sha256()
    with open(filename, 'rb') as f:
        for block in iter(lambda: f.read(block_size), b''):
            sha256.update(block)
    return sha256.hexdigest()

def download_file(url, checksum=0, directory="."):
    print("Download_file({}, {})".format(url, checksum))
    file_name = url.split('/')[-1]
    file_path = os.path.join(directory, file_name)
    
    # Download the file from `url` and save it locally under `file_path`:
    with urllib.request.urlopen(url) as response, open(file_path, 'wb') as out_file:
        shutil.copyfileobj(response, out_file)
    
    if checksum:
        downloaded_checksum = sha256_checksum(file_path)
        if (downloaded_checksum != checksum):
            print("Removing file: Checksum failed\n{}\n{}".format(checksum, downloaded_checksum))
            os.remove(file_path)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Download data from the telescope', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--api', required=False, default='https://tart.elec.ac.nz/signal', help="Telescope API server URL.")
    parser.add_argument('--pw', default='password', required=True, type=str, help='API password')
    parser.add_argument('--dir', type=str, default=".", help='local directory to download')
    parser.add_argument('--raw', action='store_true', help='Download Raw Data in HDF format')
    parser.add_argument('--vis', action='store_true', help='Download Visibility Data in HDF format')

    ARGS = parser.parse_args()

    api = AuthorizedAPIhandler(ARGS.api, ARGS.pw)

    print("Downloading Raw Data from {}".format(ARGS.api))
    os.makedirs(ARGS.dir, exist_ok=True)
    
    tart_endpoint = ARGS.api + "/"

    if not (ARGS.raw or ARGS.vis):
        raise RuntimeError("Either --raw or --vis must be specified")
    
    while True:
        resp_vis = []
        resp_raw = []
        if (ARGS.vis):
            resp_vis = api.get('vis/data')
        if (ARGS.raw):
            resp_raw = api.get('raw/data')

        try:
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
                            download_file(data_url, entry['checksum'], ARGS.dir)
                    else:
                        download_file(data_url, entry['checksum'], ARGS.dir)

        except Exception as e:
            print("Exception "+str(e))
        finally:
            print("Pausing.")
            time.sleep(2)

