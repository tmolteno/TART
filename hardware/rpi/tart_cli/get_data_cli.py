#!/usr/bin/env python
#    Download Raw Data from a TART telescope
#    The telescope should be in RAW mode if you want it to capture raw data every minute.
#
#    Tim Molteno 2017-2019 - tim@elec.ac.nz

import argparse
import json

import os
import hashlib
import urllib.request, urllib.error, urllib.parse
import time

from tart_tools.api_handler import AuthorizedAPIhandler

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
    
    file_path = os.path.join(ARGS.dir, file_name)
    f = open(file_path, 'wb')
    meta = u.info()
    file_size = int(meta.get_all("Content-Length")[0])
    print("Downloading: %s Bytes: %s" % (file_path, file_size))

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
        if (sha256_checksum(file_path) != checksum):
            os.remove(file_path)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Get RAW data from the telescope', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--api', required=False, default='https://tart.elec.ac.nz/signal', help="Telescope API server URL.")
    parser.add_argument('--pw', default='password', type=str, help='API password')
    parser.add_argument('--dir', type=str, default=".", help='local directory to download')

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
                            download_file(data_url, entry['checksum'])
                    else:
                        download_file(data_url, entry['checksum'])
        except Exception as e:
            print("Exception "+str(e))
        finally:
            time.sleep(2)

