"""
  TART API handler class
  Tim Molteno 2017-2021 - tim@elec.ac.nz
  Max Scheel 2017 - max@max.ac.nz
"""

import datetime
import json
import os
import requests
import logging
import shutil
import hashlib

import urllib.request, urllib.error, urllib.parse

from tart.operation import settings

# Default timeout
TIMEOUT = 15.0

logger = logging.getLogger()

def sha256_checksum(filename, block_size=65536):
    sha256 = hashlib.sha256()
    with open(filename, "rb") as f:
        for block in iter(lambda: f.read(block_size), b""):
            sha256.update(block)
    return sha256.hexdigest()


def download_file(url, checksum=0, file_path=None):
    logger.info("Download_file({}, {}) -> {}".format(url, checksum, file_path))

    # Download the file from `url` and save it locally under `file_path`:
    with urllib.request.urlopen(url) as response, open(file_path, "wb") as out_file:
        shutil.copyfileobj(response, out_file)

    if checksum:
        downloaded_checksum = sha256_checksum(file_path)
        if downloaded_checksum != checksum:
            logger.info(
                "Removing file: Checksum failed\n{}\n{}".format(
                    checksum, downloaded_checksum
                )
            )
            os.remove(file_path)

class APIhandler(object):
    def __init__(self, api_root):
        self.root = api_root
        self.token = None

    def url(self, path):
        return "{}/api/v1/{}".format(self.root, path)

    def catalog_url(
        self,
        config,
        catalog="https://tart.elec.ac.nz/catalog",
        datestr=datetime.datetime.utcnow().isoformat(),
    ):
        return "{}/catalog?lat={}&lon={}&date={}".format(
            catalog, config.get_lat(), config.get_lon(), datestr
        )

    def get(self, path):
        return self.get_url(self.url(path))

    def get_url(self, url):
        r = requests.get(url, timeout=TIMEOUT)
        r.raise_for_status()
        return json.loads(r.text)


class AuthorizedAPIhandler(APIhandler):
    """
    This object allows an authorized API call.

    Example:

        from tart_tools.api_handler import AuthorizedAPIhandler

        if __name__ == '__main__':
            parser = argparse.ArgumentParser(description='Change telescope mode', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
            parser.add_argument('--api', required=False, default='https://tart.elec.ac.nz/signal', help="Telescope API server URL.")
            parser.add_argument('--pw', default='password', type=str, help='API password')
            parser.add_argument('--mode', type=str, required=True, help='New mode (vis/raw)')

            ARGS = parser.parse_args()

            api = AuthorizedAPIhandler(ARGS.api, ARGS.pw)

            resp = api.post_payload_with_token('mode', ARGS.mode)


    """

    def __init__(self, api_root, pw):
        APIhandler.__init__(self, api_root=api_root)
        self.pw = pw
        self.token = None
        self.refresh_token = None
        self.login()

    def login(self):
        payload_dict = {"username": "admin", "password": self.pw}
        resp_json = self.__post_payload("/auth", payload_dict=payload_dict)
        if "access_token" in resp_json:
            self.token = resp_json["access_token"]
            self.refresh_token = resp_json["refresh_token"]
        else:
            raise Exception("Authorization failed. Wrong pw?")

    def refresh_access_token(self):
        r = requests.post(
            self.url("refresh"), headers=self.__get_refresh_header(), timeout=TIMEOUT
        )
        resp_json = json.loads(r.text)
        if "access_token" in resp_json:
            self.token = resp_json["access_token"]
            logger.info("refreshed token")

    def __post_payload(self, path, payload_dict):
        """ Currently only used for login() """
        r = requests.post(self.url(path), json=payload_dict, timeout=TIMEOUT)
        r.raise_for_status()
        return json.loads(r.text)

    def __get_header(self):
        if self.token is None:
            raise Exception("login required")
        return {"Authorization": "JWT " + self.token}

    def __get_refresh_header(self):
        if self.refresh_token is None:
            raise Exception("login required")
        return {"Authorization": "JWT " + self.refresh_token}

    # TODO catch the requests result that corresponds to a failed login (authorization expired)
    # and re-authorize automatically.

    def put(self, path, **kwargs):
        r = requests.put(
            self.url(path), headers=self.__get_header(), timeout=TIMEOUT, **kwargs
        )
        ret = json.loads(r.text)
        if "status" in ret and "sub_status" in ret:
            if (ret["status"] == 401) and (ret["sub_status"] == 101):
                self.refresh_access_token()
                return self.put(path, **kwargs)
        r.raise_for_status()
        return ret

    def post(self, path, **kwargs):
        r = requests.post(
            self.url(path), headers=self.__get_header(), timeout=TIMEOUT, **kwargs
        )
        #logger.info(r)
        ret = json.loads(r.text)
        if "status" in ret and "sub_status" in ret:
            if (ret["status"] == 401) and (ret["sub_status"] == 101):
                self.refresh_access_token()
                return self.post(path, **kwargs)
        r.raise_for_status()
        return ret

    def post_with_token(self, path):
        return self.post(path)

    def post_payload_with_token(self, path, payload_dict):
        return self.post(path, json=payload_dict)


def set_mode(api, mode):
    resp_json = api.post_with_token("mode/" + mode)
    return resp_json


def download_current_gain(api):
    gains = api.get("calibration/gain")
    return gains


def upload_gain(api, gain_dict):
    resp = api.post_payload_with_token("calibration/gain", gain_dict)
    logger.info("SUCCESS")
    return resp


def get_config(api):
    info = api.get("info")
    ant_pos = api.get("imaging/antenna_positions")
    return settings.from_api_json(info["info"], ant_pos)

