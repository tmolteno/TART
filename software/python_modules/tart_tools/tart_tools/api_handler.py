'''
  TART API handler class
  Tim Molteno 2017-2019 - tim@elec.ac.nz
  Max Scheel 2017 - max@max.ac.nz
'''

import datetime
import json
import requests

from tart.operation import settings

class APIhandler(object):
    def __init__(self, api_root):
        self.root = api_root
        self.token = None

    def url(self, path):
        return '{}/api/v1/{}'.format(self.root, path)

    def catalog_url(self, config, catalog='https://tart.elec.ac.nz/catalog', 
                    datestr=datetime.datetime.utcnow().isoformat()):
        return '{}/catalog?lat={}&lon={}&date={}'.format(catalog, config.get_lat(), 
                                                         config.get_lon(), datestr)

    def get(self, path):
        return self.get_url(self.url(path))


    def get_url(self, url):
        r = requests.get(url)
        r.raise_for_status()
        return json.loads(r.text)



class AuthorizedAPIhandler(APIhandler):
    def __init__(self, api_root, pw):
        APIhandler.__init__(self, api_root=api_root)
        self.pw = pw
        self.token = None
        self.refresh_token = None
        self.login()

    def login(self):
        payload_dict = {'username': 'admin', 'password': self.pw}
        resp_json = self.__post_payload('/auth', payload_dict=payload_dict)
        if 'access_token' in resp_json:
            self.token = resp_json['access_token']
            self.refresh_token = resp_json['refresh_token']
        else:
            raise Exception('Authorization failed. Wrong pw?')

    def refresh_access_token(self):
        r = requests.post(self.url('refresh'),
                          headers=self.__get_refresh_header())
        resp_json = json.loads(r.text)
        if 'access_token' in resp_json:
            self.token = resp_json['access_token']
            print('refreshed token')
       
    def __post_payload(self, path, payload_dict):
        ''' Currently only used for login() '''
        r = requests.post(self.url(path), json=payload_dict)
        r.raise_for_status()
        return json.loads(r.text)
 
    def __get_header(self):
        if self.token is None:
            raise Exception('login required')
        return {'Authorization': 'JWT '+ self.token}

    def __get_refresh_header(self):
        if self.refresh_token is None:
            raise Exception('login required')
        return {'Authorization': 'JWT '+ self.refresh_token}

    # TODO catch the requests result that corresponds to a failed login (authorization expired)
    # and re-authorize automatically.

    def put(self, path, **kwargs):
        r = requests.put(self.url(path), headers=self.__get_header(), **kwargs)
        ret = json.loads(r.text)
        if 'status' in ret and 'sub_status' in ret:
            if ((ret['status'] == 401) and (ret['sub_status'] == 101)):
                self.refresh_access_token()
                return self.put(path, **kwargs)
        r.raise_for_status()
        return ret

    def post(self, path, **kwargs):
        r = requests.post(self.url(path), headers=self.__get_header(), **kwargs)
        ret = json.loads(r.text)
        if 'status' in ret and 'sub_status' in ret:
            if ((ret['status'] == 401) and (ret['sub_status'] == 101)):
                self.refresh_access_token()
                return self.post(path, **kwargs)
        r.raise_for_status()
        return ret

    def post_with_token(self, path):
        return self.post(path)

    def post_payload_with_token(self, path, payload_dict):
        return self.post(path, json=payload_dict)


def set_mode(api, mode):
    resp_json = api.post_with_token('mode/'+mode)
    return resp_json

def download_current_gain(api):
    gains = api.get('calibration/gain')
    return gains

def upload_gain(api, gain_dict):
    resp = api.post_payload_with_token('calibration/gain', gain_dict)
    print("SUCCESS")
    return resp

def get_config(api):
    info = api.get('info')
    ant_pos = api.get('imaging/antenna_positions')
    return settings.from_api_json(info['info'], ant_pos)

#if __name__ == '__main__':
    #import argparse
    #import time
    #parser = argparse.ArgumentParser(description='')
    #parser.add_argument('--host', default='http://tart2-raspberry', type=str, help='api host')
    #parser.add_argument('--pw', default='password', type=str, help='API password')
    #ARGS = parser.parse_args()
    #api = AuthorizedAPIhandler(ARGS.host, ARGS.pw)
    #print(api.token)
    #api.refresh_access_token()
    #print(api.token)
    #while 1:
        #time.sleep(100)
        #set_mode(api,'vis')
