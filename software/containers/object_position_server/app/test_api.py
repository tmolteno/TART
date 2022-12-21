
import datetime
import requests

import json

import unittest


class TestCatalog(unittest.TestCase):

    def setUp(self):
        self.server = "http://localhost:8876"

    def request(self, dt):
        payload = {'date': dt.isoformat(),
                   'lat': -45.87,
                   'lon': 170.6, 'elevation': 45}

        r = requests.get('{}/catalog'.format(self.server), params=payload)
        return json.loads(r.text)

    def test_basic_request(self):
        ans = self.request(datetime.datetime.utcnow())

        for sv in ans:
            self.assertTrue('r' in sv)
            self.assertTrue('el' in sv)
            self.assertTrue('az' in sv)
            self.assertTrue('jy' in sv)

    def test_future_date(self):
        t = datetime.datetime.utcnow()  # utc.utc_datetime(2002, 10, 31, 2, 2, 2)
        dt = datetime.timedelta(days=2)

        with self.assertRaises(ValueError):
            ans = self.request(t + dt)

    def test_speed(self):
        t = datetime.datetime.utcnow()  # utc.utc_datetime(2002, 10, 31, 2, 2, 2)

        dt = datetime.timedelta(minutes=1)
        for i in range(10):
            ans = self.request(t)
            t += dt

            # for sv in ans:
            #self.assertTrue('r' in sv)
            #self.assertTrue('el' in sv)
            #self.assertTrue('az' in sv)


if __name__ == '__main__':
    unittest.main()
