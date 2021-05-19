import os
import jsonrpclib
import datetime
import numpy as np

from tart.util.singleton import Singleton
from tart.util import utc

from tart.imaging import gps_time
from tart.imaging import ephemeris
from tart.imaging import sp3_interpolator


@Singleton
class EphemeridesProxy(object):
    def __init__(self):

        if "EPHEMERIS_SERVER_HOST" in os.environ:
            server_host = os.environ["EPHEMERIS_SERVER_HOST"]
        else:
            server_host = "localhost"

        self.server = jsonrpclib.Server("http://%s:8876/rpc/gps" % server_host)
        self.cache = {}
        self.sp3_cache = {}

    def get_date_hash(self, utc_date, sv):
        # Make a hash
        gpst = gps_time.GpsTime.from_time(utc_date)
        cache_hash = "%02d%04d%02d%02d%02d-%4d" % (
            sv,
            utc_date.year,
            utc_date.month,
            utc_date.day,
            utc_date.hour,
            gpst.m_week,
        )
        return cache_hash

    def get_sp3_hash(self, utc_date):
        # Make a hash
        gpst = gps_time.GpsTime.from_time(utc_date)
        cache_hash = "%04d%02d%02d-%4d" % (
            utc_date.year,
            utc_date.month,
            utc_date.day,
            gpst.m_week,
        )
        return cache_hash

    def get_ephemeris(self, utc_date, sv):
        h = self.get_date_hash(utc_date, sv)
        print("hash({}".format(h))
        try:
            eph = self.cache[h]
        except KeyError:
            eph_hash = self.server.get_ephemeris(utc_date.isoformat(), sv)
            eph = ephemeris.Ephemeris(eph_hash)
            self.cache[h] = eph
            print(("Cache miss %s, %d" % (utc_date, sv)))
        return eph

    def get_sp3_interpolator(self, utc_date):
        h = self.get_sp3_hash(utc_date)
        try:
            sp3 = self.sp3_cache[h]
        except KeyError:
            gpst = gps_time.GpsTime.from_time(utc_date)
            pts = self.server.get_interp_points(utc_date.isoformat())
            sp3 = sp3_interpolator.Sp3Interpolator(gpst, pts)
            self.sp3_cache[h] = sp3
            print(("sp3 Cache miss %s" % (utc_date)))
        return sp3

    def get_sv_position(self, utc_date, sv):
        gpst = gps_time.GpsTime.from_time(utc_date)
        eph = self.get_ephemeris(utc_date, sv)
        pos = eph.get_sv_position(gpst)
        print("get_sv_position({}, {}, {}) -> {}".format(utc_date, gpst, sv, pos))
        return np.array(pos)

    def get_sv_position_sp3(self, utc_date, sv):
        gpst = gps_time.GpsTime.from_time(utc_date)
        sp3 = self.get_sp3_interpolator(utc_date)
        pos = sp3.get_sv_position(gpst, sv)
        return np.array(pos)

    def get_sv_velocity(self, utc_date, sv):
        gpst = gps_time.GpsTime.from_time(utc_date)
        eph = self.get_ephemeris(utc_date, sv)
        return eph.get_velocity(gpst.sow())

    def get_sv_positions(self, utc_date):
        gpst = gps_time.GpsTime.from_time(utc_date)
        ret = []
        for sv in range(1, 32):
            try:
                eph = self.get_ephemeris(utc_date, sv)
                pos = eph.get_sv_position(gpst)
                ret.append([sv, pos])
            except jsonrpclib.ProtocolError:
                pass
        return ret

    def get_remote_position(self, utc_date, sv):
        return self.server.get_sv_position_sp3(utc_date.isoformat(), sv)
