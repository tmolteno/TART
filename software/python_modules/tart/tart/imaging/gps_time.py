# Copyright (C) Tim Molteno 2008-2019. All rights reserved

import datetime
import math

from tart.util import utc


class GpsTime(object):
    SEC_PER_DAY = 86400.0

    def __init__(self, y, m, d, h, minutes, seconds):
        if y < 1980:
            raise Exception("Year #{y} must >= 1980")

        in_time = utc.utc_datetime(
            year=y, month=m, day=d, hour=h, minute=minutes, second=0
        )
        gps_t = utc.utc_datetime(year=1980, month=1, day=6, hour=0, minute=0, second=0)

        self.m_day_of_year = in_time.timetuple().tm_yday

        gps_sec = (in_time - gps_t).total_seconds() + seconds + self.utc_offset(in_time)
        gps_days = int(math.floor(gps_sec / GpsTime.SEC_PER_DAY))
        gps_weeks = gps_days / 7.0

        self.m_week = int(math.floor(gps_weeks))
        self.sec_of_week = gps_sec - self.m_week * 7.0 * GpsTime.SEC_PER_DAY

    @classmethod
    def from_time(self, utc_date):
        return GpsTime(
            utc_date.year,
            utc_date.month,
            utc_date.day,
            utc_date.hour,
            utc_date.minute,
            utc_date.second,
        )

    # I don't think that the UTC offset works correctly.
    @classmethod
    def utc_offset(self, t):
        if t >= utc.utc_datetime(2017, 1, 1):
            return 18
        if t >= utc.utc_datetime(2015, 7, 1):
            return 17
        if t >= utc.utc_datetime(2012, 7, 1):
            return 16
        if t >= utc.utc_datetime(2009, 1, 1):
            return 15
        if t >= utc.utc_datetime(2006, 1, 1):
            return 14
        if t >= utc.utc_datetime(1999, 1, 1):
            return 13
        if t >= utc.utc_datetime(1997, 7, 1):
            return 12
        if t >= utc.utc_datetime(1996, 1, 1):
            return 11
        if t >= utc.utc_datetime(1994, 7, 1):
            return 10
        if t >= utc.utc_datetime(1993, 7, 1):
            return 9
        if t >= utc.utc_datetime(1992, 7, 1):
            return 8
        if t >= utc.utc_datetime(1991, 1, 1):
            return 7
        if t >= utc.utc_datetime(1990, 1, 1):
            return 6
        if t >= utc.utc_datetime(1988, 1, 1):
            return 5
        if t >= utc.utc_datetime(1985, 7, 1):
            return 4
        if t >= utc.utc_datetime(1983, 7, 1):
            return 3
        if t >= utc.utc_datetime(1982, 7, 1):
            return 2
        if t >= utc.utc_datetime(1981, 7, 1):
            return 1
        if t >= utc.utc_datetime(1980, 1, 1):
            return 0
        return False

    def __repr__(self):
        return "W:%d_SOW:%d (doy %d)" % (
            self.m_week,
            self.sec_of_week,
            self.m_day_of_year,
        )

    def diff(self, b):
        w = (self.m_week - b.week()) * 7 * GpsTime.SEC_PER_DAY
        s = self.sec_of_week - b.sow()
        return w + s

    def week(self):
        return self.m_week

    def sow(self):
        return self.sec_of_week

    def day_of_year(self):
        return self.m_day_of_year

    def day_of_week(self):
        dow = self.sec_of_week / GpsTime.SEC_PER_DAY
        return int(math.floor(dow))
