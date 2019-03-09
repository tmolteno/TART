import math
import datetime
from .gps_time import GpsTime
from tart.util import utc 

def get_utc_hours(timestamp):
    sec =    (timestamp - datetime.datetime.combine(timestamp.date(), datetime.time.min)).total_seconds()
    hours = (sec / 3600.0)
    return hours

def sign(x):
    return math.copysign(1, x)

def get_julian_date(timestamp):
    if (timestamp.tzinfo is not None):
        if not isinstance(timestamp.tzinfo, utc.UTC):
            raise RuntimeError('Timestamp has timezone {}'.format(timestamp.tzinfo))
    
    YY=timestamp.year
    MM=timestamp.month
    DD=timestamp.day
    UTcor=0
    HR=timestamp.hour
    Min=timestamp.minute
    Sec=timestamp.second
    return 367*YY - (7*(YY+((MM+9)//12))//4) + (275*MM//9)+ DD + 1721013.5 + UTcor//24 \
        - 0.5*sign((100*YY)+MM-190002.5) + 0.5 + HR/24.0 + Min/(60.0*24.0) + Sec/(3600.0*24.0)

def get_mjd(timestamp):
    return get_julian_date(timestamp)-2400000.5


def JulianDay (utc_date):
    return get_julian_date(utc_date)
