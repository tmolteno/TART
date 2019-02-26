import math
import datetime

def get_utc_hours(timestamp):
    sec =    (timestamp - datetime.datetime.combine(timestamp.date(), datetime.time.min)).total_seconds()
    hours = (sec / 3600.0)
    return hours

def sign(x):
    return math.copysign(1, x)

def get_julian_date(timestamp):
    YY=timestamp.year
    MM=timestamp.month
    DD=timestamp.day
    UTcor=0
    HR=timestamp.hour
    Min=timestamp.minute
    Sec=timestamp.second
    return 367*YY - (7*(YY+((MM+9)/12))/4) + (275*MM/9)+ DD + 1721013.5 + UTcor/24 \
        - 0.5*sign((100*YY)+MM-190002.5) + 0.5 + HR/24.0 + Min/(60.0*24.0) + Sec/(3600.0*24.0)

def get_mjd(timestamp):
    return get_julian_date(timestamp)-2400000.5


def JulianDay (utc_date):

    jhr =utc_date.hour + utc_date.minute / 60.0 + utc_date.second / 3600.0
    day = utc_date.day
    month = utc_date.month
    year = utc_date.year

    if (month<=2):
        month=month+12
        year=year-1
    return (int)(365.25*year) + (int)(30.6001*(month+1)) - 15 + 1720996.5 + day + jhr/24.0
