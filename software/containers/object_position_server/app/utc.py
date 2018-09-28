from datetime import tzinfo, timedelta, datetime
import pytz

#class UTC(tzinfo):
    #"""UTC"""
    #ZERO = timedelta(0)
    #HOUR = timedelta(hours=1)


    #def utcoffset(self, dt):
        #return self.ZERO

    #def tzname(self, dt):
        #return "UTC"
    
    #def dst(self, dt):
        #return self.ZERO

    #def localize(self, dt, is_dst=False):
        #'''Convert naive time to local time'''
        #if dt.tzinfo is not None:
            #raise ValueError('Not naive datetime (tzinfo is already set)')
        #return dt.replace(tzinfo=self)

    #def __repr__(self):
        #return "<UTC>"

    #def __str__(self):
        #return "UTC"

#UTC = utc = UTC() # UTC is a singleton


def utc_datetime(year, month, day, hour=0, minute=0, second=0.0):
  s = int(second)
  us = int((second - int(second)) * 1000000)
  return datetime(year=year, month=month, day=day, hour=hour, minute=minute, second=s, microsecond=us, tzinfo=UTC())

def now():
  t = datetime.now(pytz.utc)
  return t

def to_utc(dt):
    if dt.tzinfo is None:
        dt = pytz.utc.localize(dt)
    return dt.astimezone(pytz.utc)
