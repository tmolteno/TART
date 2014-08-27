# Copyright (C) Tim Molteno 2008-2013. All rights reserved

import math
from tart.simulation.util import Util

class Ephemeris:
  GM = 3.986005e14  # earth's universal gravitational parameter m^3/s^2
  WGS84_EARTH_ROTATION_RATE = 7.2921151467e-5;  # earth rotation rate, rad/s

  def __init__(self,in_hash):
    self.a0, self.a1, self.a2, self.a3 = in_hash['a'] # alpha parameters for ionospheric model
    self.b0, self.b1, self.b2, self.b3 = in_hash['b'] # beta parameters for ionospheric model

    self.svprn = in_hash['svprn']
    self.af0 = in_hash['af0']
    self.af1 = in_hash['af1']
    self.af2 = in_hash['af2']

    self.toc = in_hash['toc'] # GpsTime

    self.iode = in_hash['iode']
    self.crs = in_hash['crs']
    self.deltan = in_hash['deltan']
    self.m0 = in_hash['m0']

    self.cuc = in_hash['cuc']
    self.ecc = in_hash['ecc']
    self.cus = in_hash['cus']
    self.roota = in_hash['roota']

    self.toe = in_hash['toe']
    self.cic = in_hash['cic']
    self.omega_c = in_hash['omega_c']
    self.cis = in_hash['cis']

    self.i0 = in_hash['i0']
    self.crc = in_hash['crc']
    self.omega = in_hash['omega']
    self.omegadot = in_hash['omegadot']

    self.idot = in_hash['idot']
    self.codes = in_hash['codes']
    self.weekno = in_hash['weekno']
    self.l2flag = in_hash['l2flag']

    self.svaccur = in_hash['svaccur']
    self.svhealth = in_hash['svhealth']
    self.tgd = in_hash['tgd']

    self.tom = in_hash['tom']
    self.fit = in_hash['fit']
    self


  def from_hash(self, in_hash):
    ret = Ephemeris()
    ret.from_hash(in_hash)
    return ret


  def to_s(self):
    return "Ephemeris SV=#{self.svprn}, toc=#{self.toc}, a0=#{self.a0}, ecc=#{self.ecc}, m0=#{self.m0}, roota=#{self.roota}"


  def to_hash(self):
    ret = {}
    ret['a']=[self.a0, self.a1, self.a2, self.a3] # alpha parameters for ionospheric model
    ret['b']=[self.b0, self.b1, self.b2, self.b3] # beta parameters for ionospheric model

    ret['svprn']=self.svprn
    ret['af0']=self.af0
    ret['af1']=self.af1
    ret['af2']=self.af2

    ret['toc']=self.toc # GpsTime

    ret['iode']=self.iode
    ret['crs']=self.crs
    ret['deltan']=self.deltan
    ret['m0']=self.m0

    ret['cuc']=self.cuc
    ret['ecc']=self.ecc
    ret['cus']=self.cus
    ret['roota']=self.roota

    ret['toe']=self.toe
    ret['cic']=self.cic
    ret['omega_c']=self.omega_c
    ret['cis']=self.cis

    ret['i0']=self.i0
    ret['crc']=self.crc
    ret['omega']=self.omega
    ret['omegadot']=self.omegadot

    ret['idot']=self.idot
    ret['codes']=self.codes
    ret['weekno']=self.weekno
    ret['l2flag']=self.l2flag

    ret['svaccur']=self.svaccur
    ret['svhealth']=self.svhealth
    ret['tgd']=self.tgd

    ret['tom']=self.tom
    ret['fit']=self.fit
    return ret


  #\brief Find the clock correction from UTC for this SV
  #     This is done ignoring leap seconds. In other words, this is modulo 1 second.
  #
  def clock_correct(self, sow):
    # more accurate calculation
    e = self.getE(self.Tsv)
    dt = Util.check_t(self.Tsv - self.toc.sow())
    dtr = -4.442807e-10 * self.ecc * self.roota * math.sin(e)   #relatavistic correction
    dtsv = self.af0 + self.af1*(dt) + self.af2*dt*dt + dtr - self.tgd
    return dtsv


  def get_tk(self, sow):
    tk = Util.check_t(sow-self.toe) #    Time from ephemeris reference epoch (1)
    if (tk < -302400.0):
      raise "Invalid time %f" % tk
    if (tk > 302400.0):
      raise "Invalid time %f" % tk
    return tk

  # http://home-2.worldonline.nl/~samsvl/satpos.htm
  # Get the eccentricity
  def getE0(self, sow):

    x=self.m0       # kepler's equation for eccentric anomaly ek
    y=self.m0 - (x-self.ecc*math.sin(x))
    x1=x
    x=y
    for i in range(0,15):
      x2=x1
      x1=x
      y1=y
      y = self.m0 - (x- self.ecc*math.sin(x))
      if (abs(y-y1)<1.0e-15):
        break
      x=(x2*y-x*y1)/(y-y1)
    ek=x    # end of det. of ecc. anomaly

    tk = self.get_tk(sow)
    n0 = math.sqrt(Ephemeris.GM/(self.roota**6)) #    Computed mean motion
    n = n0+self.deltan            #    Corrected mean motion
    mk=self.m0+n*tk  #  mean anomaly

    x=mk  #  kepler's equation for eccentric anomaly ek
    y=mk-(x- self.ecc* math.sin(x))
    x1=x
    x=y
    for i in range(0,15):
      x2=x1
      x1=x
      y1=y
      y=mk-(x- self.ecc* math.sin(x))
      if(abs(y-y1)<1.0E-15):
        break
      x=(x2*y-x*y1)/(y-y1)
    ek=x

    print "  E0 -> ", ek
    return ek

  def getE(self, sow):
    a = self.roota*self.roota
    tk = self.get_tk(sow)
    n0 = math.sqrt(Ephemeris.GM/(a**3)) #    Computed mean motion
    n = n0+self.deltan            #    Corrected mean motion
    m = self.m0+n*tk              #    Mean anomaly

    #test = self.getE0(sow)
    m = Util.rem2pi(m + Util.PI2)
    e = m
    for i in range(0,15):
      e_old = e
      e = m + self.ecc*math.sin(e)
      dE = Util.rem2pi(e - e_old)
      if (abs(dE) < 1.0e-15):
        break

    e = Util.rem2pi(e + Util.PI2)
    return e


  def get_sv_position(self, gt):
    return self.get_location(gt.sow())

  def get_sv_position_utc(self, utc_datetime):
    gpst = gps_time.GpsTime.from_time(utc_datetime)
    return self.get_location(gpst.sow())

  def get_location(self, sow):
    a = self.roota*self.roota    # Semi major axis
    tk = self.get_tk(sow)

    e = self.getE(sow)

    v = math.atan2(math.sqrt(1.0-(self.ecc**2))*math.sin(e), math.cos(e)-self.ecc)
    phi = v+self.omega
    phi = Util.rem2pi(phi)
    phi2 = 2.0*phi

    cosphi2 = math.cos(phi2)
    sinphi2 = math.sin(phi2)

    u = phi + self.cuc*cosphi2+self.cus*sinphi2
    r = a*(1.0-self.ecc*math.cos(e)) + self.crc*cosphi2+self.crs*sinphi2
    i = self.i0+self.idot*tk + self.cic*cosphi2+self.cis*sinphi2
    om = self.omega_c + (self.omegadot - Ephemeris.WGS84_EARTH_ROTATION_RATE)*tk - Ephemeris.WGS84_EARTH_ROTATION_RATE*self.toe
    om = Util.rem2pi(om + Util.PI2)
    x1 = math.cos(u)*r
    y1 = math.sin(u)*r

    x = x1*math.cos(om) - y1*math.cos(i)*math.sin(om)
    y = x1*math.sin(om) + y1*math.cos(i)*math.cos(om)
    z = y1*math.sin(i)
    return [x,y,z]

  def get_velocity(self, sow):
    loc1 = np.array(self.get_location(sow - 0.5))
    loc2 = np.array(self.get_location(sow + 0.5))
    return (loc1 - loc2)
