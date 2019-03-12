# Copyright (C) Tim Molteno 2008-2013. All rights reserved 

require 'Util'

class Ephemeris
  GM = 3.986005e14  # earth's universal gravitational parameter m^3/s^2
  EARTH_ROTATION_RATE = 7.2921151467e-5;  # earth rotation rate, rad/s

  attr_accessor :a0, :a1, :a2, :a3 # alpha parameters for ionospheric model
  attr_accessor :b0, :b1, :b2, :b3 # beta parameters for ionospheric model

  attr_accessor :svprn
  attr_accessor :af0 # clock_bias (seconds)
  attr_accessor :af1 # clock_drift (seconds per second)
  attr_accessor :af2 # clock_drift_rate (seconds per second^2

# 
#     # The corrections of the satellite time to UTC are as follows:
#     # Tutc = Tsv - af0  -  af1  *(Tsv-Toc) - ... -  A0  - ... - leap_sec
# 
  attr_accessor :toc # GpsTime

  attr_accessor :iode
  attr_accessor :crs
  attr_accessor :deltan
  attr_accessor :m0
  
  attr_accessor :cuc
  attr_accessor :ecc
  attr_accessor :cus
  attr_accessor :roota

  attr_accessor :toe
  attr_accessor :cic
  attr_accessor :omega_c
  attr_accessor :cis

  attr_accessor :i0
  attr_accessor :crc
  attr_accessor :omega
  attr_accessor :omegadot

  attr_accessor :idot
  attr_accessor :codes
  attr_accessor :weekno
  attr_accessor :l2flag

  attr_accessor :svaccur  # SV accuracy      (meters)
  attr_accessor :svhealth # SV health        (bits 17-22 w 3 sf 1)
  attr_accessor :tgd # Group delay (seconds) (differential between L1 and L2)

  attr_accessor :tom
  attr_accessor :fit

  def initialize()
  end
  
  def from_hash(in_hash)
    @a0, @a1, @a2, @a3 = in_hash['a'] # alpha parameters for ionospheric model
    @b0, @b1, @b2, @b3 = in_hash['b'] # beta parameters for ionospheric model

    @svprn = in_hash['svprn']
    @af0 = in_hash['af0']
    @af1 = in_hash['af1']
    @af2 = in_hash['af2']

    @toc = in_hash['toc'] # GpsTime

    @iode = in_hash['iode']
    @crs = in_hash['crs']
    @deltan = in_hash['deltan']
    @m0 = in_hash['m0']
    
    @cuc = in_hash['cuc']
    @ecc = in_hash['ecc']
    @cus = in_hash['cus']
    @roota = in_hash['roota']

    @toe = in_hash['toe']
    @cic = in_hash['cic']
    @omega_c = in_hash['omega_c']
    @cis = in_hash['cis']

    @i0 = in_hash['i0']
    @crc = in_hash['crc']
    @omega = in_hash['omega']
    @omegadot = in_hash['omegadot']

    @idot = in_hash['idot']
    @codes = in_hash['codes']
    @weekno = in_hash['weekno']
    @l2flag = in_hash['l2flag']

    @svaccur = in_hash['svaccur'] 
    @svhealth = in_hash['svhealth']
    @tgd = in_hash['tgd']

    @tom = in_hash['tom']
    @fit = in_hash['fit']
    self
  end
  
  def self.from_hash(in_hash)
    ret = Ephemeris.new()
    ret.from_hash(in_hash)
    return ret
  end
  
  def to_s
    "GPS Ephemeris SV=#{@svprn}, toc=#{@toc}, a0=#{@a0}, ecc=#{@ecc}, m0=#{@m0}, roota=#{@roota}"
  end
  
  def to_hash
    ret = Hash.new
    ret['a']=[@a0, @a1, @a2, @a3] # alpha parameters for ionospheric model
    ret['b']=[@b0, @b1, @b2, @b3] # beta parameters for ionospheric model

    ret['svprn']=@svprn
    ret['af0']=@af0
    ret['af1']=@af1
    ret['af2']=@af2

    ret['toc']=@toc # GpsTime

    ret['iode']=@iode
    ret['crs']=@crs
    ret['deltan']=@deltan
    ret['m0']=@m0
    
    ret['cuc']=@cuc
    ret['ecc']=@ecc
    ret['cus']=@cus
    ret['roota']=@roota

    ret['toe']=@toe
    ret['cic']=@cic
    ret['omega_c']=@omega_c
    ret['cis']=@cis

    ret['i0']=@i0
    ret['crc']=@crc
    ret['omega']=@omega
    ret['omegadot']=@omegadot

    ret['idot']=@idot
    ret['codes']=@codes
    ret['weekno']=@weekno
    ret['l2flag']=@l2flag

    ret['svaccur']=@svaccur 
    ret['svhealth']=@svhealth
    ret['tgd']=@tgd

    ret['tom']=@tom
    ret['fit']=@fit
    return ret
  end
  
  #\brief Find the clock correction from UTC for this SV
  #	This is done ignoring leap seconds. In other words, this is modulo 1 second.
  #
  def clock_correct(sow)
    # more accurate calculation
    e = self.getE(@Tsv)
    dt = Util.check_t(@Tsv - @toc.sow())
    dtr = -4.442807e-10 * @ecc * @roota * Math.sin(e)	#relatavistic correction
    dtsv = @af0 + @af1*(dt) + @af2*dt*dt + dtr - @tgd
    return dtsv
  end

  # http://home-2.worldonline.nl/~samsvl/satpos.htm
  # Get the eccentricity
  def getE(sow)
    a = @roota*@roota
    tk = Util.check_t(sow-@toe) #    Time from ephemeris reference epoch (1)
    n0 = Math.sqrt(GM/(a**3)) #    Computed mean motion
    n = n0+@deltan            #    Corrected mean motion
    m = @m0+n*tk              #    Mean anomaly

    m = Util.rem2pi(m + Util::PI2)
    e = m
    12.times do |i|
      e_old = e
      e = m + @ecc*Math.sin(e)
      dE = Util.rem2pi(e - e_old)
      break if (dE.abs() < 1.0e-15) 
    end
    e = Util.rem2pi(e + Util::PI2)
    return e
  end

  def get_sv_position(gt)
    return get_location(gt.sow())
  end
  
  def get_sv_velocity(gt)
    return get_velocity(gt.sow())
  end
  
  def get_location(sow)
    a = @roota*@roota    # Semi major axis
#     puts "roota=#{@roota}, a=#{a}"
    tk = Util.check_t(sow-@toe)

    e = self.getE(sow)

    v = Math.atan2(Math.sqrt(1.0-(@ecc**2))*Math.sin(e), Math.cos(e)-@ecc)
    phi = v+@omega
    phi = Util.rem2pi(phi)
    phi2 = 2.0*phi
    
    cosphi2 = Math.cos(phi2)
    sinphi2 = Math.sin(phi2)
    
    u = phi + @cuc*cosphi2+@cus*sinphi2
    r = a*(1.0-@ecc*Math.cos(e)) + @crc*cosphi2+@crs*sinphi2
    i = @i0+@idot*tk + @cic*cosphi2+@cis*sinphi2
    om = @omega_c + (@omegadot - EARTH_ROTATION_RATE)*tk - EARTH_ROTATION_RATE*@toe
    om = Util.rem2pi(om + Util::PI2)
    x1 = Math.cos(u)*r
    y1 = Math.sin(u)*r

    x = x1*Math.cos(om) - y1*Math.cos(i)*Math.sin(om)
    y = x1*Math.sin(om) + y1*Math.cos(i)*Math.cos(om)
    z = y1*Math.sin(i)

    puts "Ephemeris:get_location(#{sow}) -> [#{x}}"
    return [x,y,z]
  end
	
	
  def get_velocity(sow)
    loc1 = self.get_location(sow - 0.5)
    loc2 = self.get_location(sow + 0.5)
    v =  [(loc2[0] - loc1[0]), (loc2[1] - loc1[1]), (loc2[2] - loc1[2])]
    return v
  end


end

