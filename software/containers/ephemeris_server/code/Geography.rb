$LOAD_PATH.unshift File.dirname(__FILE__)
require 'Angle'

class Vector3
  attr_accessor :x, :y, :z
  def initialize(x,y,z)
    @x = x
    @y = y
    @z = z
  end
  
#   def initialize(x)
#     @x = x[0]
#     @y = x[1]
#     @z = x[2]
#   end
  
  def norm()
    return Math.sqrt(@x*@x + @y*@y + @z*@z)
  end
  
  def /(x)
    x_out = @x / x
    y_out = @y / x
    z_out = @z / x
    return Vector3.new(x_out, y_out, z_out)
  end

  def -(v)
    x_out = @x - v.x
    y_out = @y - v.y
    z_out = @z - v.z
    return Vector3.new(x_out, y_out, z_out)
  end
  
  def to_s
    "(#{@x}, #{@y}, #{@z})"
  end
end

# r = Vector3.new(1,2,3)
# r = r / r.norm()
# puts r

class Geography
# \brief Conversion of geographical coordinates (lat, lon, alt) to Cartesian Earth Centered Earth Fixed coordinates (X, Y, Z). 
#         \param latitude  geocentric latitude 
#         \param longitude geocentric longitude 
#         \param h altitude in meters
#         \return array of ECEF co-ordinates in meters [x,y,z]
#
  R_EARTH = 6378137.0 # earth semimajor axis in meters 
  F_RECIP = 1.0/298.257223563 # reciprocal flattening 
  E2 = 2.0*F_RECIP - F_RECIP*F_RECIP; # eccentricity squared 
  
  def initialize(latitude, longitude, altitude)
    @lat = latitude.radians()
    @lon = longitude.radians()
    @alt = altitude
  end
  
  def get_ecef() 

    sinlat = Math.sin(@lat)
    chi = Math.sqrt(1.0 - E2*sinlat*sinlat)
    coslat = Math.cos(@lat)
    achih = (R_EARTH/chi + @alt)
    x = achih*coslat*Math.cos(@lon)
    y = achih*coslat*Math.sin(@lon)
    z = (R_EARTH*(1.0-E2)/chi + @alt)*sinlat

    return [x,y,z]
  end

   # ECEF to ENU
  def ecef_to_enu(x_in, y_in, z_in)
    e = -x_in*Math.sin(@lon)                + y_in*Math.cos(@lon)
    n = -x_in*Math.cos(@lon)*Math.sin(@lat) - y_in*Math.sin(@lon)*Math.sin(@lat) + z_in*Math.cos(@lat)
    u =  x_in*Math.cos(@lon)*Math.cos(@lat) + y_in*Math.sin(@lon)*Math.cos(@lat) + z_in*Math.sin(@lat)
    return Vector3.new(e,n,u)
  end
  
  # http://www.navipedia.net/index.php/Transformations_between_ECEF_and_ENU_coordinates
  def get_alt_az(x_in, y_in, z_in)

    ex,ey,ez = self.get_ecef() # My position in ECEF
    
    rx,ry,rz = [x_in - ex, y_in - ey, z_in - ez]
    enu = self.ecef_to_enu(rx,ry,rz)
    
    r = enu.norm()
    rho   = enu / r
    
    el = Math.asin(rho.z)
    
    n = rho.y #n
    e = rho.x #e
    
    az = Math.atan2(e, n)

    topo = [r, el, az]
  end
end

if __FILE__ == $0
=begin
  Earth Centered Rotating Coordinates (ECR): 
    Position: Lon   = 171°16'32"     Lat   = -52°27'10"     r     = 26383.87 km   
    Position: X     =-15892.65 km    Y     =  2438.89 km    Z     =-20918.51 km   
    Velocity: VX    =     0.82 km/s  VY    =    -2.57 km/s  VZ    =    -0.93 km/s 
    Velocity: V     =     2.86 km/s 
    Orbital Pole:   Lon   = 199°17'25"     Lat   = +34°09'35"    
  Angle of observer above orbital plane:    6.03° (relative to Earth center)
  Next orbital plane crossing of observer:   4h57m11s (4. April 2013, ascending)
  Satellite above (umbral) Shadow of Earth: s     = 14325.47 km   (in sunlight)
  Satellite above penumbral Shadow of Earth:  s     = 14087.22 km   
  Solar elongation (Sun-Earth-satellite):   elon  =   128.75 °    
  Angle of Sun above orbital plane:         Beta angle=   -50.26 °    

Topocentric:

  Magnitude:     13.1 mag
  Constellation: Centaurus (Cen)
  Altazimuth:    Az    = 175.91° S      Alt   = +81.25°        r     = 20075.51 km   
  Apparent:      R.A.  = 11h12m56s      Dec   = -54°35'40"     r     = 20075.51 km   
  Altaz. airfree:Az    = 175.91° S      Alt   = +81.24°        r     = 20075.51 km   
  Appar. airfree:R.A.  = 11h12m56s      Dec   = -54°35'49"     r     = 20075.51 km   
  J2000 airfree: R.A.  = 11h12m21s      Dec   = -54°31'29"     r     = 20075.51 km   
  Range rate:    dR    =     0.24 km/s 
  Doppler shift: dfd   = -8.100e-07 Hz/Hz
  Angular velocity: vAng  = 0.67 '/s
  Phase:         F     = 80.1% (illumination; illuminated part)
  Elongation from Sun center:  127.04° (Sun below horizon)
  Elongation from Moon center:  92.43° (Moon below horizon)

=end
  geo = Geography.new(Angle.from_deg(-45.5), Angle.from_deg(170.87), 123)
  r, el, az = geo.get_alt_az(-15892.65e3, 2438.89e3, -20918.51e3)
  puts "r=#{r/1.0e3} km"
  puts "az=#{Util.rad2deg(az)} deg"
  puts "el=#{Util.rad2deg(el)} deg"
end
