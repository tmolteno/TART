$LOAD_PATH.unshift File.dirname(__FILE__)

require 'xmlrpc/client'
require 'time'
require 'jimson'

require 'Ephemeris'
require 'GpsTime'

URL = 'http://0.0.0.0:8876/rpc/gps'
#URL = 'http://astro.elec.ac.nz:8876/rpc/gps'


class JSONClient
  def initialize
    @client = Jimson::Client.new(URL) # the URL for the JSON-RPC 2.0 server to connect to
  end
  
  def get_ephemeris(date, sv)
    result = @client.get_ephemeris(date.to_s, sv)
    return result
  end

  def get_sv_position(date, sv)
    result = @client.get_sv_position(date.to_s, sv)
    return result
  end
  
  def get_sv_position_sp3(date, sv)
    result = @client.get_sv_position_sp3(date.to_s, sv)
    return result
  end
  
  
  def get_sv_positions(date)
    result = @client.get_sv_positions(date.to_s)
    return result
  end
  def get_sv_azel(date, lat, lon, alt)
    result = @client.get_sv_azel(date.to_s, lat, lon, alt)
    return result
  end
  
  

end


class XMLClient
  def initialize
      @client = XMLRPC::Client.new_from_uri(URL)
  end
  
  def get_brdc(date)
    result = @client.call("tart.get_brdc", date.to_s)
    return result
  end

  def get_sv_position(date, sv)
    result = @client.call("tart.get_sv_position", date.to_s, sv)
    return result
  end

end

def rad2deg(rad)
  return rad*180.0/3.1415
end

foo = JSONClient.new

# Compare to results at http://www.nstb.tc.faa.gov/Full_WaasSatelliteStatus.htm
# More data http://www.calsky.com/?GPS=

if (true)
#  t = Time.now().utc()
  t = Time.new(2013, 4, 4, 0, 0, 0, "+13:00").utc
  puts "Satellite Elevation at #{t}"
  alt_az = foo.get_sv_azel(t, -45.86, 170.5, 123)
  alt_az.each do |sv, altaz|
    puts "#{sv} : az=#{rad2deg(altaz[2])} el=#{rad2deg(altaz[1])}" if altaz[1] > 0.0
  end

  pos = foo.get_sv_positions(t)
  pos.each do |sv, p|
    puts "#{sv} : p=#{p}"
  end
  
  hash = foo.get_ephemeris(t, 1)
  eph = Ephemeris.from_hash(hash)
  puts eph.to_hash
  10000.times do
    t = t + 100.0
    gt = GpsTime.from_time(t)
    eph.get_location(gt.sow())
  end
  gt = GpsTime.from_time(t)
  puts eph.get_location(gt.sow())
  puts foo.get_sv_position_sp3(t, 1)
end


if false
  t = Time.new(2005, 8, 21, 4, 5, 0, "+00:00").utc
  puts t
  puts foo.get_sv_position(t, 11)
    
  t = Time.new(2002, 10, 31, 2, 2, 2, "+00:00").utc
  puts foo.get_sv_positions(t)
  1000.times do
    t = t + 100.0
    puts foo.get_sv_position(t, 1)
  end
#puts foo.get_sv_position(t, 1)
end
