# A ruby XMLRPC server that returns the BRDC data for a given date

$LOAD_PATH.unshift File.dirname(__FILE__)
require 'xmlrpc/server'
require 'logger'
require 'open-uri'
require 'fileutils'

require 'Geography'
require 'Ephemerides'
require 'Sp3Interpolator'
require 'GpsTime'

def uncompress(file)
  cmd = "uncompress --force #{file}"
  puts "command : #{cmd}"
  system(cmd)
end

def read_local_file(filename)
  ret = ""
  begin
    file = File.new(filename, "r")
    while (line = file.gets)
      ret = ret + line
    end
    file.close
  rescue => err
    puts "Exception: #{err}"
    err
  end
  return ret
end

def get_gps_time(date)
  utc_date = Time.parse(date.to_s).utc()
  gt = GpsTime.new(utc_date.year(), utc_date.month(), utc_date.day(), utc_date.hour(), utc_date.min(), utc_date.sec())
end
  
def download_file(url, local_file, attempt_list)
  puts "download_file (#{url} -> #{local_file}"
  last_try = attempt_list[url]
  if last_try and (Time.now - last_try < 3600)
    puts "Already attempted  #{url} in the last hour"
    raise "Error (#{url} -> #{local_file}: Already attempted in the last hour"
  end
  attempt_list[url] = Time.now
  begin
    FileUtils.mkdir_p(File.dirname(local_file))
    open(url) {|src| open(local_file,"wb") {|dst|      dst.write(src.read)  }}
  rescue => err
    raise "Error (#{url} -> #{local_file}: #{err}"
  end
end

# . The file ftp://cddis.gsfc.nasa.gov/pub/gps/data/daily/YYn/brdcDDD0.YYn.Z 
#  (YY is the two-digit year, DDD is the three-digit day of year) is the daily GPS broadcast ephemeris file. 

class BrdcHandler
  def initialize
    @brdc_root = "/freenas/brdc"

    # Keep a list of last download attempts to avoid spamming the NASA server
    # This should be a hash of the URL and attempt
    @last_download_attempt = Hash.new
    @cache = Hash.new
  end

  def get_url(path)
    return "ftp://cddis.gsfc.nasa.gov/gps/data/#{path}"
  end
  
  def get_hourly_filename(in_utc)
    utc_date = in_utc - 3600.0  # Try a file that is one hour old
    ddd = "%.3d" % utc_date.yday()
    yy = "%.2d" % (utc_date.year()-2000)
    yyyy = utc_date.year()
    path = "hourly/#{yyyy}/#{ddd}/hour#{ddd}0.#{yy}n"
  end
  
  def get_daily_filename(utc_date)
    doy = "%.3d" % utc_date.yday()
    yy = "%.2d" % (utc_date.year()-2000)
    yyyy = utc_date.year()
    path = "daily/#{yyyy}/brdc/brdc#{doy}0.#{yy}n"
  end
      
  
  
  def get_brdc_aux(utc_date)
    # Return the BRDC data
    #puts "get_brdc_aux(#{utc_date.to_s})"
    if (utc_date > Time.now) 
      return "Time #{utc_date} from #{date.to_s} is in the future"
    end
    
    use_hourly = false
    begin
      daily_file = get_daily_filename(utc_date)
      daily_path = "#{@brdc_root}/#{daily_file}"
      if (File.exists?(daily_path))
        #puts "Using local file #{daily_path}"
        return read_local_file(daily_path)
      else
        url = "#{get_url(daily_file)}.Z"
        download_file(url, "#{daily_path}.Z", @last_download_attempt)
        uncompress("#{daily_path}.Z")
        return read_local_file(daily_path)
      end
    rescue => err
      use_hourly = true
    end
    
    if use_hourly
      hourly_file = get_hourly_filename(utc_date)
      hourly_path = "#{@brdc_root}/#{hourly_file}"
      #puts "getting hourly file #{hourly_path}"
      if (File.exists?(hourly_path))
        return read_local_file(hourly_path)
      else
        # Download the remote filename
        url = "#{get_url(hourly_file)}.Z"
        download_file(url, "#{hourly_path}.Z", @last_download_attempt);
        uncompress("#{hourly_path}.Z")
        return read_local_file(hourly_path)
      end
    end
    return "data not found #{utc_date} : #{daily_file}"
  end
  
  def get_brdc(date)
    utc_date = Time.parse(date.to_s).utc()
    return get_brdc_aux(utc_date)
  end
  
  def get_ephemeris(date, sv)
    utc_date = Time.parse(date.to_s).utc()
    # Keep a cache of the last N ephemerides that have been found indexed by date - accurate to the day
    key = "#{utc_date.year()}#{utc_date.month()}#{utc_date.day()}}"
    eds = @cache[key]
    
    if (eds == nil)
      brdc = self.get_brdc_aux(utc_date)
      eds = Ephemerides.new(brdc)
      @cache[key] = eds
    end
    gt = get_gps_time(utc_date)
    
    eph = eds.get_ephemeris(sv, gt)
  end
  
  def get_sv_position(date, sv)
    eph = self.get_ephemeris(date,sv)
    gt = get_gps_time(date)
    return eph.get_sv_position(gt)
  end

  def get_sv_positions(date)
    ret = []
    gt = get_gps_time(date)
    1.upto(32).each do |sv|
      begin
        eph = get_ephemeris(date,sv)
        ret << [sv, eph.get_sv_position(gt)]
      rescue Exception => e
        puts e
      end
    end
    return ret 
  end

  def get_all_ephemeris(date)
    ret = []
    1.upto(32).each do |sv|
      begin
        eph = get_ephemeris(date,sv)
        ret << [sv, eph]
      rescue Exception => e
        puts e
      end
    end
    return ret 
  end

  def get_sv_azel(date, lat, lon, alt)
    ret = []
    pos = self.get_sv_positions(date)
    geo = Geography.new(Angle.from_deg(lat), Angle.from_deg(lon), alt)
    pos.each do |sv, ecef|
      x = ecef[0]
      y = ecef[1]
      z = ecef[2]
      r,el,az = geo.get_alt_az(x,y,z)
      ret << [sv, [r,el,az]]
    end
    return ret
  end


end

# Get sp3 files from the corrected orbits ftp://cddis.gsfc.nasa.gov/pub/gps/products/1172/
# We are after igrwwwwd.sp3.Z
class Sp3Handler
  def initialize
    @sp3_root = "/freenas/igr"

    # Keep a list of last download attempts to avoid spamming the NASA server
    # This should be a hash of the URL and attempt
    @last_download_attempt = Hash.new
    @cache = Hash.new
  end

  def get_url(path)
    return "ftp://cddis.gsfc.nasa.gov/pub/gps/products/#{path}"
  end
  
  def get_rapid_filename(week, day_of_week)
    wwww = "%.4d" % week # Gps week
    d = "%.1d" % day_of_week # GPS Day of week
    path = "#{wwww}/igr#{wwww}#{d}.sp3"
  end
  
  def get_final_filename(week, day_of_week)
    wwww = "%.4d" % week # Gps week
    d = "%.1d" % day_of_week # GPS Day of week
    path = "#{wwww}/igs#{wwww}#{d}.sp3"
  end
  
  def get_sp3(date)
    gt = get_gps_time(date)
    interp = get_sp3_gpsdate(gt)
    return interp.get_points()
  end
    
  def get_sp3_gpsdate(gt)
    key = "#{gt.week()}#{gt.day_of_week()}"
    interp = @cache[key]
    return interp if (interp != nil)
    
   # Previous Day
    pweek = gt.week()
    pday = gt.day_of_week() - 1
    if (pday < 0)
      pday += 7
      pweek -= 1
    end
    prev_day = get_sp3_cache(pweek, pday)
    # Currrent Day
    today = get_sp3_cache(gt.week(), gt.day_of_week())
    # Next Day
    nweek = gt.week()
    nday = gt.day_of_week() + 1
    if (nday > 6)
      nday -= 7
      nweek += 1
    end
    next_day = get_sp3_cache(nweek, nday)
    interp = Sp3Interpolator.new(gt, prev_day, today, next_day)
    @cache[key] = interp
    return interp
  end

  def get_sp3_cache(week, day_of_week)
    # Keep a cache of the last N ephemerides that have been found indexed by date - accurate to the day
    sp3 = self.download_sp3(week, day_of_week)
    return sp3
  end

  def download_sp3(week, day_of_week)
    # Return the sp3 data
    
    use_rapid = false
    begin
      final_file = get_final_filename(week, day_of_week)
      final_path = "#{@sp3_root}/#{final_file}"
      if (File.exists?(final_path))
        # puts "Using local file #{final_path}"
        return read_local_file(final_path)
      else
        url = "#{get_url(final_file)}.Z"
        download_file(url, "#{final_path}.Z", @last_download_attempt)
        uncompress("#{final_path}.Z")
        return read_local_file(final_path)
      end
      rescue => err
      use_rapid = true
    end
    
    if use_rapid
      rapid_file = get_rapid_filename(week, day_of_week)
      rapid_path = "#{@sp3_root}/#{rapid_file}"
      # puts "getting rapid file #{rapid_path}"
      if (File.exists?(rapid_path))
        return read_local_file(rapid_path)
      else
        # Download the remote filename
        url = "#{get_url(rapid_file)}.Z"
        download_file(url, "#{rapid_path}.Z", @last_download_attempt);
        uncompress("#{rapid_path}.Z")
        return read_local_file(rapid_path)
      end
    end
     return "sp3 data not found #{week}, #{day_of_week}"
  end


  def get_sv_position(date, sv)
    gt = get_gps_time(date)
    interp = self.get_sp3_gpsdate(gt)
    return interp.get_sv_position(gt, sv)
  end

  def get_sv_positions(date)
    ret = []
    gt = get_gps_time(date)
    interp = get_sp3_gpsdate(gt)
    1.upto(32).each do |sv|
      begin
        ret << [sv, interp.get_sv_position(gt, sv)]
      rescue Exception => e
        puts e
      end
    end
    return ret 
  end

end

require 'jimson'

class RackServer
    extend Jimson::Handler 

  def initialize()
    @brdc = BrdcHandler.new
    @sp3 = Sp3Handler.new
  end
  
  # Sp3 routines (using interpolation)
  def get_sv_position_sp3(date, sv)
    @sp3.get_sv_position(date,sv)
  end
  
  def get_sv_positions_sp3(date)
    @sp3.get_sv_positions(date)
  end

  def get_interp_points(date)
    @sp3.get_sp3(date)
  end

  # BRDC routines (using ephemerides)
  def get_sv_position(date, sv)
    @brdc.get_sv_position(date,sv)
  end
  
  def get_sv_positions(date)
    @brdc.get_sv_positions(date)
  end

  def get_all_ephemeris(date)
    @brdc.get_all_ephemeris(date)
  end

  def get_sv_azel(date, lat, lon, alt)
    @brdc.get_sv_azel(date, lat, lon, alt)
  end
  
  def get_ephemeris(date, sv)
    eph = @brdc.get_ephemeris(date,sv)
    return eph.to_hash()
  end

end
server = Jimson::Server.new(RackServer.new, opts = {:port => 8876, :server => 'thin'})
server.start # serve with webrick on http://0.0.0.0:8999/
# Run via Rack. see config.ru and Makefile
