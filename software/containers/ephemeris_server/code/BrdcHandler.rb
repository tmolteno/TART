require 'open-uri'
require 'fileutils'

require 'BaseHandler'

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



# . The file ftp://cddis.gsfc.nasa.gov/pub/gps/data/daily/YYn/brdcDDD0.YYn.Z 
#  (YY is the two-digit year, DDD is the three-digit day of year) is the daily GPS broadcast ephemeris file. 

class BrdcHandler < BaseHandler
    def initialize
        @name = "GPS"
        @brdc_root = "./freenas/brdc"

        # Keep a list of last download attempts to avoid spamming the NASA server
        # This should be a hash of the URL and attempt
        @last_download_attempt = Hash.new
        @cache = Hash.new
    end

    def get_url(path)
        return "ftp://cddis.gsfc.nasa.gov/gps/data/#{path}"
    end
  
    def get_hourly_filename(in_utc)
        utc_date = in_utc - 12*3600.0  # Try a file that is one hour old
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
        if (utc_date > Time.now.utc) 
            puts "Time #{utc_date} from #{Time.now.utc} is in the future"
            get_brdc_aux(Time.now.utc)
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
  
    def get_ephemerides(date)
        utc_date = Time.parse(date.to_s).utc()
        # Keep a cache of the last N ephemerides that have been found indexed by date - accurate to the day
        key = "#{utc_date.year()}#{utc_date.month()}#{utc_date.day()}}"
        eds = @cache[key]
        
        if (eds == nil)
            brdc = self.get_brdc_aux(utc_date)
            eds = Ephemerides.new(brdc)
            @cache[key] = eds
        end
        return eds
    end


end
