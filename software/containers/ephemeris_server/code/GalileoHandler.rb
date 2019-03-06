$LOAD_PATH.unshift File.dirname(__FILE__)
require 'nokogiri'
require 'GalileoAlmanac'
require 'GpsTime'
require 'BaseHandler'

class GalileoHandler  < BaseHandler
    def initialize
        @name = "GAL"
        @galileo_root = "/freenas/galileo"

        # Keep a list of last download attempts to avoid spamming the Almanac server
        # This should be a hash of the URL and attempt
        @last_download_attempt = Hash.new
        @cache = Hash.new
    end

    def get_url(path)
        return "https://www.gsc-europa.eu/sites/default/files/#{path}"
    end

    def get_file_aux(utc_date)
        # Return the Almanac data
        almanac_file = "2017-06-22.xml" # get_hourly_filename(utc_date)
        file_path = "#{@galileo_root}/#{almanac_file}"
        #puts "getting hourly file #{file_path}"
        if (File.exists?(file_path))
            return (file_path)
        else
            # Download the remote filename
            url = "#{get_url(almanac_file)}"
            download_file(url, file_path, @last_download_attempt);
            return (file_path)
        end
        return "data not found #{utc_date} : #{daily_file}"
    end

    def get_ephemerides(date)
        utc_date = Time.parse(date.to_s).utc()
        fname = get_file_aux(utc_date)
        return GalileoEphemerides.new(fname)
    end

end

# gh = GalileoHandler.new
# pos = gh.get_sv_positions(Time.now.utc)
# alez = gh.get_sv_azel(Time.now.utc, lat=-45.8, lon=170.5, alt=0.0)
# print alez
