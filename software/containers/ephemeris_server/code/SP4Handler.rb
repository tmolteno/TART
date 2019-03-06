$LOAD_PATH.unshift File.dirname(__FILE__)
require 'FileCache'

require 'Geography'
require 'Ephemerides'
require 'GpsTime'

class Sp4Ephemeris
    
    def initialize(local_path)
    end
end

class NORADCache < FileCache
    
    def initialize()
        super("norad_sbas")
        # 
    end

    def get_url(utc_date)
        return "http://www.celestrak.com/NORAD/elements/sbas.txt"
    end

    def create_object_from_file(local_path)
        return Sp4Ephemeris.new(local_path)
    end

end

class SP4Handler
    def initialize
        @cache = NORADCache.new()
    end

    def get_ephemerides(date)
        return @cache.get_object(date)
    end

    def get_prn_list(date)
        eds = self.get_ephemerides(date)
        return eds.get_prn_list()
    end

    def get_sv_positions(date)
        ret = []
        gt = get_gps_time(date)
        prn_list = self.get_prn_list(date)
        prn_list.each do |sv|
        begin
            eph = get_ephemeris(date,sv)
            name = "#{@name} #{sv}"
            ret << {'name': name, 
                    'type': @name, 
                    'ecef': eph.get_sv_position(gt),
                    'ecef_dot': eph.get_sv_velocity(gt)
                    }
        rescue Exception => e
            puts e
        end
        end
        return ret 
    end

    def get_all_ephemeris(date)
        ret = []
        prn_list = get_prn_list(date)
        prn_list.each do |sv|
        begin
            eph = get_ephemeris(date,sv)
            ret << [sv, eph]
        rescue Exception => e
            puts e
        end
        end
        return ret 
    end

    def get_sv_azel(date, lat, lon, alt, threshold=0.0)
        ret = []
        pos = self.get_sv_positions(date)
        geo = Geography.new(Angle.from_deg(lat), Angle.from_deg(lon), alt)
        pos.each do |data|
            ecef = data[:ecef]
            x = ecef[0]
            y = ecef[1]
            z = ecef[2]
            r,el,az = geo.get_alt_az(x,y,z)

            velocity = data[:ecef_dot]
            dx = velocity[0]
            dy = velocity[1]
            dz = velocity[2]
            _r,_el,_az = geo.get_alt_az(x+dx,y+dy,z+dz)
           if (el > threshold)
                ret <<  {'name': data[:name], 
                         'r': display_float(r), 
                         'el': display_float(Angle.rad2deg(el)), 
                         'az': display_float(Angle.wrap360(az)),
                         'r_dot': display_float(_r - r),
                         'el_dot': display_float(_el - el, 7),
                         'az_dot': display_float(_az - az, 7)
                        }
            end
        end
        return ret
    end

end


cache = NORADCache.new()
cache.get_object(Time.now)
