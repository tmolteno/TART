$LOAD_PATH.unshift File.dirname(__FILE__)
require 'Util'

class Angle

  DEG2RAD = Util::PI / 180.0
  RAD2DEG = 180.0 / Util::PI
  
  def initialize(radians)
    @theta = radians
  end

  def self.rad2deg(in_rad)
          return in_rad * RAD2DEG
  end

  def self.wrap360(in_rad)
      d = rad2deg(in_rad)
      d = d + 360.0 if d < 0
      return d
  end

  def self.deg2rad(in_deg)
  
          return in_deg * DEG2RAD
  end

  def self.from_deg(in_deg)
  
          return Angle.new(deg2rad(in_deg))
  end

  def self.from_rad(in_rad)
  
          return Angle.new(in_rad)
  end

  def self.from_dms(degrees, minutes, seconds)

          if (degrees < 0)
                  deg = degrees - minutes/60.0 - seconds/3600.0
          else
                  deg = degrees + minutes/60.0 + seconds/3600.0
          end

          return Angle.new(Angle.deg2rad(deg))
  end

  def radians()
    return @theta
  end

  def degrees()
    return Angle.rad2deg(@theta)
  end
  
  def cos()
    return Math.cos(@theta)
  end

  def to_s
    sign = "+"
    d = a.degrees()

    sign = "-" if (a.degrees() < 0) 
    d = -d if (a.degrees() < 0)
    
    deg = d.floor.to_i

    mind = (d - deg) * 60.0;
    minutes = mind.floor.to_i
    seconds = (mind - minutes) * 60.0;

    return "#{sign}#{deg} #{minutes}'#{seconds}\""
  end
end




