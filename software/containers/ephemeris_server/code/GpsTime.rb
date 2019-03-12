# Copyright (C) Tim Molteno 2008-2013. All rights reserved 

=begin !\brief A GPS time object. http://adn.agi.com/GNSSWeb/Default.aspx

SV Clock and GPS Time stuctures are easy to figure out. GPS Time is kept as Weeks and Seconds from Jan. 5 midnight/Jan. 6 morning of 1980.
This means that we don't have to keep track of UTC leap seconds (or perhaps more accurately, we're not keeping track of them here). This
means that when the date in yyyy, mm, dd format is entered, the conversion to GPS time is done by computing the number of seconds between 
that date and the gps start date.

When converting from gps seconds to UTC we technically should keep track of UTC leap seconds.

TODO. Make the conversion between gps_time and UTC robust to the UTC leap second issue.

=end
require 'time'

class GpsTime  
  SEC_PER_DAY = 86400.0

  # !\brief Construct an invalid gps_time
  #
  def initialize()
  
    @m_week = -9999
    @sec_of_week = nil
    @m_day_of_year = nil
  end

  # !\brief Construct from a UTC year, month, day and hour
  #
  #   !\brief Conversion of Julian Day number to GPS week and Seconds of Week reckoned from Saturday midnight
  # 	\param y Year as a four digit number YYYY
  # 	\param m Month 1-12
  # 	\param d Day of Month 1-31
  # 	\param h Hour 0-23
  # 	\param minutes Minutes 0-59
  # 	\param seconds Seconds 0-60
  # 
  # GPS Time is a uniformly counting time scale. GPS time was born at the 1/5/1980 to 1/6/1980 midnight. January 6, 1980 is a Sunday. 
  # GPS Time counts in weeks and seconds of a week from this instant. The weeks begin at the Saturday/Sunday transition.
  # The days of the week are numbered, with Sunday being 0, Saturday is day 6. GPS week 0 began at the beginning of the GPS Time Scale. 
  # Within each week the time is usually denoted as the second of the week. This is a number between 0 and 604,800 ( 60 x 60 x 24 x 7).
  def initialize(y, m, d, h, minutes, seconds)
    
    throw Exception.new("Year #{y} must >= 1980") if (y < 1980)


    in_time = Time.utc(year=y, month=m, day=d, hour=h, min=minutes, sec_with_frac=0)	
    # This is broken if we use -1 because if there is no local daylight savings time, then this fails.
    #in_time.tm_isdst = -1;

    gps_t = Time.utc(year=1980, month=1, day=6, hour=0, min=0, sec_with_frac=0)

    @m_day_of_year = in_time.yday;
    
    
    gps_sec = ( in_time - gps_t ) + seconds + self.utc_offset(in_time)
    gps_days = (gps_sec / SEC_PER_DAY).floor.to_i
    gps_weeks = gps_days / 7.0;
    
    @m_week = (gps_weeks).floor.to_i
    @sec_of_week = gps_sec - @m_week*7.0*SEC_PER_DAY;
  end

  def self.from_time(utc_date)
    return GpsTime.new(utc_date.year(), utc_date.month(), utc_date.day(), utc_date.hour(), utc_date.min(), utc_date.sec())
  end

  # I don't think that the UTC offset works correctly.
  def utc_offset(t)
    return 18 if (t >= Time.utc(2017, 1, 1))
    return 17 if (t >= Time.utc(2015, 7, 1))
    return 16 if (t >= Time.utc(2012, 7, 1))
    return 15 if (t >= Time.utc(2009, 1, 1))
    return 14 if (t >= Time.utc(2006, 1, 1))
    return 13 if (t >= Time.utc(1999, 1, 1))
    return 12 if (t >= Time.utc(1997, 7, 1))
    return 11 if (t >= Time.utc(1996, 1, 1))
    return 10 if (t >= Time.utc(1994, 7, 1))
    return 9 if (t >= Time.utc(1993, 7, 1))
    return 8 if (t >= Time.utc(1992, 7, 1))
    return 7 if (t >= Time.utc(1991, 1, 1))
    return 6 if (t >= Time.utc(1990, 1, 1))
    return 5 if (t >= Time.utc(1988, 1, 1))
    return 4 if (t >= Time.utc(1985, 7, 1))
    return 3 if (t >= Time.utc(1983, 7, 1))
    return 2 if (t >= Time.utc(1982, 7, 1))
    return 1 if (t >= Time.utc(1981, 7, 1))
    return 0 if (t >= Time.utc(1980, 1, 1))
    return nil
  end
  
  def to_s
    "W:#{@m_week}_SOW:#{@sec_of_week}"
  end
  
  def diff(b)
    #print "GpsTime(#{self}).diff(#{b})"
    w = (@m_week - b.week()) * 7 * SEC_PER_DAY
    s = (@sec_of_week - b.sow())
    return w + s
  end

  def week()
    return @m_week
  end

  def sow()
    return @sec_of_week
  end

  def day_of_year()
    return @m_day_of_year
  end
  
  def day_of_week()
    dow = @sec_of_week / SEC_PER_DAY
    return dow.floor.to_i
  end
  
  def yesterday()
    sow = @sec_of_week - SEC_PER_DAY
    week = @m_week
    if (sow < 0)
      sow += 7*SEC_PER_DAY
      week -= 1
    end
    [week, sow]
  end
  
  def tomorrow()
    sow = @sec_of_week + SEC_PER_DAY
    week = @m_week
    if (sow > 7*SEC_PER_DAY)
      sow -= 7*SEC_PER_DAY
      week += 1
    end
    [week, sow]
  end
  

end

def get_gps_time(date)
  utc_date = Time.parse(date.to_s).utc()
  gt = GpsTime.new(utc_date.year(), utc_date.month(), utc_date.day(), utc_date.hour(), utc_date.min(), utc_date.sec())
end
