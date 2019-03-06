$LOAD_PATH.unshift File.dirname(__FILE__)

=begin Parse the Broadcast ephemerides file.

COMMENT             | Comment line(s)                          |     A60    |*
 +--------------------+------------------------------------------+------------+
*|ION ALPHA           | Ionosphere parameters A0-A3 of almanac   |  2X,4D12.4 |*
 |                    | (page 18 of subframe 4)                  |            |
 +--------------------+------------------------------------------+------------+
*|ION BETA            | Ionosphere parameters B0-B3 of almanac   |  2X,4D12.4 |*
 +--------------------+------------------------------------------+------------+
*|DELTA-UTC: A0,A1,T,W| Almanac parameters to compute time in UTC| 3X,2D19.12,|*
 |                    | (page 18 of subframe 4)                  |     2I9    |
 |                    | A0,A1: terms of polynomial               |            |
 |                    | T    : reference time for UTC data       |      *)    |
 |                    | W    : UTC reference week number.        |            |
 |                    |        Continuous number, not mod(1024)! |            |
 +--------------------+------------------------------------------+------------+
*|LEAP SECONDS        | Delta time due to leap seconds           |     I6     |*
 +--------------------+------------------------------------------+------------+
 |END OF HEADER       | Last record in the header section.       |    60X     |


For each ephemeris, the files is stored in RINEX format. Here is the data

 +----------------------------------------------------------------------------+
 |                                  TABLE A4                                  |
 |           GPS NAVIGATION MESSAGE FILE - DATA RECORD DESCRIPTION            |
 +--------------------+------------------------------------------+------------+
 |    OBS. RECORD     | DESCRIPTION                              |   FORMAT   |
 +--------------------+------------------------------------------+------------+
 |prn / EPOCH / SV CLK| - Satellite prn number                   |     I2,    |
 |                    | - Epoch: Toc - Time of Clock             |            |
 |                    |          year (2 digits, padded with 0   |            |
 |                    |                if necessary)             |  1X,I2.2,  |
 |                    |          month                           |   1X,I2,   |
 |                    |          day                             |   1X,I2,   |
 |                    |          hour                            |   1X,I2,   |
 |                    |          minute                          |   1X,I2,   |
 |                    |          second                          |    F5.1,   |
 |                    | - SV clock bias       (seconds)          |  3D19.12   |
 |                    | - SV clock drift      (sec/sec)          |            |
 |                    | - SV clock drift rate (sec/sec2)         |     *)     |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 1| - iode Issue of Data, Ephemeris          | 3X,4D19.12 |
 |                    | - Crs                 (meters)           |            |
 |                    | - Delta n             (radians/sec)      |            |
 |                    | - M0                  (radians)          |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 2| - Cuc                 (radians)          | 3X,4D19.12 |
 |                    | - e Eccentricity                         |            |
 |                    | - Cus                 (radians)          |            |
 |                    | - sqrt(A)             (sqrt(m))          |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 3| - Toe Time of Ephemeris                  | 3X,4D19.12 |
 |                    |                       (sec of GPS week)  |            |
 |                    | - Cic                 (radians)          |            |
 |                    | - OMEGA               (radians)          |            |
 |                    | - CIS                 (radians)          |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 4| - i0                  (radians)          | 3X,4D19.12 |
 |                    | - Crc                 (meters)           |            |
 |                    | - omega               (radians)          |            |
 |                    | - OMEGA DOT           (radians/sec)      |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 5| - IDOT                (radians/sec)      | 3X,4D19.12 |
 |                    | - Codes on L2 channel                    |            |
 |                    | - GPS Week # (to go with TOE)            |            |
 |                    |   Continuous number, not mod(1024)!      |            |
 |                    | - L2 P data flag                         |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 6| - SV accuracy         (meters)           | 3X,4D19.12 |
 |                    | - SV health        (bits 17-22 w 3 sf 1) |            |
 |                    | - TGD                 (seconds)          |            |
 |                    | - IODC Issue of Data, Clock              |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 7| - Transmission time of message       **) | 3X,4D19.12 |
 |                    |         (sec of GPS week, derived e.g.   |            |
 |                    |    from Z-count in Hand Over Word (HOW)  |            |
 |                    | - Fit interval        (hours)            |            |
 |                    |         (see ICD-GPS-200, 20.3.4.4)      |            |
 |                    |   Zero if not known                      |            |
 |                    | - spare                                  |            |
 |                    | - spare                                  |            |
 +--------------------+------------------------------------------+------------+

=end

require 'Ephemeris'
require 'GpsTime'

class Ephemerides
  
  def initialize(data)
    # Remove the silly use of D for the exponent
    d2 = data.gsub(/D([\+\-][0-9][0-9])/, 'E\1')
    @lines = d2.split("\n")
    @ephemerides = []
    self.parse_brdc()
  end
  
  def line_parse(line)
      a = line.slice(3,19).to_f
      b = line.slice(22,19).to_f
      c = line.slice(41,19).to_f
      d = line.slice(60,19).to_f
      return [a,b,c,d]
  end
  
  def parse_brdc

    @ephemerides = []
    #scan until we get a line that contains END OF HEADER


    #     search header for ionosphere model parameters...
    # 	      1         2         3         4         5         6         7         8
    #     012345678901234567890123456789012345678901234567890123456789012345678901234567890
    # 	2              NAVIGATION DATA                         RINEX VERSION / TYPE
    #     CCRINEXN V1.6.0 UX  CDDIS               11-APR-10 02:51     PGM / RUN BY / DATE 
    #     IGS BROADCAST EPHEMERIS FILE                                COMMENT             
    # 	0.1118E-07  0.1490E-07 -0.5960E-07 -0.5960E-07          ION ALPHA           
    # 	0.8806E+05  0.1638E+05 -0.1966E+06 -0.1311E+06          ION BETA            
    #       -0.279396772385E-08-0.266453525910E-14    61440     1579 DELTA-UTC: A0,A1,T,W
    # 	15                                                      LEAP SECONDS        

    loop_counter = 0
    while (loop_counter < 100)
      str = @lines.shift
      if (str.index("ION ALPHA") != nil)
        a0 = (str.slice(3,12)).to_f();
        a1 = (str.slice(15,12)).to_f();
        a2 = (str.slice(27,12)).to_f();
        a3 = (str.slice(39,12)).to_f();
      end
      if (str.index("ION BETA") != nil)
        b0 = (str.slice(3,12)).to_f();
        b1 = (str.slice(15,12)).to_f();
        b2 = (str.slice(27,12)).to_f();
        b3 = (str.slice(39,12)).to_f();
      end
      break if (str.index("END OF HEADER") != nil)
      loop_counter += 1
    end

    while true
      eph = Ephemeris.new
      eph.a0 = a0
      eph.a1 = a1
      eph.a2 = a2
      eph.a3 = a3
      eph.b0 = b0
      eph.b1 = b1
      eph.b2 = b2
      eph.b3 = b3
     # read in the ephemeris data...
      #           1         2         3         4         5         6         7         8
      # 012345678901234567890123456789012345678901234567890123456789012345678901234567890
      #  2 08  9 12  0  0  0.0 0.188095495105E-03-0.284217094304E-11 0.000000000000E+00
      #     0.670000000000E+02 0.192500000000E+02 0.537165232231E-08-0.256690701048E+01
      #     0.110641121864E-05 0.875317445025E-02 0.500865280628E-05 0.515376602745E+04
      #     0.432000000000E+06-0.141561031342E-06-0.205415914935E+01 0.109896063805E-06
      #     0.942551693336E+00 0.274468750000E+03 0.263802852256E+01-0.833356141200E-08
      #     0.553594488004E-10 0.100000000000E+01 0.149600000000E+04 0.000000000000E+00
      #     0.200000000000E+01 0.000000000000E+00-0.172294676304E-07 0.670000000000E+02


      line = @lines.shift
      break if line == nil
      break if (line.length() < 32)
	      

      svprn = line.slice(0,2).to_i

      year = line.slice(3,2).to_i
      month = line.slice(6,2).to_i
      day = line.slice(9,2).to_i
      hour = line.slice(12,2).to_i
      minute = line.slice(15,2).to_i
      second = line.slice(18,4).to_f

      eph.toc = GpsTime.new(2000+year, month, day, hour, minute, second)
      
      eph.svprn = svprn
      eph.af0 = line.slice(22,19).to_f
      eph.af1 = line.slice(41,19).to_f
      eph.af2 = line.slice(60,19).to_f
      
      eph.iode,    eph.crs,  eph.deltan, eph.m0 = line_parse(@lines.shift)
      eph.cuc,     eph.ecc,  eph.cus,    eph.roota = line_parse(@lines.shift)
      eph.toe,     eph.cic,  eph.omega_c, eph.cis = line_parse(@lines.shift)
      eph.i0,      eph.crc,  eph.omega,  eph.omegadot = line_parse(@lines.shift)
      eph.idot,    eph.codes,eph.weekno, eph.l2flag = line_parse(@lines.shift)
      eph.svaccur, eph.svhealth, eph.tgd, iodc = line_parse(@lines.shift)
      eph.tom,     eph.fit,  spare,      spare2 = line_parse(@lines.shift)
      
      @ephemerides.push(eph)
    end
  end

    def get_prn_list()
        ret = []
        @ephemerides.each do |eph| 
#             puts "sv=#{eph.svprn} health=#{eph.svhealth}" if eph.svhealth > 0.0
            ret << eph.svprn if eph.svhealth < 32.0
        end
        return ret.uniq.sort
    end
    
  #!\brief Get a valid ephemeris for the prn, and GpsTime t
  def get_ephemeris(prn, t)

    fail = true;
    best_e = nil
    
    tmin = 4*86400.0; # number of seconds that the ephemeris should be younger than (4 days)
    @ephemerides.each do |eph| 
      if (eph.svprn == prn)
        dt = t.diff(eph.toc).abs()
        if (dt < tmin) and (eph.svhealth == 0)
          tmin = dt
          best_e = eph;
          fail = false;
        end
      end
    end
    # TODO Check the health status of the received satellite!!!!!
    
    throw Exception.new("No suitable ephemeris found SV=#{prn}, t=#{t}") if (fail)

    #puts "SV #{prn} health = #{best_e.svhealth} accuracy = #{best_e.svaccur}"

    return best_e
  end


end

if __FILE__ == $0
    e = Ephemerides.new()
    e.parse_brdc("brdc2560.08n")

    # find an ephemeris for prn 5, closest to the current time
    gt = GpsTime.new(2008,9,12,13, 45, 0.0)
    puts " julday(2008,9,12,13.45) = #{gt}"

    best_e = e.get_ephemeris(5, gt)

    vector3 sat_loc = best_e.get_location(gt.sec_of_week)
    puts "prn #{best_e.svprn} (#{sat_loc})"
end
