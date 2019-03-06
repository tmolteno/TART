# Interpolator for sp3 orbit files
require 'matrix'

class Sp3Interpolator
  
  def initialize(gt, yesterday, today, tomorrow)
    @interps = []
    @week = gt.week()
    self.parse_sp3(yesterday)
    self.parse_sp3(today)
    self.parse_sp3(tomorrow)
  end
  
  def get_points()
    return @interps
  end
  #           1         2         3         4         5         6         7         8
  # 012345678901234567890123456789012345678901234567890123456789012345678901234567890
  # P  1 -25146.132348  -1055.530460   8698.159950    235.825095
  def pos_parse(line)
    raise "Invalid Position  #{line}" if (line[0] != 'P')
    sv = line.slice(2,2).to_i
    x = line.slice(5,13).to_f
    y = line.slice(19,13).to_f
    z = line.slice(33,13).to_f
    t = line.slice(47,13).to_f
    return [sv,x,y,z,t]
  end
  
  #           1         2         3         4         5         6         7         8
  # 012345678901234567890123456789012345678901234567890123456789012345678901234567890
  # *  2002  6 25  0  0  0.00000000
  def date_parse(line)
    raise "Invalid Date #{line}" if (line[0] != '*')
    yyyy = line.slice(3,4).to_i
    mm = line.slice(8,2).to_i
    dd = line.slice(11,2).to_i
    hh = line.slice(14,2).to_i
    minute = line.slice(17,2).to_i
    sec = line.slice(20,10).to_f
    return GpsTime.new(yyyy, mm, dd, hh, minute, sec)
  end
  
  #           1         2         3         4         5         6         7         8
  # 012345678901234567890123456789012345678901234567890123456789012345678901234567890
  # ## 1172 259200.00000000   900.00000000 52451 0.0000000000000
  def time_parse(line)
    raise "Invalid Time #{line}" if (line[0] != '#')
    wwww = line.slice(3,4).to_i
    sow = line.slice(8,14).to_f
    interval = line.slice(24,13).to_f
    return [wwww, sow, interval]
  end
  
  def parse_sp3(sp3_data)

    lines = sp3_data.split("\n")

    line = lines.shift
    
    while (line[0,2] != "##")
      line = lines.shift
    end
    wwww, sow, interval = time_parse(line)
    if (wwww < @week)
      sow -= 7*86400.0
    end  
    if (wwww > @week)
      sow += 7*86400.0
    end  
    sow -= interval
    
    while (line != nil)
      begin
        date = date_parse(line)
        sow2 = date.sow()
        sow = sow + interval
        while true
        begin
            line = lines.shift
            sv,x,y,z,t = pos_parse(line)
            @interps[sv] = [] if (@interps[sv] == nil)
            @interps[sv].push([sow, x,y,z])
          rescue Exception => err
            # No longer have a position
            break
          end
        end
      rescue Exception => err
        #puts err
        line = lines.shift
      end
    end
  end

  def lagrange_numerator(t_i, i, t)
    n = t_i.length()
    ret = 1
    n.times do |j|
      ret *= (t - t_i[j]) if (j != i)
    end
    ret
  end
  
  def lagrange_denominator(t_i, i)
    n = t_i.length()
    ret = 1
    n.times do |j|
      ret *= (t_i[i] - t_i[j]) if (j != i)
    end
    ret
  end
  
  def lagrange_interpolate(t_i, y_i, t)
    ret = 0.0
    n = t_i.length()
    n.times do |i|
      ret += y_i[i]*(lagrange_numerator(t_i, i, t) /
              lagrange_denominator(t_i, i))
    end
    return ret
  end
  
  def get_sv_position(gt, sv)
    sow = gt.sow()
    
    inter = @interps[sv]
    
    dt0 = 999999.0
    i_close = 0
    n = inter.length()
    n.times do |j|
      i = inter[j]
      test = (i[0] - sow).abs()
      if (i[0] <= sow) and (test < dt0)
        dt0  = test
        i_close = j
      end
    end
        
    t_points = []
    x_points = []
    y_points = []
    z_points = []

    order = 5
    i0 = [i_close - order, 0].max()
    i1 = [i_close + order + 1, n-1].min()

    (i0..i1).each do |i|
      t_points.push(inter[i][0])
      x_points.push(inter[i][1])
      y_points.push(inter[i][2])
      z_points.push(inter[i][3])
    end
    
    xl = lagrange_interpolate(t_points, x_points, sow)*1000.0
    yl = lagrange_interpolate(t_points, y_points, sow)*1000.0
    zl = lagrange_interpolate(t_points, z_points, sow)*1000.0
    
    return [xl,yl,zl]
  end
  

end