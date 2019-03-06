class Util
  PI=3.1415926535897932
  PI2=6.2831853071795864
  
  def self.check_t(t)
    half_week = 302400.0
    tt = t
    
    tt = t-2*half_week if (t >  half_week)
    tt = t+2*half_week if (t < -half_week)
    
    return tt
  end
  
  def self.idiv(x, y)
    return (x / y).floor.to_i;
  end

  def self.rem(x, y)
    return x - y * idiv(x,y);
  end

  #! \brief remainder = numerator - quotient * denominator
  #
  def self.mod(x, y)
    ret = x - y*idiv(x,y)
    return ret;
  end
  
  def self.mod_int(x, y)
    ret = x%y
    ret = y - (-x)%y if (x < 0)
    return ret
  end

  def self.rad2deg(x)
    return x*180.0 / PI
  end

  def self.rem2pi(x)
    return rem(x, PI2);
  end

end