/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */
#ifndef __sow__
#define __sow__

#include <cmath>

class second_of_week
{
public:
  second_of_week() : _seconds(0.0) { }
  second_of_week(const double& in_sow) : _seconds(in_sow) { }
  
  static second_of_week from_double(const double& in_sow)
  {
    second_of_week ret;
    ret._seconds = in_sow;
    return ret;
  }
  double to_double() const { return _seconds; }

  double diff(const second_of_week& b) const
  {
    return _seconds - b._seconds;
  }

  bool operator<(const second_of_week& a) const
  {
    return _seconds < a._seconds; 
  }

  void operator+=(const double& s)
  {
    _seconds += s;  
  }
  
  inline double operator-(const second_of_week& b)
  {
    return _seconds - b._seconds; 
  }  
  
  unsigned long ms() const
  {
    return floor(_seconds * 1000.0);
  }

  double _seconds;
};

inline double operator-(const second_of_week& a, const second_of_week& b)
{
  return a._seconds - b._seconds; 
}

/*inline second_of_week operator-(const second_of_week& a, const double& s)
{
  second_of_week ret(a);
  ret._seconds -= s;  
  return ret;
}*/
inline second_of_week operator+(const second_of_week& a, const double& s)
{
  second_of_week ret(a);
  ret._seconds += s;  
  return ret;
}

#include <iostream>
std::ostream& operator<<(std::ostream& os, const second_of_week& s);

#endif /* __sow__ */

