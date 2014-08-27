/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */
#include "second_of_week.h"

std::ostream& operator<<(std::ostream& os, const second_of_week& s)
{
  os.precision(7);
  os << s._seconds;
  return os;
}

