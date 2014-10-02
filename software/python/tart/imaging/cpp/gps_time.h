/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */

#ifndef __gps_time__
#define __gps_time__

#include <cmath>

#include "second_of_week.h"

#include "julian_date.h"
/**!\brief A GPS time object. http://adn.agi.com/GNSSWeb/Default.aspx

SV Clock and GPS Time stuctures are easy to figure out. GPS Time is kept as Weeks and Seconds from Jan. 5 midnight/Jan. 6 morning of 1980.
This means that we don't have to keep track of UTC leap seconds (or perhaps more accurately, we're not keeping track of them here). This
means that when the date in yyyy, mm, dd format is entered, the conversion to GPS time is done by computing the number of seconds between 
that date and the gps start date.

When converting from gps seconds to UTC we technically should keep track of UTC leap seconds.

TODO. Make the conversion between gps_time and UTC robust to the UTC leap second issue.

*/ 
class gps_time
{
public:

	/** !\brief Construct an invalid gps_time
	*/
	gps_time()
	{
	  m_week = -9999;
	}

	/** !\brief Construct from a UTC year, month, day and hour
	*/
	gps_time(int y, int m, int d, int h, int minutes, double seconds);

	static int utc_offset(int y, int m);
	
	double diff(const gps_time& b) const;

	int week() const
	{
		return m_week;
	}

	double sow() const
	{
		return sec_of_week.to_double();
	}

  int day_of_year() const;
  int day_of_week() const;

  int m_week;
  second_of_week sec_of_week;

    static const double sec_per_day = 86400.0;
private:  // makes life easier
  int m_day_of_year;
};

#include <iostream>
std::ostream& operator<<(std::ostream& os, const gps_time& gt);

#endif
