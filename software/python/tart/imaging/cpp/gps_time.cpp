/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */

#include "gps_time.h"

#include <cmath>
#include "util.h"

#include <ctime>

/** !\brief Conversion of Julian Day number to GPS week and Seconds of Week reckoned from Saturday midnight
	\param y Year as a four digit number YYYY
	\param m Month 1-12
	\param d Day of Month 1-31
	\param h Hour 0-23
	\param minutes Minutes 0-59
	\param seconds Seconds 0-60

GPS Time is a uniformly counting time scale. GPS time was born at the 1/5/1980 to 1/6/1980 midnight. January 6, 1980 is a Sunday. 
GPS Time counts in weeks and seconds of a week from this instant. The weeks begin at the Saturday/Sunday transition.
The days of the week are numbered, with Sunday being 0, Saturday is day 6. GPS week 0 began at the beginning of the GPS Time Scale. 
Within each week the time is usually denoted as the second of the week. This is a number between 0 and 604,800 ( 60 x 60 x 24 x 7).

*/
gps_time::gps_time(int y, int m, int d, int h, int minutes, double seconds)
{
	if (y < 1980)
	{
	  std::stringstream ss;
	  ss << "Year '" << y << "' must >= 1980"; 
	  throw ss.str();
	}


	struct tm in_time;
	in_time.tm_sec = 0;
	in_time.tm_min = minutes;
	in_time.tm_hour = h;
	in_time.tm_mday = d;
	in_time.tm_mon = m-1; // months since january
	in_time.tm_year = y - 1900;
	
	// This is broken if we use -1 because if there is no local daylight savings time, then this fails.
 	in_time.tm_isdst = -1;

	struct tm gps_tm;
 	gps_tm.tm_sec = 0; // january 6th 1980
 	gps_tm.tm_min = 0;
 	gps_tm.tm_hour = 0;
 	gps_tm.tm_mday = 6; 
 	gps_tm.tm_mon = 0; // months since january
 	gps_tm.tm_year = 80;
 	gps_tm.tm_isdst = -1;
	// time_t gps_t = mktime ( &gps_tm );
	time_t gps_t = timegm ( &gps_tm );
  
	/* call mktime: in_time->tm_yday will be set */
	time_t in_t = timegm ( &in_time );
	m_day_of_year = in_time.tm_yday;
  
	// I don't think that the UTC offset works correctly.
	int utc_offset = gps_time::utc_offset(y,m);
	
	double gps_sec = difftime ( in_t, gps_t ) + seconds + utc_offset;
	double gps_days = floor(gps_sec / sec_per_day);
	double gps_weeks = gps_days / 7.0;

	m_week = floor(gps_weeks);
	sec_of_week._seconds = gps_sec - m_week*7.0*sec_per_day;

#if 0
	std::cout << "gps_sec " << gps_sec << std::endl;
	std::cout << "gps_days " << gps_days << std::endl;
	std::cout << "gps_time " << m_week << ":" << sec_of_week << std::endl;
#endif

}



int gps_time::utc_offset(int y, int m)
{
	int utc_offset = 0;
	if (y == 1981) { if (m >= 7) utc_offset = 1; else utc_offset = 0; }
	if (y == 1982) { if (m >= 7) utc_offset = 2; else utc_offset = 1; }
	if (y == 1983) { if (m >= 7) utc_offset = 3; else utc_offset = 2; }
	if (y == 1984) utc_offset = 3;
	if (y == 1985) { if (m >= 7) utc_offset = 4; else utc_offset = 3; }
	if (y == 1986) utc_offset = 4;
	if (y == 1987) utc_offset = 4;
	if (y == 1988) utc_offset = 5;
	if (y == 1989) utc_offset = 5;
	if (y == 1990) utc_offset = 6;
	if (y == 1991) utc_offset = 7;
	if (y == 1992) { if (m >= 7) utc_offset = 8; else utc_offset = 7; }
	if (y == 1993) { if (m >= 7) utc_offset = 9; else utc_offset = 8; }
	if (y == 1994) { if (m >= 7) utc_offset = 10; else utc_offset = 9; }
	if (y == 1995) utc_offset = 10;
	if (y == 1996) utc_offset = 11;
	if (y == 1997) { if (m >= 7) utc_offset = 12; else utc_offset = 11; }
	if (y == 1998) utc_offset = 12;
	if (y == 1999) utc_offset = 13;
	if (y == 2006) utc_offset = 14;
	if (y >= 2009) utc_offset = 15;
	
	return utc_offset;
}

int gps_time::day_of_week() const
{
  double dow = sec_of_week._seconds / sec_per_day;
  return floor(dow);
}


// difference in seconds between gps_times
double gps_time::diff(const gps_time& b) const
{
  double w = (m_week - b.m_week) * 7 * sec_per_day;
  double s = (sec_of_week - b.sec_of_week);
  return w + s;
}

int gps_time::day_of_year() const
{
  return m_day_of_year;
}


std::ostream& operator<<(std::ostream& os, const gps_time& gt)
{
//	double jd = gt.mm_julian_date;
	os << gt.day_of_year() << ":";
	os << gt.m_week << "-" << gt.day_of_week() << "-" << gt.sec_of_week;
	return os;
}