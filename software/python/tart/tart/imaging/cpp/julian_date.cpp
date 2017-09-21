/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */

#include "gps_time.h"
#include "julian_date.h"
#include <cmath>

using namespace std;

julian_date::julian_date() 
{
	mJulianDayfrac = 0.0;
	mJulianDay= 0;
}

/**
In everyday life we use month, day and year to denote a date. However adding and subtracting dates is complicated. 
Astronomers use a time scale that counts uniformly in days they call the Julian Date. 
They set the origin of this date system far back in time, at January 0, 4173 BC at noon. 
This scale is counted in days and fractions of a day. January 1, 2000 at noon will be JD = 2,451,545.0 . 

The days used to be started at noon, hence the use of noon as the reference time.
In fact, astronomical publications used noon as the day dividing line until 1925. 
Julian dates are large numbers and sometime hard to handle. There is a slightly modified JD scale that is 
commonly used in science today. It is called the Modified Julian Date (MJD). The introduction of this scale 
was used to place the day transition at midnight, agreeing with the civilian days. MJD is defined as the 
Julian Date minus 2,400,000.5 . Thus January 1, 2000 at noon will be MJD of 51,544.5 .
*/
julian_date::julian_date(uInt year, uInt month, uInt day, uInt hour, uInt min, double sec) 
{
	int y=year,m=month,d=day;
	
	// deal with the UTC offset problem.
	int utc_offset = gps_time::utc_offset(y, m);
	sec += utc_offset;
	
	uInt md = (1461*(y + 4800 + (m-14)/12))/4 
			+ (367*(m - 2 - 12*((m-14)/12)))/12 
			- (3*((y+4900+(m-14)/12)/100))/4 
			+ d - 32075;
	
	// the fraction of the day
	double jd = double(hour)/24.0 + double(min)/1440.0 + sec/86400.0;
	// with this formula (calculated md)
	// date --> Julian day number - 0.5
	// then Julian day -1 (jd-1) and fraction +0.5 (md+0.5)
	
	mJulianDayfrac = jd;
	mJulianDay= md - 2400001;
}


julian_date::julian_date(double jdn)
{
	mJulianDayfrac= jdn - (int)jdn;
	if( mJulianDayfrac >= 0.5)
	{
		mJulianDayfrac= mJulianDayfrac - 0.5;
		mJulianDay= (int)jdn - 2400000;
	}
	else
	{
		mJulianDayfrac= mJulianDayfrac + 0.5;
		mJulianDay= (int)jdn - 2400001;
	}
}

double julian_date::getJulianDate() const
{
	// return Julian day
	return (double)mJulianDay + mJulianDayfrac + 2400000.5;
}

double julian_date::operator-(const julian_date& begin) const
{
	double d = (double)mJulianDay - (double)begin.mJulianDay;

	double s = mJulianDayfrac - begin.mJulianDayfrac;
	if (s < 0.0)
	{
		s += 1.0;
		d = (double)mJulianDay - (double)begin.mJulianDay - 1.0;
	}
	
	return (d+s)*86400.0;
}

int julian_date::day_of_year() const
{
// Return day of the year for the Julian day number.
// Where day runs from 1 though 366.

	int i,j,jd,l,n,day,month,year;
	
	jd = mJulianDay + 2400001;
//	jd = round(getJulianDate());
	l=jd+68569;
	n=(4*l)/146097;
	l=l-(146097*n+3)/4;
	i=(4000*(l+1))/1461001;
	l=l-(1461*i)/4+31;
	j=(80*l)/2447;
	day=l-(2447*j)/80;
	l=j/11;
	month=j+2-12*l;
	year=100*(n-49)+i+l;
	
	switch(month)
	{
		case 1: return day;
		case 2: return day+=31;
		case 3: day+=59;   // jan + feb = 31 + 28 = 59
			break;
		case 4: day+=90;   // jan + feb + march = 31+28+31= 90
			break;
		case 5: day+=120;  // 31+28+31+30 = 120
			break;
		case 6: day+=151;  // 31+28+31+30+31 = 151
			break;
		case 7: day+=181;  // 31+28+31+30+31+30 = 181
			break;
		case 8: day+=212;  // 31+28+31+30+31+30+31 = 212
			break;
		case 9: day+=243;  // 31+28+31+30+31+30+31+31 = 243
			break;
		case 10: day+=273; // 31+28+31+30+31+30+31+31+30 = 273
			break;
		case 11: day+=304; // 31+28+31+30+31+30+31+31+30+31 = 304
			break;
		case 12: day+=334; // 31+28+31+30+31+30+31+31+30+31+30 = 334
			break;
	}

	if (year%100 != 0)
	{
		if (year%400 == 0) day++;
		else if(year%4 == 0) day++;
	}
	return day;
}

