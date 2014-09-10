/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */

#ifndef __julian_date__
#define __julian_date__

typedef unsigned int uInt;

/**!\brief A Julian date object. http://adn.agi.com/GNSSWeb/Default.aspx

In everyday life we use month, day and year to denote a date. However adding and subtracting dates is complicated. 
Astronomers use a time scale that counts uniformly in days they call the Julian Date. They set the origin of this 
date system far back in time, at January 0, 4173 BC at noon. This scale is counted in days and fractions of a day.
January 1, 2000 at noon will be JD = 2,451,545.0 . The days used to be started at noon, hence the use of noon as 
the reference time. In fact, astronomical publications used noon as the day dividing line until 1925.

Julian dates are large numbers and sometime hard to handle. There is a slightly modified JD scale that is commonly 
used in science today. It is called the Modified Julian Date (MJD). The introduction of this scale was used to 
place the day transition at midnight, agreeing with the civilian days. MJD is defined as the Julian Date minus 
2,400,000.5 . Thus January 1, 2000 at noon will be MJD of 51,544.5 .

**/ 
class julian_date
{
public:
	julian_date();

	/**! \brief Calculate the Julian Date from UTC date */
	julian_date(uInt year, uInt month, uInt day, uInt hour, uInt min, double sec);

	julian_date(double jdn);

	double getJulianDate() const;

	double operator-(const julian_date& begin) const;

	int day_of_year() const;

	double mJulianDayfrac;
	long mJulianDay;
};

#endif /* __julian_date__ */