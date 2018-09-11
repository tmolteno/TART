/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */

#include "julian_date.h"
#include "gps_time.h"
#include "gps_time_new.h"

#include <iostream>
using namespace std;

inline void assert(int test, int actual, const char* message)
{
  if (test != actual)
	cout << "Failed! " << message << " " << test << " != " << actual << endl;
}

inline void assert(double test, double actual, const char* message)
{
  if (abs(test - actual) > 1e-9)
	cout << "Failed! " << message << " " << test << " != " << actual << endl;
}


void brute_force_test()
{
	// compute a GPS time object for every day
	int day_counter = -5;
	for (int yyyy = 1980; yyyy<2010; yyyy++)
	{
	  bool leap_year = (yyyy % 4) == 0;
	  for (int mm = 1; mm<=12; mm++)
	  {
	    int day_count = 31;
	    if (2 == mm) day_count = leap_year ? 29 : 28;
	    if (9 == mm) day_count = 30;
	    if (4 == mm) day_count = 30;
	    if (6 == mm) day_count = 30;
	    if (11 == mm) day_count = 30;
	      
	    for (int dd = 1; dd<= day_count; dd++)
	    {
	      gps_time gt(yyyy,mm,dd,0,0,30); // 30 seconds after midnight
	      assert(gt.day_of_week(), day_counter % 7, "gt.day_of_week()");
	      day_counter++;
	    }
	  }
	    
	}
}

void date_testbench()
{
	cout.precision(4);                 // Set 4 digits past the decimal
	cout.flags(ios::right | ios::fixed); // Fixed point, right justified
    {
      cout << "------------------------------------------" << endl;
       
      gps_time gt(1980,1,6,0,0,0.23);
      cout << "week " << gt.week() << endl;
      cout << "day of week " << gt.day_of_week() << endl;
      cout << "second of week " << gt.sow() << endl;
    }
    
    {
      gps_time gt(1980,1,10,0,0,0.0);
      cout << "week " << gt.week() << endl;
      cout << "day of week " << gt.day_of_week() << endl;
      cout << "second of week " << gt.sow() << endl;
    }
    
    {
      cout << "------------------------------------------" << endl;
      julian_date jd(2000,1,1,12,0,0.0);
      cout << "day_of_year = " << jd.day_of_year() << endl;
      cout << "julian_date = " << jd.getJulianDate() << endl;
      
      gps_time gt(2000,1,1,12,0,0.0);
      cout << "week " << gt.week() << endl;
      cout << "day of week " << gt.day_of_week() << endl;
      cout << "second of week " << gt.sow() << endl;
    }
    
    {
		cout << "------------------------------------------" << endl;
		julian_date jd(2008,1,1,0,0,0.1);
		cout << "day_of_year = " << jd.day_of_year() << endl;
		cout << "julian_date = " << jd.getJulianDate() << endl;
	
    gps_time gt(2008,1,1,0,0,0.1);
		cout << "week " << gt.week() << endl;
		cout << "day of week " << gt.day_of_week() << endl;
		cout << "second of week " << gt.sow() << endl;
	}

	{
		cout << "------------------------------------------" << endl;
		julian_date jd(2008,1,6,0,0,0.1);
		cout << "day_of_year = " << jd.day_of_year() << endl;
		cout << "julian_date = " << jd.getJulianDate() << endl;
	
    gps_time gt(2008,1,6,0,0,0.1);
		cout << "week " << gt.week() << endl;
		cout << "day of week " << gt.day_of_week() << endl;
		cout << "second of week " << gt.sow() << endl;
	}

	{
		cout << "------------------------------------------" << endl;
		julian_date jd(2008,9,24,0,0,0.0);
		cout << "day_of_year = " << jd.day_of_year() << endl;
		cout << "julian_date = " << jd.getJulianDate() << endl;
	
    gps_time gt(2008,9,24,0,0,0.0);
		cout << "week " << gt.week() << endl;
		cout << "day of week " << gt.day_of_week() << endl;
		cout << "second of week " << gt.sow() << endl;
	
		if (jd.day_of_year() != 268)
			cout << "Failed! jd.day_of_year() != 268 " << jd.day_of_year() << endl;
		if (jd.getJulianDate() != 2454733.50)
			cout << "Failed! jd.getJulianDate() != 2454733.50 " << round(jd.getJulianDate()) << endl;
	
		assert(gt.week(), 1498, "gt.week()");
		assert(gt.day_of_week(), 3, "gt.day_of_week()");
		assert(gt.sow(), 259200.0, "gt.sow()");
	}

	{	// 10_05_13_133_00_22_08.bin
		cout << "------------------------------------------" << endl;
		julian_date jd(2010,5,13,0,0,0.0);
		cout << "day_of_year = " << jd.day_of_year() << endl;
		cout << "julian_date = " << jd.getJulianDate() << endl;
	
		gps_time gt(2010,5,13,0,0,0.0);
		cout << "week " << gt.week() << endl;
		cout << "day of week " << gt.day_of_week() << endl;
		cout << "second of week " << gt.sow() << endl;
	
		assert(jd.day_of_year(), 133, "jd.day_of_year()");
		assert(jd.getJulianDate(), 2455329.5, "jd.getJulianDate()");
	
		assert(gt.week(), 1583, "gt.week()");
		assert(gt.day_of_week(), 4, "gt.day_of_week()");
		assert(gt.sow(), 345600.0, "gt.sow()");
		
		gps_time gt2(2010,5,13,0,0,0.0);
		assert(gt2.sow(), gt.sow(), "gt.sow() == gt2.sow()");
	}
	brute_force_test();
}

int main(void)
{
	date_testbench();
}