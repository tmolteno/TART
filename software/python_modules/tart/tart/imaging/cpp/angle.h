#ifndef __angle__
#define __angle__

#include "util.h"

class angle
{
public:
	angle()
	: _theta(0.0)
	{
	}

	angle(double radians)
	: _theta(radians)
	{
	}

	static double rad2deg(double in_rad)
	{
		static const double rad2deg = 180.0 / util::pi;
		return in_rad * rad2deg;
	}

	static double deg2rad(double in_deg)
	{
		static const double deg2rad = util::pi / 180.0;
		return in_deg * deg2rad;
	}

	static angle from_deg(double in_deg)
	{
		return angle(deg2rad(in_deg));
	}

	static angle from_rad(double in_rad)
	{
		return angle(in_rad);
	}

	angle(double degrees, double minutes, double seconds)
	{
		double deg;

		if (degrees < 0)
			deg = degrees - minutes/60.0 - seconds/3600.0;
		else
			deg = degrees + minutes/60.0 + seconds/3600.0;

		_theta = deg2rad(deg);
	}

	double radians() const { return _theta; }

	double degrees() const
	{
		return rad2deg(_theta);
	}
	
	double cos() const { return std::cos(_theta); }

	double _theta;
};

#include <iostream>

inline std::ostream& operator<<(std::ostream& os, const angle& a)
{
	double d = a.degrees();

	if (a.degrees() < 0) d = -d;
	int deg = floor(d);

	double mind = (d - deg) * 60.0;
	int minutes = floor(mind);
	double seconds = (mind - minutes) * 60.0;

	if (a.degrees() < 0) os << "-";
	os << deg << " " << minutes << "'" << seconds << "\"";
	return os;
}


#endif /* __angle__ */


