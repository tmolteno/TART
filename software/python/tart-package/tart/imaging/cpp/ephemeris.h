#ifndef __ephemeris__
#define __ephemeris__

#include "vector3.h"
#include "gps_time.h"
#include "util.h"


class ephemeris
{
public:

	ephemeris()
	{
	}

	ephemeris(const gps_time& t)
	: toc(t)
	{
	}

	/*!\brief Find the clock correction from UTC for this SV
		This is done ignoring leap seconds. In other words, this is modulo 1 second.
	*/
	double clock_correct(const second_of_week& sow);

	double getE(const second_of_week& sow);

	vector3 get_location(const second_of_week& sow);
	vector3 get_velocity(const second_of_week& sow);

	// Units are either seconds, meters, or radians
	gps_time toc;

	long svprn;
	
	double a0, a1, a2, a3; // alpha parameters for ionospheric model
	double b0, b1, b2, b3; // beta parameters for ionospheric model
	
	double af0; // clock_bias (seconds)
	double af1; // clock_drift (seconds per second)
	double af2; // clock_drift_rate (seconds per second^2
	double tgd; // Group delay (seconds) (differential between L1 and L2)

 	// The corrections of the satellite time to UTC are as follows:
	// Tutc = Tsv - af0  -  af1  *(Tsv-Toc) - ... -  A0  - ... - leap_sec

	double IODE;
	double codes;
	double weekno;
	double L2flag;
	double fit;

	double M0;
	double roota;
	double deltan;
	double ecc;
	double omega;
	double cuc;
	double cus;
	double crc;
	double crs;
	double i0;
	double idot;
	double cic;
	double cis;
	double Omega0;
	double Omegadot;
	double tom;
	double toe;

	double svaccur;		// SV accuracy      (meters)
	double svhealth;	// SV health        (bits 17-22 w 3 sf 1)
};

#endif
