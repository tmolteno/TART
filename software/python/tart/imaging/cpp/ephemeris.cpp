/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */

#include "ephemeris.h"

#include <cmath>
#include "util.h"

using namespace std;

double ephemeris::getE(const second_of_week& sow)
{
	double t = sow._seconds;

	static double GM = 3.986005e14;  // earth's universal gravitational parameter m^3/s^2
	
	// Procedure for coordinate calculation
	double A = roota*roota;
	double tk = util::check_t(t-toe);
	double n0 = sqrt(GM/pow(A,3));
	double n = n0+deltan;
	double M = M0+n*tk;

	M = util::rem2pi(M + util::pi2);
	double E = M;
	for (int i = 0; i< 10; i++)
	{
		double E_old = E;
		E = M + ecc*sin(E);
		double dE = util::rem2pi(E - E_old);
		if (abs(dE) < 1.e-12)
			break;
	}
	E = util::rem2pi(E + util::pi2);
	return E;
}

vector3 ephemeris::get_location(const second_of_week& sow)
{		
	double t = sow._seconds;
	double A = roota*roota;
 	double tk = util::check_t(t-toe);

	double E = getE(sow);

	static double Omegae_dot = 7.2921151467e-5;  // earth rotation rate, rad/s

	double v = atan2(sqrt(1-pow(ecc,2))*sin(E), cos(E)-ecc);
	double phi = v+omega;
	phi = util::rem2pi(phi);
	double phi2 = 2.0*phi;
	
	double cosphi2 = cos(phi2);
	double sinphi2 = sin(phi2);
	
	double u = phi              + cuc*cosphi2+cus*sinphi2;
	double r = A*(1-ecc*cos(E)) + crc*cosphi2+crs*sinphi2;
	double i = i0+idot*tk       + cic*cosphi2+cis*sinphi2;
	double Omega = Omega0 + (Omegadot - Omegae_dot)*tk - Omegae_dot*toe;
	Omega = util::rem2pi(Omega + util::pi2);
	double x1 = cos(u)*r;
	double y1 = sin(u)*r;

	vector3 satp;
	satp.x = x1*cos(Omega) - y1*cos(i)*sin(Omega);
	satp.y = x1*sin(Omega) + y1*cos(i)*cos(Omega);
	satp.z = y1*sin(i);

	return satp;
}


vector3 ephemeris::get_velocity(const second_of_week& sow)
{
	vector3 ret;
#if 0
	// more accurate calculation TODO tom's velocity formula here.
	ret = foo.
	cout << "Accurate " << ret_accurate;
#else
	vector3 loc1 = get_location(sow - 0.5);
	vector3 loc2 = get_location(sow + 0.5);

	ret = (loc1 - loc2);
//	cout << "Simple " << ret;
#endif
	
	return ret;
}


#include <iostream>
using namespace std;

/*!\brief Find the clock correction from UTC for this SV
	This is done ignoring leap seconds. In other words, this is modulo 1 second.

	Tutc = Tsv - af0  -  af1  *(Tsv-Toc) - ... -  A0  - ... - leap_sec
*/
double ephemeris::clock_correct(const second_of_week& Tsv)
{
#if 1
	// more accurate calculation
	double E = getE(Tsv);
	double dt = util::check_t(Tsv - toc.sec_of_week);
	double dtr = -4.442807e-10 * ecc * roota * sin(E);	// relatavistic correction
	double dtsv = af0 + af1*(dt) + af2*dt*dt + dtr - tgd;
#else
	double dt = util::check_t(Tsv._seconds - toe);
	double dtsv = af0 + af1*(dt);
#endif
	return dtsv;
}
