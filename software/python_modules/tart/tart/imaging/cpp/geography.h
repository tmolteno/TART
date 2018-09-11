#ifndef __geography__
#define __geography__

#include "angle.h"

#include <iostream>

class geography
{
public:
	/** \brief Conversion of geographical coordinates (lat, lon, alt) to Cartesian Earth Centered Earth Fixed coordinates (X, Y, Z). 
		\param latitude  geocentric latitude 
		\param longitude geocentric longitude 
		\param h altitude in meters
		\return vector3 of ECEF co-ordinates in meters
	*/
	static vector3 WGS84(const angle& latitude,const angle& longitude, double h) 
	{
		double lat = latitude.radians(); 
 		double lon = longitude.radians();

 		static double a = 6378137.0; // earth semimajor axis in meters 
		static double f = 1.0/298.257223563; // reciprocal flattening 
		static double e2 = 2.0*f - f*f; // eccentricity squared 
 
		double sinlat = sin(lat);
		double chi = sqrt(1.0 - e2*sinlat*sinlat);

		double coslat = cos(lat);
		double achih = (a/chi + h);
		double X = achih*coslat*cos(lon); 
		double Y = achih*coslat*sin(lon); 
		double Z = (a*(1.0-e2)/chi + h)*sinlat;

		return vector3(X,Y,Z);
	}

#if 0
	/*!\brief Calculation of tropospheric correction.
	%       The range correction ddr in m is to be subtracted from
	%       pseudo-ranges and carrier phases
	%
	%ddr = tropo(sinel, hsta, p, tkel, hum, hp, htkel, hhum);
	%
	%   Inputs:
	%       sinel   - sin of elevation angle of satellite
	%       hsta    - height of station in km
	%       p       - atmospheric pressure in mb at height hp
	%       tkel    - surface temperature in degrees Kelvin at height htkel
	%       hum     - humidity in % at height hhum
	%       hp      - height of pressure measurement in km
	%       htkel   - height of temperature measurement in km
	%       hhum    - height of humidity measurement in km
	%
	%   Outputs:
	%       ddr     - range correction (meters)
	%
	% Reference
	% Goad, C.C. & Goodman, L. (1974) A Modified Tropospheric
	% Refraction Correction Model. Paper presented at the
	% American Geophysical Union Annual Fall Meeting, San
	% Francisco, December 12-17
	
	*/
	static double tropo(double sinel, double hsta, double p, double tkel, double hum, double hp, double htkel, double hhum)
	{
		double a_e    = 6378.137;     // semi-major axis of earth ellipsoid
		double b0     = 7.839257e-5;
		double tlapse = -6.5;
		double tkhum  = tkel + tlapse*(hhum-htkel);
		double atkel  = 7.5*(tkhum-273.15) / (237.3+tkhum-273.15);
		double e0     = 0.0611 * hum * 10^atkel;
		double tksea  = tkel - tlapse*htkel;
		double em     = -978.77 / (2.8704e6*tlapse*1.0e-5);
		double tkelh  = tksea + tlapse*hhum;
		double e0sea  = e0 * (tksea/tkelh)^(4*em);
		double tkelp  = tksea + tlapse*hp;
		double psea   = p * (tksea/tkelp)^em;
		
		if (sinel < 0)
			sinel = 0;
		
		double tropo   = 0;
		bool done    = false;
		double refsea  = 77.624e-6 / tksea;
		double htop    = 1.1385e-5 / refsea;
		refsea  = refsea * psea;
		double ref     = refsea * pow((htop-hsta)/htop,4);
		
		double ddr = 0;

		while (true)
		{
			rtop = (a_e+htop)^2 - (a_e+hsta)^2*(1-sinel^2);
			
			// check to see if geometry is crazy
			if (rtop < 0)
				rtop = 0; 
			
			rtop = sqrt(rtop) - (a_e+hsta)*sinel;
			a    = -sinel/(htop-hsta);
			b    = -b0*(1-sinel^2) / (htop-hsta);
			rn   = zeros(8,1);
			
			for (int i = 1:8)
				rn(i) = rtop^(i+1);
			
			alpha = [2*a, 2*a^2+4*b/3, a*(a^2+3*b),...
				a^4/5+2.4*a^2*b+1.2*b^2, 2*a*b*(a^2+3*b)/3,...
				b^2*(6*a^2+4*b)*1.428571e-1, 0, 0];
			
			if (b*b > 1.0e-35)
			{
				alpha(7) = a*b^3/2; 
				alpha(8) = b^4/9; 
			}
			
			dr = rtop;
			dr = dr + alpha*rn;
			tropo = tropo + dr*ref*1000;
			
			if (done)
			{
				ddr = tropo; 
				break; 
			}
			
			done    = true;
			refsea  = (371900.0e-6/tksea-12.92e-6)/tksea;
			htop    = 1.1385e-5 * (1255/tksea+0.05)/refsea;
			ref     = refsea * e0sea * pow((htop-hsta)/htop,4);
		}
		
		return ddr;
	}
#endif

};

#endif /* __geography__ */
