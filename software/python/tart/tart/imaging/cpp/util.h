#ifndef __util__
#define __util__

#include <cmath>
#include <cstdlib>
#include <algorithm>
#include <iostream>

using namespace std;

class util
{
public:
	static const double frequency=1.57542e9;
	static const double pi=3.1415926535898;
	static const double pi2=6.2831853071795864;
	static const double speed_of_light = 299792458.0;
	static const double wavelength=0.1905;
	static const double deg2rad=0.017453292;
	static const double rad2deg=57.29577951;
	
	static inline double idiv(const double& x, const double& y)
	{
		return floor (x / y);
	}

	static inline double rem(const double& x, const double& y)
	{
		return x - y * idiv(x,y);
	}

	/*! \brief remainder = numerator - quotient * denominator
        */static inline double mod(const double& x, const double& y)
	{
		double ret = x - y*idiv(x,y);
		
// 		if (x < 0)
// 			ret = y - x*idiv(x,y);
		
// 		cout.precision(18);
// 		cout << abs(ret-y) << endl;

// 		if (abs(ret-y) < 1e-10)
// 			return 0.0;
		
		return ret;
	}
	
	static inline int mod_int(const int& x, const int& y)
	{
		double ret = x%y;
		if (x < 0)
			ret = y - (-x)%y;
		return ret;
	}

	static inline double rem2pi(const double& x)
	{
		static const double p2=6.2831853071795864;
		return rem(x, p2);
	}

	static inline double rand01() // generate uniform random number from 0..1
	{
		return double(std::rand())/RAND_MAX;
	}

	/*! \brief Return a random integer between min and max (inclusive)
	*/
	static inline int random_int(int min, int max)
	{
		int range = max - min;
		return round(rand01()*range)+min;
	}
	
	/*! \brief Return a random double between min and max
	*/
	static inline double random(double min, double max)
	{
		double range = max - min;
		return rand01()*range + min;
	}

        /*! \brief Calculate the difference betwen x and y modulo the range
         * for example: -179, 179, 360 -> 2
         * d0 = util::mod(358, 360) -> x - y*idiv(x,y); -? 358 - 360*(0) -> 358
         * d1 = util::mod(-358, 360) -> x - y*idiv(x,y); -? -358 - 360*(0) -> -358
        */
	static inline double delta_cyclic(double x,double y,double range)
	{
		double d0 = util::mod(y - x, range);
		double d1 = util::mod(x - y, range);	
		return std::min(d0,d1);
	}
	
	/*! \brief Repairs oveflow and underflow of GPS time	
	*/
	static inline double check_t(const double& t)
	{
		static const double half_week = 302400;
		double tt = t;
		
		if (t >  half_week)
			tt = t-2*half_week;
		if (t < -half_week)
			tt = t+2*half_week;
		
		return tt;
	}

	/*! \brief Convert degrees to radians	
	*/
	static inline double to_rad(const double& deg)
	{
		return deg2rad*deg;
	}
	
	/*! \brief Convert radians to degrees	
	*/
	static inline double to_deg(const double& rad)
	{
		return rad/deg2rad;
	}
	
	/*! \brief Top hat probability distribution
	*/
	static inline double top_hat(const double& x,const double& min,const double& max)
	{
		if (x < min) return 0.0;
		
		if (x > max) return 0.0;
		
		return 1.0;
	}
	
	/*! \brief Top hat probability distribution function
	*/
	static inline double top_hat_pdf(const double& x,const double& min,const double& max)
	{
		if (x < min) return 0.0;
		
		if (x > max) return 0.0;
		
		return 1.0;
	}
	
	/*! \brief Normal probability distribution function
	*/
	static inline double normal_pdf(const double& x,const double& mu,const double& sigma)
	{
		double two_sigma_sq = 2*sigma*sigma;
		return exp( - pow(x-mu,2) / two_sigma_sq );// * ( 1.0 / sqrt(pi*two_sigma_sq) );
	}
	
	/*! \brief Draw random sample from normal probability distribution function
	*/
	static inline double sample_normal(const double& mu,const double& sigma)
	{
		double x = rand01();
		for (int i=0; i<11; i++) x += rand01();
		return sigma*(x - 6.0) + mu;
	}
	
};


#include <string>
#include <sstream>
#include <iostream>
#include <fstream>

template <class T> T from_string(const std::string& s)
{
	T t;
	std::istringstream iss(s);
	iss >> std::dec >> t;
	if (iss.fail())
	{
		std::cout << "Failed to parse " << s << std::endl;
		throw -1;
	}

	return t;
}

double to_double(const std::string& s);


#endif
