#ifndef __vector3__
#define __vector3__

#include <cmath>

class vector3
{
public:

	vector3(const double& _x, const double& _y, const double& _z)
		: x(_x), y(_y), z(_z)
	{ }

	vector3()
		: x(0.0), y(0.0), z(0.0)
	{ }

	double norm2() const	{ return (x*x + y*y + z*z); }
	double norm() const	{ return std::sqrt(norm2()); }
	vector3 normalize() const;

	double x,y,z;
};

vector3 operator-(const vector3& a, const vector3& b);
double operator*(const vector3& a, const vector3& b);
vector3 operator/(const vector3& a, const double& b);

#include <iostream>
std::ostream& operator<<(std::ostream& os, const vector3& v);

#endif

