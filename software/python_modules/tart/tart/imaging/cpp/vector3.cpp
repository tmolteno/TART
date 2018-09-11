#include "vector3.h"

vector3 vector3::normalize() const
{ 
	return *this/norm();
}

vector3 operator-(const vector3& a, const vector3& b)
{
	return vector3(a.x - b.x, a.y - b.y, a.z - b.z);
}

double operator*(const vector3& a, const vector3& b)
{
	return (a.x*b.x + a.y*b.y + a.z*b.z);
}

vector3 operator/(const vector3& a, const double& b)
{
	return vector3(a.x/b, a.y/b, a.z/b);
}

#include <iostream>
std::ostream& operator<<(std::ostream& os, const vector3& v)
{
	os << v.x << ", " << v.y << ", " << v.z;
	return os;
}

