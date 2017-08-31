#include "util.h"

// These must be declared in a .cpp file (otherwise if we switch optimization off
// and they are not inlined, then we'll get into trouble.
const double util::frequency;
const double util::pi;
const double util::pi2;
const double util::speed_of_light;
const double util::wavelength;
const double util::deg2rad;


void replaceall( std::string& source, const std::string find, std::string replace )
{
	size_t j;
	for ( ; (j = source.find( find )) != std::string::npos ; ) {
	  source.replace( j, find.length(), replace );
	}
}
      
void trim(std::string& str)
{
  std::string::size_type pos = str.find_last_not_of(' ');
  if(pos != std::string::npos) {
    str.erase(pos + 1);
    pos = str.find_first_not_of(' ');
    if(pos != std::string::npos) str.erase(0, pos);
  }
  else str.erase(str.begin(), str.end());
}

double to_double(const std::string& s)
{
	double t;
 	std::string replacer(s);
	trim(replacer);
	replaceall(replacer, "D", "E");
	replaceall(replacer, "d", "e");
	
	std::istringstream iss(replacer);
	iss >> std::dec >> t;
	if (iss.fail())
	{
		std::cout << "Failed to parse " << s << std::endl;
		throw -1;
	}

	if (!iss.eof())
	{
		std::cout << "Failed to parse double '" << s << "' as '" << replacer << "'" << std::endl;
		throw -1;
	}

	return t;
}

