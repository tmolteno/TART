#ifndef __ephemerides__
#define __ephemerides__

#include "ephemeris.h"
#include "gps_time.h"

#include <vector>

/**!\brief A collection of ephemeris
*/
class ephemerides
{
public:
	void parse_brdc(const char* filename);

	ephemeris get_ephemeris(int PRN, const gps_time& t);


	static void test(void);

	std::vector<ephemeris> _data;

};

#endif
