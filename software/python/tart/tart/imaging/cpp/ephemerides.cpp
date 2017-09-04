/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */
#include "ephemerides.h"
#include "util.h"

using namespace std;

/**!\brief Parse the Broadcast ephemerides file.

COMMENT             | Comment line(s)                          |     A60    |*
 +--------------------+------------------------------------------+------------+
*|ION ALPHA           | Ionosphere parameters A0-A3 of almanac   |  2X,4D12.4 |*
 |                    | (page 18 of subframe 4)                  |            |
 +--------------------+------------------------------------------+------------+
*|ION BETA            | Ionosphere parameters B0-B3 of almanac   |  2X,4D12.4 |*
 +--------------------+------------------------------------------+------------+
*|DELTA-UTC: A0,A1,T,W| Almanac parameters to compute time in UTC| 3X,2D19.12,|*
 |                    | (page 18 of subframe 4)                  |     2I9    |
 |                    | A0,A1: terms of polynomial               |            |
 |                    | T    : reference time for UTC data       |      *)    |
 |                    | W    : UTC reference week number.        |            |
 |                    |        Continuous number, not mod(1024)! |            |
 +--------------------+------------------------------------------+------------+
*|LEAP SECONDS        | Delta time due to leap seconds           |     I6     |*
 +--------------------+------------------------------------------+------------+
 |END OF HEADER       | Last record in the header section.       |    60X     |


For each ephemeris, the files is stored in RINEX format. Here is the data

 +----------------------------------------------------------------------------+
 |                                  TABLE A4                                  |
 |           GPS NAVIGATION MESSAGE FILE - DATA RECORD DESCRIPTION            |
 +--------------------+------------------------------------------+------------+
 |    OBS. RECORD     | DESCRIPTION                              |   FORMAT   |
 +--------------------+------------------------------------------+------------+
 |PRN / EPOCH / SV CLK| - Satellite PRN number                   |     I2,    |
 |                    | - Epoch: Toc - Time of Clock             |            |
 |                    |          year (2 digits, padded with 0   |            |
 |                    |                if necessary)             |  1X,I2.2,  |
 |                    |          month                           |   1X,I2,   |
 |                    |          day                             |   1X,I2,   |
 |                    |          hour                            |   1X,I2,   |
 |                    |          minute                          |   1X,I2,   |
 |                    |          second                          |    F5.1,   |
 |                    | - SV clock bias       (seconds)          |  3D19.12   |
 |                    | - SV clock drift      (sec/sec)          |            |
 |                    | - SV clock drift rate (sec/sec2)         |     *)     |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 1| - IODE Issue of Data, Ephemeris          | 3X,4D19.12 |
 |                    | - Crs                 (meters)           |            |
 |                    | - Delta n             (radians/sec)      |            |
 |                    | - M0                  (radians)          |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 2| - Cuc                 (radians)          | 3X,4D19.12 |
 |                    | - e Eccentricity                         |            |
 |                    | - Cus                 (radians)          |            |
 |                    | - sqrt(A)             (sqrt(m))          |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 3| - Toe Time of Ephemeris                  | 3X,4D19.12 |
 |                    |                       (sec of GPS week)  |            |
 |                    | - Cic                 (radians)          |            |
 |                    | - OMEGA               (radians)          |            |
 |                    | - CIS                 (radians)          |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 4| - i0                  (radians)          | 3X,4D19.12 |
 |                    | - Crc                 (meters)           |            |
 |                    | - omega               (radians)          |            |
 |                    | - OMEGA DOT           (radians/sec)      |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 5| - IDOT                (radians/sec)      | 3X,4D19.12 |
 |                    | - Codes on L2 channel                    |            |
 |                    | - GPS Week # (to go with TOE)            |            |
 |                    |   Continuous number, not mod(1024)!      |            |
 |                    | - L2 P data flag                         |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 6| - SV accuracy         (meters)           | 3X,4D19.12 |
 |                    | - SV health        (bits 17-22 w 3 sf 1) |            |
 |                    | - TGD                 (seconds)          |            |
 |                    | - IODC Issue of Data, Clock              |            |
 +--------------------+------------------------------------------+------------+
 | BROADCAST ORBIT - 7| - Transmission time of message       **) | 3X,4D19.12 |
 |                    |         (sec of GPS week, derived e.g.   |            |
 |                    |    from Z-count in Hand Over Word (HOW)  |            |
 |                    | - Fit interval        (hours)            |            |
 |                    |         (see ICD-GPS-200, 20.3.4.4)      |            |
 |                    |   Zero if not known                      |            |
 |                    | - spare                                  |            |
 |                    | - spare                                  |            |
 +--------------------+------------------------------------------+------------+

*/
void ephemerides::parse_brdc(const char* filename)
{
	// scan until we get a line that contains END OF HEADER

	ifstream ifs(filename, ifstream::in);
	string str;

#if QFDEBUG
	  cout << "Broadcast ephemerides file '" << filename << endl; 
#endif
	if (ifs.good() == false)
	{
	  stringstream ss;
	  ss << "Broadcast ephemerides file '" << filename << "' not found"; 
	  throw ss.str();
	}
/** search header for ionosphere model parameters...
          1         2         3         4         5         6         7         8
012345678901234567890123456789012345678901234567890123456789012345678901234567890
     2              NAVIGATION DATA                         RINEX VERSION / TYPE
CCRINEXN V1.6.0 UX  CDDIS               11-APR-10 02:51     PGM / RUN BY / DATE 
IGS BROADCAST EPHEMERIS FILE                                COMMENT             
    0.1118E-07  0.1490E-07 -0.5960E-07 -0.5960E-07          ION ALPHA           
    0.8806E+05  0.1638E+05 -0.1966E+06 -0.1311E+06          ION BETA            
   -0.279396772385E-08-0.266453525910E-14    61440     1579 DELTA-UTC: A0,A1,T,W
    15                                                      LEAP SECONDS        
                                                            END OF HEADER
*/
	ephemeris eph;
	int loop_counter = 0;
	while (loop_counter++ < 100)
	{
		getline(ifs,str);
		if (str.find("ION ALPHA") != string::npos)
		{
			eph.a0 = to_double(str.substr(3,12));
			eph.a1 = to_double(str.substr(15,12));
			eph.a2 = to_double(str.substr(27,12));
			eph.a3 = to_double(str.substr(39,12));
		}
		if (str.find("ION BETA") != string::npos)
		{
			eph.b0 = to_double(str.substr(3,12));
			eph.b1 = to_double(str.substr(15,12));
			eph.b2 = to_double(str.substr(27,12));
			eph.b3 = to_double(str.substr(39,12));
		}
		if (str.find("END OF HEADER") != string::npos)
		{
			break;
		}
	}

	while (ifs.good())
	{
		string line;
/** read in the ephemeris data...
          1         2         3         4         5         6         7         8
012345678901234567890123456789012345678901234567890123456789012345678901234567890
 2 08  9 12  0  0  0.0 0.188095495105E-03-0.284217094304E-11 0.000000000000E+00
    0.670000000000E+02 0.192500000000E+02 0.537165232231E-08-0.256690701048E+01
    0.110641121864E-05 0.875317445025E-02 0.500865280628E-05 0.515376602745E+04
    0.432000000000E+06-0.141561031342E-06-0.205415914935E+01 0.109896063805E-06
    0.942551693336E+00 0.274468750000E+03 0.263802852256E+01-0.833356141200E-08
    0.553594488004E-10 0.100000000000E+01 0.149600000000E+04 0.000000000000E+00
    0.200000000000E+01 0.000000000000E+00-0.172294676304E-07 0.670000000000E+02
    0.430428000000E+06 0.400000000000E+01 0.000000000000E+00 0.000000000000E+00
*/

		getline(ifs,line); // line 1
#if QFDEBUG
//	  cout << "Line '" << line << endl; 
#endif
		if (line.length() < 32)
			break;

			int svprn = from_string<int>(line.substr(0,2));

			int year = from_string<int>(line.substr(3,2));
			int month = from_string<int>(line.substr(6,2));
			int day = from_string<int>(line.substr(9,2));
			int hour = from_string<int>(line.substr(12,2));
			int minute = from_string<int>(line.substr(15,2));
			double second = to_double(line.substr(18,4));


		gps_time gt(2000+year, month, day, hour, minute, second);
			eph.toc = gt;
			eph.svprn = svprn;

			eph.af0 = to_double(line.substr(22,19));
			eph.af1 = to_double(line.substr(41,19));
			eph.af2 = to_double(line.substr(60,19));
		getline(ifs,line); // line 2
			eph.IODE = to_double(line.substr(3,19));
			eph.crs = to_double(line.substr(22,19));
			eph.deltan = to_double(line.substr(41,19));
			eph.M0 = to_double(line.substr(60,19));
		getline(ifs,line); // line 3
			eph.cuc = to_double(line.substr(3,19));
			eph.ecc = to_double(line.substr(22,19));
			eph.cus = to_double(line.substr(41,19));
			eph.roota = to_double(line.substr(60,19));
		getline(ifs,line); // line 4
			eph.toe = to_double(line.substr(3,19));
			eph.cic = to_double(line.substr(22,19));
			eph.Omega0 = to_double(line.substr(41,19));
			eph.cis = to_double(line.substr(60,19));
		getline(ifs,line); // line 5
			eph.i0 =  to_double(line.substr(3,19));
			eph.crc = to_double(line.substr(22,19));
			eph.omega = to_double(line.substr(41,19));
			eph.Omegadot = to_double(line.substr(60,19));
		getline(ifs,line);
			eph.idot = to_double(line.substr(3,19));
			eph.codes = to_double(line.substr(22,19));
			eph.weekno = to_double(line.substr(41,19));
			eph.L2flag = to_double(line.substr(60,19));
		getline(ifs,line);
			eph.svaccur = to_double(line.substr(3,19));
			eph.svhealth = to_double(line.substr(22,19));
			eph.tgd = to_double(line.substr(41,19));
			//iodc = line(60,19), null;
		getline(ifs,line);
			eph.tom = to_double(line.substr(3,19));
			eph.fit = to_double(line.substr(22,19));
		/*	spare = line(41,19), null;
			spare = line(60,19), null;*/

		_data.push_back(eph);
	}
}

/*!\brief Get a valid ephemeris for the PRN */
ephemeris ephemerides::get_ephemeris(int PRN, const gps_time& t)
{
	bool fail = true;

	vector<ephemeris>::iterator it;
	ephemeris best_e;
	double tmin = 4*86400.0; // number of seconds that the ephemeris should be younger than (4 days)
	for ( it=_data.begin() ; it < _data.end(); it++ )
	{
		ephemeris& eph = *it;
		if (eph.svprn == PRN)
		{
			if (abs(t.diff(eph.toc)) < tmin)	
			{
				tmin =  abs(t.diff(eph.toc));
				best_e = eph;
				fail = false;
			}
		}
	}
	// Check the health status of the received satellite!!!!!
	if (fail)
		throw string("No suitable ephemeris found");

//	cout << "SV " << PRN << " health = " << best_e.svhealth << " accuracy = " << best_e.svaccur << endl;

	return best_e;
}


void ephemerides::test(void)
{
	ephemerides e;
	e.parse_brdc("brdc2560.08n");

	// find an ephemeris for PRN 5, closest to the current time
	gps_time gt(2008,9,12,13, 45, 0.0);
	cout << " julday(2008,9,12,13.45) = " << gt << endl;

	ephemeris best_e = e.get_ephemeris(5, gt);

	vector3 sat_loc = best_e.get_location(gt.sec_of_week);
	cout << "PRN " << best_e.svprn << " (" << sat_loc << ")" << endl;
}

