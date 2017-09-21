/* Copyright (C) Tim Molteno 2008-2010. All rights reserved */

#include "vector3.h"
#include "geography.h"
#include "satellite.h"

#include "ephemerides.h"
#include "util.h"

#include "quick_fix.h"

	#define DEBUGGING 1
#include "nelder_meade.h"

using namespace std;




#include <iostream>
using namespace std;

int main(int argc,char *argv[])
{
	try
	{
	  ephemerides e;
	  
	  // create an empty vector of strings
	  vector<string> args;
	  // copy program arguments into vector
	  int i;
	  for (i=1;i<argc;i++) 
		  args.push_back(argv[i]);
	  
	  if (args.size() != 6)
	  {
			cerr << "Args.size = " << args.size() << endl;
			cerr << "Usage: ephemeris_tb <date> <data_dir> <ephemeris_file> <data_file> <output_file> <doppler_offset>" << endl;
			cerr << " where <ephemeris_file> is the name of the correct file with ephemeris data' " << endl;
		  return 1;
	  }

	  string date(args[0]);
	  string data_dir(args[1]);
	  string ephemeris_dir(args[2]);
	  string data_file(args[3]);
		string output_file(args[4]);
		string doppler_offset(args[5]);
		

	  // get 	the date yy_mm_dd_jjj_HH_MM_SS from the date string
	  // example 08_09_30_274_06_59_29

	  stringstream ss(date);
	  string yy_s = date.substr(0,2);
	  int yy = from_string<int>(yy_s);
	  int mm = from_string<int>(date.substr(3,2));
	  int dd = from_string<int>(date.substr(6,2));
	  string jjj_s = date.substr(9,3);	
	  int HH = from_string<int>(date.substr(13,2));
	  int MM = from_string<int>(date.substr(16,2));
	  int SS = from_string<int>(date.substr(19,2));

	  int jjj = from_string<int>(jjj_s);
	  cout << yy << "_" << mm << "_" << dd << "_" << jjj << "_" << HH << "_" << MM << "_" << SS << ", ";
	  // approximate starting time
    gps_time gt(2000+yy,mm,dd,HH,MM,SS);
    gps_time initial_gt(2000+yy,mm,dd,HH,MM,SS);

		string ephemeris_file=ephemeris_dir;
	  e.parse_brdc(ephemeris_file.c_str());
		
  #if QFDEBUG
	  cout << "opening file: " << data_file << endl;
  #endif

          
	  second_of_week sow = gt.sec_of_week;
	  fix known(angle(-45,51,50.13), angle(170,30,47.87), 42, sow, 0.0, 9e99); // Room 533. Physics Lab

	  qf._known = known;

  	  fix final_fix = qf.flag_outliers(sow);

	  // time search at a known location...
// 	  second_of_week time = qf.time_search(sow, known);
 	  cout << "time diff: " << final_fix.sow.diff(sow) << endl;

	  // Now get our best fix
//	  fix final_fix = qf.calculate_location(sow);
		
	  gt.sec_of_week = final_fix.sow;
		
	  vector3 r = known.location() - final_fix.location();
	  double ret = r.norm();
	  cout << gt << ", " << final_fix;
	  cout.flags(ios::right | ios::scientific); // Fixed point, right
	  cout.precision(4);
	  cout << ", " << final_fix.least_squares;
	  cout.flags(ios::right | ios::fixed); // Fixed point, right
	  cout << ", " << ret << endl;

  //	string outfile = data_dir + "/" + date + ".kml";
  #if QFDEBUG
	  cout << "opening file: " << output_file << endl;
  #endif

    /*
          "fix" : {
                  "lat" : [       -46.048704,     0.461187        ],
                  "lon" : [       170.666444,     1.484947        ],
                  "alt" : [       1583.663471,    1191.731256     ],
                  "sow" : [       0.104511,       3.908023        ],
                  "pha" : [       0.500000,       9999.000000     ],
                  "fs" : [        8183833.000000, 9999.000000     ],
                  "fc" : [        32164.768763,   32.737899       ]
          }
    */
          ofstream ofs(output_file.c_str(), ofstream::out);
          ofs.precision(6);
          ofs.setf(ios::fixed,ios::floatfield);
          ofs << "{\n\t\"fix\" : {\n";
          ofs << "\t\t\"lat\" : [\t" << final_fix.latitude.degrees() << ",\t" << 0.0 << "\t],\n";
          ofs << "\t\t\"lon\" : [\t" << final_fix.longitude.degrees() << ",\t" << 0.0 << "\t],\n";
          ofs << "\t\t\"alt\" : [\t" << final_fix.altitude << ",\t" << 0.0 << "\t],\n";
          ofs << "\t\t\"hdop\" : [\t" << log10(final_fix.least_squares)+10 << ",\t" << 0.0 << "\t],\n";
          ofs << "\t\t\"delta_t\" : [\t" << (initial_gt.sow() - gt.sow()) << ",\t" << 0.0 << "\t],\n";
          ofs << "\t\t\"sow\" : [\t" << gt.sow() << ",\t" << 0.0 << "\t],\n";
          ofs << "\t\t\"week\" : [\t" << gt.week() << ",\t" << 0.0 << "\t],\n";
          ofs << "\t\t\"pha\" : [\t" << -1 << ",\t" << 0.0 << "\t],\n";
          ofs << "\t\t\"fs\" : [\t" << -1 << ",\t" << 0.0 << "\t],\n";
          ofs << "\t\t\"fc\" : [\t" << (doppler_offset_val + final_fix.fc0_offset) << ",\t" << 0.0 << "\t]\n";
          ofs << "\t}\n}";
	  return 0;
	 }
	 catch (std::string mess)
	 {
	  cerr << "Error! " << mess << endl;
	 }
}
