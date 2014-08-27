
#include "vector3.h"
#include "geography.h"
#include "satellite.h"

#include "ephemerides.h"
#include "util.h"


using namespace std;




#include <iostream>
using namespace std;

int main(int argc,char *argv[])
{
	try
	{
	  
	  // create an empty vector of strings
	  vector<string> args;
	  // copy program arguments into vector
	  int i;
	  for (i=1;i<argc;i++) 
		  args.push_back(argv[i]);
	  
	  if (args.size() != 3)
	  {
		  cerr << "Usage: satpos <svnum> <date> <data_dir>" << endl;
		  cerr << "     : <date> =  yy_mm_dd_jjj_HH_MM_SS.sss" << endl;
		  cerr << "     : <data_dir> =  location of broadcast ephemerides" << endl;
		  return 1;
	  }

	  string svnum_in(args[0]);
	  string date(args[1]);
	  string data_dir(args[2]);
	  

	  int svnum = from_string<int>(svnum_in);
	  // get 	the date yy_mm_dd_jjj_HH_MM_SS from the date string
	  // example 08_09_30_274_06_59_29

	  stringstream ss(date);
	  string yy_s = date.substr(0,2);		int yy = from_string<int>(yy_s);
	  int mm = from_string<int>(date.substr(3,2));
	  int dd = from_string<int>(date.substr(6,2));
	  string jjj_s = date.substr(9,3);	
	  int HH = from_string<int>(date.substr(13,2));
	  int MM = from_string<int>(date.substr(16,2));
	  int SS = from_string<int>(date.substr(19,2));
	  int sss = from_string<int>(date.substr(22,3));

	  int jjj = from_string<int>(jjj_s);
//	  cout << yy << "_" << mm << "_" << dd << "_" << jjj << "_" << HH << "_" << MM << "_" << SS << ", ";
	  gps_time gt(2000+yy,mm,dd,HH,MM,double(SS) + double(sss)*0.001);

	  // get the brdc file
	  stringstream fsstr;
	  fsstr << data_dir << "/brdc" << jjj_s << "0." << yy_s << "n";
	  string ephemeris_file=fsstr.str();

  #if 0
	  cout << "parsing brdc file " << ephemeris_file << endl;
  #endif
	  ephemerides e;
	  e.parse_brdc(ephemeris_file.c_str());

	  // now parse the file "<date>.dat"
	  
 
	  satellite s(svnum, 0.0, 0.0 ,0.0);
	  s._eph = e.get_ephemeris(s._PRN, gt);
	  
	  second_of_week sow = gt.sec_of_week;
	  
	  cout.precision(14);
	  cout << s.location(sow) << ", " << s.velocity(sow) << ", " << s.clock_correct(sow) << endl;

	  return 0;
	 }
	 catch (std::string mess)
	 {
	  cerr << "Error! " << mess << endl;
	 }
}
