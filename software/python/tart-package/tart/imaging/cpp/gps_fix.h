#ifndef __gps_fix__
#define __gps_fix__

#include "include.h"
#include "markov_chain.h"
#include "gps_time.h"
#include "prior.h"

using namespace Eigen;
//USING_PART_OF_NAMESPACE_EIGEN

class gps_fix
{
public:

// 	gps_fix(double	lat_mean, lat_sd,
// 			lon_mean, lon_sd,
// 			alt_mean, alt_sd,
// 			sow_mean, sow_sd,
// 			pha_mean, pha_sd,
// 			fs_mean, fs_sd,
// 			fc_mean, fc_sd	)
// 	:	_lat_m(lat_mean), _lat_sd(lat_sd), 
// 		_lon_m(lon_mean), _lon_sd(lon_sd),
// 		_alt_m(alt_mean), _alt_sd(alt_sd),
// 		_sow_m(sow_mean), _sow_sd(sow_sd)
// 		_pha_m(pha_mean), _pha_sd(pha_sd),
// 		_fs_m(fs_mean), _fs_sd(fs_sd),
// 		_fc_m(fc_mean), _fc_sd(fc_sd)
// 	{
// 	}
	
	void print_stats();
	
	string to_json(gps_time& t);
	
	void from_chain(markov_chain& mc);
	
	void from_json(string& json_string);
	
	void combine(gps_fix& other_fix);

	double get_stdev(int parameter)
	{
		return _stdevs(parameter);
	}
	
	double fc()
        {
          return _means(IND_FC);
        }
	
	parameter_vector get_stdevs()
	{
		return _stdevs;
	}
	
	parameter_vector get_means()
	{
		return _means;
	}
	
	void set_mean(int parameter, double mean)
	{
		_means[parameter] = mean;
	}
	
	phase_prior to_prior();
	
private:
	string parameter_name(int index);
	parameter_vector _means;
	parameter_vector _stdevs;
        gps_time _gps_t;
// 		double _lat_m, _lat_sd;
// 		double _lon_m, _lon_sd;
// 		double _alt_m, _alt_sd;
// 		double _sow_m, _sow_sd;
// 		double _pha_m, _pha_sd;
// 		double _fs_m, _fs_sd;
// 		double _fc_m, _fc_sd;
};

#endif
