#include "gps_fix.h"
#include <algorithm>
using namespace std;

inline double to_angle(vector<double>& _cos, vector<double>& _sin, int i)
{
  return util::to_deg(atan2(_sin[i],_cos[i]));
}

inline double to_unit(vector<double>& _cos, vector<double>& _sin, int i)
{
  return atan2(_sin[i],_cos[i])/util::pi;
}
#define MEDIAN 1
void gps_fix::from_chain(markov_chain& mc)
{
        int chain_length = mc.size();
// 	cout << "Chain length: " << chain_length << endl;
	
	_means.setZero();
	_stdevs.setZero();
	
	
#if MEDIAN
        vector<double> lat_sin(chain_length);
        vector<double> lat_cos(chain_length);
        vector<double> lon_sin(chain_length);
        vector<double> lon_cos(chain_length);
        vector<double> alt(chain_length);
        vector<double> sow(chain_length);
        vector<double> fc(chain_length);
        vector<double> fs(chain_length);
        vector<double> pha_cos(chain_length);
        vector<double> pha_sin(chain_length);
        for (int i = 0; i<chain_length; i++)
        {
                double lat_rad = util::to_rad(mc[i](IND_LAT));
                double lon_rad = util::to_rad(mc[i](IND_LON));
                lat_sin[i] = sin(lat_rad);
                lat_cos[i] = cos(lat_rad);
                lon_sin[i] = sin(lon_rad);
                lon_cos[i] = cos(lon_rad);
                alt[i] = mc[i](IND_ALT);
                sow[i] = mc[i](IND_SOW);
                fc[i] = mc[i](IND_FC);
                fs[i] = mc[i](IND_FS);
                double phase_angle = util::pi*mc[i](IND_PHA);
                pha_cos[i] = cos(phase_angle);
                pha_sin[i] = sin(phase_angle);
        }
        sort( pha_sin.begin(), pha_sin.end() );
        sort( pha_cos.begin(), pha_cos.end() );
        sort( lat_sin.begin(), lat_sin.end() );
        sort( lat_cos.begin(), lat_cos.end() );
        sort( lon_sin.begin(), lon_sin.end() );
        sort( lon_cos.begin(), lon_cos.end() );
        sort( alt.begin(), alt.end() );
        sort( sow.begin(), sow.end() );
        sort( fc.begin(), fc.end() );
        sort( fs.begin(), fs.end() );

        int median_index = chain_length/2;
        _means(IND_LAT) = to_angle(lat_cos, lat_sin, median_index);
        _means(IND_LON) = to_angle(lon_cos, lon_sin, median_index);
        _means(IND_ALT) = alt[median_index];
        _means(IND_SOW) = sow[median_index];
        _means(IND_FC) = fc[median_index];
        _means(IND_FS) = fs[median_index];
        _means(IND_PHA) = to_unit(pha_cos, pha_sin, median_index);
        
        int hi_index = int(chain_length*0.7);
        int lo_index = int(chain_length*0.3);
        
        _stdevs(IND_LAT) = util::delta_cyclic(to_angle(lat_cos, lat_sin, hi_index), to_angle(lat_cos, lat_sin, lo_index),180) / 2;
        _stdevs(IND_LON) = util::delta_cyclic(to_angle(lon_cos, lon_sin, hi_index), to_angle(lon_cos, lon_sin, lo_index),360) / 2;
        _stdevs(IND_ALT) = (alt[hi_index] - alt[lo_index]) / 2;
        _stdevs(IND_SOW) = (sow[hi_index] - sow[lo_index]) / 2;
        _stdevs(IND_FC) = (fc[hi_index] - fc[lo_index]) / 2;
        _stdevs(IND_FS) = (fs[hi_index] - fs[lo_index]) / 2;
        _stdevs(IND_PHA) = util::delta_cyclic(to_unit(pha_cos, pha_sin, hi_index),to_unit(pha_cos, pha_sin, lo_index),1);
#else        
        double x_lat(0),y_lat(0);
        double x_lon(0),y_lon(0);
        double x_pha(0),y_pha(0);
	for (int i = 0; i<chain_length; i++)
	{
		x_lat += cos( util::to_rad(mc[i](IND_LAT)) );
		y_lat += sin( util::to_rad(mc[i](IND_LAT)) );
		x_lon += cos( util::to_rad(mc[i](IND_LON)) );
		y_lon += sin( util::to_rad(mc[i](IND_LON)) );
		_means(IND_ALT) += mc[i](IND_ALT);
		_means(IND_SOW) += mc[i](IND_SOW);
                _means(IND_FS) += mc[i](IND_FS);
                _means(IND_FC) += mc[i](IND_FC);
                x_pha += cos( util::pi*mc[i](IND_PHA) );
		y_pha += sin( util::pi*mc[i](IND_PHA) );
	}
        _means(IND_LAT) = util::to_deg(atan2(y_lat,x_lat));
        _means(IND_LON) = util::to_deg(atan2(y_lon,x_lon));
        _means(IND_ALT) /= chain_length;
        _means(IND_SOW) /= chain_length;
        _means(IND_FC) /= chain_length;
        _means(IND_FS) /= chain_length;
        _means(IND_PHA) = atan2(y_pha,x_pha)/util::pi;

        for (int i = 0; i<chain_length; i++)
	{
		_stdevs(IND_LAT) += pow( util::delta_cyclic(_means(IND_LAT),mc[i](IND_LAT),180), 2 );
		_stdevs(IND_LON) += pow( util::delta_cyclic(_means(IND_LON),mc[i](IND_LON),360), 2 );
		_stdevs(IND_ALT) += pow( _means(IND_ALT)-mc[i](IND_ALT), 2 );
		_stdevs(IND_SOW) += pow( _means(IND_SOW)-mc[i](IND_SOW), 2 );
                _stdevs(IND_FC) += pow( _means(IND_FC)-mc[i](IND_FC), 2 );
                _stdevs(IND_FS) += pow( _means(IND_FS)-mc[i](IND_FS), 2 );
                _stdevs(IND_PHA) += pow( util::delta_cyclic(_means(IND_PHA),mc[i](IND_PHA),1), 2 );
	}
	_stdevs /= chain_length;
	_stdevs = _stdevs.array().sqrt();
#endif
	
        /*! For those parameters not varied, standard deviation is evaluated 
		as zero, where it should be infinite.
	*/
	for (int i=0; i<NUM_PARAM; i++)
		if ( _stdevs(i)<1e-7 ) _stdevs(i) = 9999;

}

void gps_fix::combine(gps_fix& other_fix)
{
	parameter_vector other_means = other_fix.get_means();
	parameter_vector other_stdevs = other_fix.get_stdevs();
	for (int i=0; i<NUM_PARAM; i++)
	{
		if (other_stdevs[i] < _stdevs[i])
		{
			_means[i] = other_means[i];
			_stdevs[i] = other_stdevs[i];
		}
	}
}

string gps_fix::parameter_name(int index)
{
	static string parameter_names[] = {"lat","lon","alt","sow","pha","fs","fc"};
	return parameter_names[index];
}


void gps_fix::print_stats()
{
	cout.precision(8);
	
	cout << "\nFix statistics:\t\tParameter\tMean\t\tStd Dev\n" << endl;
	for (int loop=0; loop<NUM_PARAM; loop++)
		cout << "\t\t\t" 	<< parameter_name(loop) << "\t\t" 
					<< _means(loop) << "\t\t"
					<< _stdevs(loop) << endl;
	cout << endl;
	cout.precision(10);
	cout << "\nView fix at:   http://maps.google.com/?q=" << _means(IND_LAT) << "," << _means(IND_LON) << endl;
	cout << endl;
	// 	cout << "\nFix statistics:\tParameter\tMean\t\tStd Dev\n" << endl;
// 	cout << "\t\t\tlat\t\t"	<< _lat_m << "\t\t"
// 				<< _lat_sd << endl;
// 	cout << "\t\t\tlon\t\t"	<< _lon_m << "\t\t"
// 				<< _lon_sd << endl;
// 	cout << "\t\t\talt\t\t"	<< _alt_m << "\t\t"
// 				<< _alt_sd << endl;
// 	cout << "\t\t\tsow\t\t"	<< _sow_m << "\t\t"
// 				<< _sow_sd << endl;
// 	cout << "\t\t\tpha\t\t"	<< _pha_m << "\t\t"
// 				<< _pha_sd << endl;
// 	cout << "\t\t\tfs\t\t" 	<< _fs_m << "\t\t"
// 				<< _fs_sd << endl;
// 	cout << "\t\t\tfc\t\t" 	<< _fc_m << "\t\t"
// 				<< _fc_sd << endl;
}

string gps_fix::to_json(gps_time& t)
{
	sum_stats fix_stats;
	fix_stats.col(0) = _means;
	fix_stats.col(1) = _stdevs;
	
	
        double sow = fix_stats(IND_SOW,0);
        long week = t.week();
	
	stringstream ss;
	ss.precision(6);
	ss.setf(ios::fixed,ios::floatfield);
	ss << "{\n\t\"fix\" : {\n";
	// "lat","lon","alt","sow","pha","fs","fc"};
	ss << "\t\t\"" << "lat" << "\" : [\t" << fix_stats(IND_LAT,0) << ",\t" << fix_stats(IND_LAT,1) << "\t],\n";
	ss << "\t\t\"" << "lon" << "\" : [\t" << fix_stats(IND_LON,0) << ",\t" << fix_stats(IND_LON,1) << "\t],\n";
	ss << "\t\t\"" << "alt" << "\" : [\t" << fix_stats(IND_ALT,0) << ",\t" << fix_stats(IND_ALT,1) << "\t],\n";
	ss << "\t\t\"" << "week" << "\" : [\t" << week << ",\t" << 0.0 << "\t],\n";
        ss << "\t\t\"" << "sow" << "\" : [\t" << sow << ",\t" << fix_stats(IND_SOW,1) << "\t],\n";
        ss << "\t\t\"" << "delta_t" << "\" : [\t" << (t.sow() - sow) << ",\t" << fix_stats(IND_SOW,1) << "\t],\n";
	ss << "\t\t\"" << "pha" << "\" : [\t" << fix_stats(IND_PHA,0) << ",\t" << fix_stats(IND_PHA,1) << "\t],\n";
	ss << "\t\t\"" << "fs" << "\" : [\t" << fix_stats(IND_FS,0) << ",\t" << fix_stats(IND_FS,1) << "\t],\n";
	ss << "\t\t\"" << "fc" << "\" : [\t" << fix_stats(IND_FC,0) << ",\t" << fix_stats(IND_FC,1) << "\t]\n";

// 	for (int i=0; i<NUM_PARAM; i++)
// 	{
// 		ss << "\t\t\"" << parameter_name(i) << "\" : [\t";
// 		ss << fix_stats(i,0) << ",\t" << fix_stats(i,1) << "\t]";
// 		if (i<(NUM_PARAM-1)) ss << ",\n";
// 		else ss << "\n";
// 	}
	ss << "\t}\n}";
	
	return ss.str();
}


/*! Parse a json string, filling the _means and _stdevs vectors of this gps_fix 
*/

#include "picojson.h"
using namespace picojson;

void gps_fix::from_json(string& json_string)
{
	istringstream is(json_string); // Convert string to input stringstream ready to give to new 'value'
	picojson::value v;
	is >> v;
	
	// Check json_string contains a json object
	if (!v.is<picojson::object>()) 
		throw string("File doesn't contain a valid json object!");


	const picojson::object& top_object = v.get<picojson::object>();

	// Check json_string contains a fix object
	if (top_object.count("fix")==0) 
		throw string("Json file doesn't contain a fix object!");

	picojson::value params;
	params = top_object.find("fix")->second;
	
	const picojson::object& params_object = params.get<picojson::object>();

	for (picojson::object::const_iterator i = params_object.begin(); i != params_object.end(); ++i) 
	{
		string name = i->first;
		
// 		std::cout << name << "  " << i->second << std::endl;
		
		const picojson::array& a = i->second.get<picojson::array>();
		
		if (!i->second.is<picojson::array>())
		{
			stringstream ss;
			ss << "Parameter '" << name << "' doesn't have associated array;" << endl;
			throw ss.str();
		}
			
		if (name=="lat") 
		{
			_means(IND_LAT) = a[0].get<double>();
			_stdevs(IND_LAT) = a[1].get<double>();
		}
		if (name=="lon") 
		{
			_means(IND_LON) = a[0].get<double>();
			_stdevs(IND_LON) = a[1].get<double>();
		}
		if (name=="alt") 
		{
			_means(IND_ALT) = a[0].get<double>();
			_stdevs(IND_ALT) = a[1].get<double>();
		}
		if (name=="sow") 
		{
			_means(IND_SOW) = a[0].get<double>();
			_stdevs(IND_SOW) = a[1].get<double>();
		}
		if (name=="pha") 
		{
			_means(IND_PHA) = a[0].get<double>();
			_stdevs(IND_PHA) = a[1].get<double>();
		}
		if (name=="fs") 
		{
			_means(IND_FS) = a[0].get<double>();
			_stdevs(IND_FS) = a[1].get<double>();
		}
		if (name=="fc") 
		{
			_means(IND_FC) = a[0].get<double>();
			_stdevs(IND_FC) = a[1].get<double>();
		}
		 
	}	
// 	if (v.is<picojson::null>()) 
// 		cout << "input is null" << endl;
// 	
// 	else if (v.is<bool>())
// 		cout << "input is " << (v.get<bool>() ? "true" : "false") << endl;
// 	
// 	else if (v.is<double>()) 
// 		std::cout << "input is " << v.get<double>() << endl;
// 	
// 	else if (v.is<std::string>())
// 		std::cout << "input is string: " << v.get<std::string>() << endl;
// 	
// 	else if (v.is<picojson::array>())
// 	{
// 		cout << "input is an array" << endl;
// 		const picojson::array& a = v.get<picojson::array>();
// 		
// 		for (picojson::array::const_iterator i = a.begin(); i != a.end(); ++i)
// 			cout << "  " << *i << endl;
// 	}
}

phase_prior gps_fix::to_prior()
{
	phase_prior ret(_means(IND_LAT), _stdevs(IND_LAT),
			_means(IND_LON), _stdevs(IND_LON),
			_means(IND_ALT), _stdevs(IND_ALT),
			_means(IND_SOW), _stdevs(IND_SOW),
			_means(IND_PHA), _stdevs(IND_PHA),
			_means(IND_FS), _stdevs(IND_FS),
			_means(IND_FC), _stdevs(IND_FC) );
	return ret;
}
