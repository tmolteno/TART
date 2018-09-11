#ifndef __satellite__
#define __satellite__

#include "vector3.h"
#include "gps_time.h"
#include "ephemeris.h"

class satellite
{
public:

	satellite(int in_PRN, double in_signal, double inmeasured_doppler, double in_codephase)
		: _PRN(in_PRN), _signal_strength(in_signal), measured_doppler(inmeasured_doppler), measured_codephase(in_codephase), _eph(), enabled(true)
	{
		weight = 1.0;
	}

	double clock_correct(const second_of_week& sow)
	{
		return _eph.clock_correct(sow);
	}

	double code_delay()
	{
		static int code_delay_table[37] = {5,6,7,8,17,18,139,140,141,251,252,254,255,256,257,258,469,470,471,472,473,474,509,512,513,514,515,516,859,860,861,862,863,950,947,948,950};

		return double(code_delay_table[_PRN-1])/1023.0;
	}

	/*!\brief Get the doppler shift (in Hertz) for the signal from this satellite
	\param sow GPS time in seconds of the week
	\param receiver_location Receiver location (in rectangular co-ordinates)
	\return range in meters.
	 
	* Calculate two positions on second apart, and calculate the change in range
	* This is the relative velocity, v, between the satellite and the receiver (in m/s)
	* Then the doppler shift is given by -v / lambda (v is positive when moving away)
	*/
	double doppler(const second_of_week& sow, const vector3& receiver_location)
	{
		double range1 = range(sow-0.5, receiver_location);
		double range2 = range(sow+0.5, receiver_location);
		double velocity = (range1 - range2);
		return velocity / util::wavelength;
	}

	/*!\brief Get the range to this satellite from the receiver_location.
	\param sow GPS time in seconds of the week
	\param receiver_location Receiver location (in rectangular co-ordinates)
	\return range in meters.
	*/
	double range(const second_of_week& sow, const vector3& receiver_location)
	{
		second_of_week rx_time(sow);

		vector3 r = receiver_location - location(rx_time);
		double ret = r.norm();
#if 1
		double dt = ret / util::speed_of_light;
		second_of_week tx_time(rx_time - dt);

		// recalculate satellite location and range
		r = receiver_location - location(tx_time);
		ret = r.norm();
#endif
		return ret;
	}

	double codephase(const second_of_week& sow, const vector3& receiver_location)
	{
		static const double c1k = 1000.0 / util::speed_of_light;
		double ms = range(sow, receiver_location) * c1k;
		// codephase is started on the millisecond, and received at an arbitrary time.

		double uncorrected_codephase = ms - floor(ms);
//		double uncorrected_codephase = ceil(ms) - ms;
		
//		double corrected_codephase = uncorrected_codephase - 1000.0*clock_correct(sow) - code_delay();
		double corrected_codephase = uncorrected_codephase - 1000.0*clock_correct(sow);
		double ret = util::mod(corrected_codephase,1);
		return ret;
	}

	/*!\brief Get the satellite location
	\param sow GPS time in seconds of the week
	\return Receiver location (in rectangular co-ordinates)
	*/
	vector3 location(const second_of_week& sow)
	{
		return _eph.get_location(sow);
	}

	/*!\brief Get the satellite velocity
	\param sow GPS time in seconds of the week
	\return Receiver location (in rectangular co-ordinates)
	*/
	vector3 velocity(const second_of_week& sow)
	{
		return _eph.get_velocity(sow);
	}

	int _PRN;
	double _signal_strength;
	double measured_doppler;
	double measured_codephase;
	ephemeris _eph;
	bool enabled;
	double validation_score;
	double weight;
};

#endif /* __satellite__ */
