# Copyright (C) Tim Molteno 2008-2010. All rights reserved
#
# Requires Eigen aptitude install libeigen2-dev
# Requires json-c aptitude install libjson0-dev

all:	ephemeris_tb

.SUFFIXES:
.SUFFIXES: .cpp .o .dat .kml .bin

HDRS = ephemeris.h gps_time.h angle.h geography.h julian_date.h geography.h satellite.h second_of_week.h
OBJS = gps_time.o second_of_week.o julian_date.o ephemerides.o vector3.o ephemeris.o util.o
LDFLAGS = -static-libgcc -static  -lstdc++ -lm
CXXFLAGS = -O3 -g -Wall -I /usr/include/eigen3/

# -std=c++0x
################# DEPENDENCY INFORMATION GENERATION ####################################
# compile and generate dependency info
%.o: %.cpp
	${CXX} ${CXXFLAGS} -c -o $*.o $*.cpp 
	${CXX} -MM ${CXXFLAGS} $*.cpp  > $*.d

# pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)
#########################################################################################

ephemeris_tb:	${OBJS} ${HDRS} ephemeris_tb.o
	g++ -o ephemeris_tb ${OBJS} ephemeris_tb.o


julian_date_tb: julian_date.h julian_date.o julian_date_tb.o gps_time.o  second_of_week.o
	g++ -o julian_date_tb julian_date.o julian_date_tb.o gps_time.o   second_of_week.o
	
test_date: julian_date_tb
	./julian_date_tb


clean:
	rm -f *.o *.d

YY=08
YEAR=20${YY}
DOY=276


FILENAME="brdc${DOY}0.${YY}n"
brdc:
#	wget "ftp://cddis.gsfc.nasa.gov/gps/data/daily/${YEAR}/${DOY}/${YY}n/${FILENAME}.Z"
	curl -o ${FILENAME}.Z --proxy http://adslorcon.px.otago.ac.nz:3128  "ftp://cddis.gsfc.nasa.gov/gps/data/daily/${YEAR}/${DOY}/${YY}n/${FILENAME}.Z"
	uncompress ${FILENAME}.Z
	mv ${FILENAME} ../data
